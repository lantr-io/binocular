package binocular

import binocular.util.SlotConfigHelper
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.{BackendService, DefaultUtxoSupplier}
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.{PlutusV3Script, Redeemer}
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import scalus.bloxbean.Interop.toPlutusData
import scalus.builtin.Data
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Get compiled PlutusV3 script */
    def getCompiledScript(): PlutusV3Script = {
        val program = BitcoinContract.bitcoinProgram
        val scriptCborHex = program.doubleCborHex

        PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(scriptCborHex)
            .build()
            .asInstanceOf[PlutusV3Script]
    }

    /** Compute the validity interval start time that will be used on-chain. This matches the
      * computation in buildAndSubmitUpdateTransaction.
      *
      * The on-chain validator reads time from tx.validRange.from, which is set using
      * .validFrom(slot). The slot is computed from System.currentTimeMillis(). To ensure offline
      * computation matches on-chain validation, we must use the same time derivation.
      *
      * @param backendService
      *   Backend service to retrieve slot configuration
      * @return
      *   The validity interval start time in seconds (POSIX time)
      */
    def computeValidityIntervalTime(backendService: BackendService): BigInt = {
        val slotConfig = SlotConfigHelper.retrieveSlotConfig(backendService)
        val currentPosixTimeMs = System.currentTimeMillis()
        val currentSlot = slotConfig.timeToSlot(currentPosixTimeMs)
        // Manually compute the slot start time: zeroTime + (slot - zeroSlot) * slotLength
        val intervalStartMs =
            slotConfig.zeroTime + (currentSlot - slotConfig.zeroSlot) * slotConfig.slotLength
        val intervalStartSeconds = BigInt(intervalStartMs / 1000)

        println(s"[DEBUG computeValidityIntervalTime]")
        println(s"  currentPosixTimeMs: $currentPosixTimeMs")
        println(s"  currentSlot: $currentSlot")
        println(s"  intervalStartMs: $intervalStartMs")
        println(s"  intervalStartSeconds: $intervalStartSeconds")

        intervalStartSeconds
    }

    /** Apply Bitcoin headers to ChainState to calculate new state */
    def applyHeaders(
        currentState: ChainState,
        headers: scalus.prelude.List[BlockHeader],
        currentTime: BigInt
    ): ChainState = {
        headers.foldLeft(currentState) { (state, header) =>
            BitcoinValidator.updateTip(state, header, currentTime)
        }
    }

    /** Create UpdateOracle redeemer */
    def createUpdateOracleRedeemer(
        blockHeaders: scalus.prelude.List[BlockHeader],
        currentTime: BigInt
    ): Redeemer = {
        val action = Action.UpdateOracle(blockHeaders, currentTime)
        val actionData = action.toData
        val redeemerData = toPlutusData(actionData)

        Redeemer
            .builder()
            .data(redeemerData)
            .build()
    }

    /** Build and submit initialization transaction
      *
      * Creates a new oracle UTxO with initial ChainState datum.
      *
      * @param account
      *   Account for signing the transaction
      * @param backendService
      *   Backend service for querying and submitting
      * @param scriptAddress
      *   Oracle script address
      * @param initialState
      *   Initial ChainState datum
      * @param lovelaceAmount
      *   Amount of ADA to lock (default: 5 ADA)
      * @return
      *   Either error message or transaction hash
      */
    def buildAndSubmitInitTransaction(
        account: Account,
        backendService: BackendService,
        scriptAddress: Address,
        initialState: ChainState,
        lovelaceAmount: Long = 5000000L // 5 ADA in lovelace
    ): Either[String, String] = {
        Try {
            // Convert ChainState to PlutusData
            val chainStateData = initialState.toData
            val plutusDatum = toPlutusData(chainStateData)

            // Get script
            val script = getCompiledScript()

            // Build transaction using QuickTxBuilder
            val quickTxBuilder = new QuickTxBuilder(backendService)

            val amount = Amount.lovelace(java.math.BigInteger.valueOf(lovelaceAmount))

            val tx = new Tx()
                .payToContract(scriptAddress.getAddress, amount, plutusDatum)
                .from(account.baseAddress())

            val result = quickTxBuilder
                .compose(tx)
                .withSigner(SignerProviders.signerFrom(account))
                .completeAndWait()

            if result.isSuccessful then {
                Right(result.getValue)
            } else {
                Left(s"Transaction failed: ${result.getResponse}")
            }
        } match {
            case Success(result) => result
            case Failure(ex)     => Left(s"Error building transaction: ${ex.getMessage}")
        }
    }

    /** Build and submit UpdateOracle transaction
      *
      * Updates the oracle with new Bitcoin block headers.
      *
      * @param account
      *   Account for signing the transaction
      * @param backendService
      *   Backend service for querying and submitting
      * @param scriptAddress
      *   Oracle script address
      * @param oracleTxHash
      *   Transaction hash of the current oracle UTxO
      * @param oracleOutputIndex
      *   Output index of the current oracle UTxO
      * @param currentChainState
      *   Current ChainState datum
      * @param newChainState
      *   New ChainState datum (pre-computed using computeUpdateOracleState with validityIntervalTimeSeconds)
      * @param blockHeaders
      *   Bitcoin block headers to submit
      * @param validityIntervalTimeSeconds
      *   The time (in seconds) that was used to compute newChainState. Must be obtained from computeValidityIntervalTime()
      * @return
      *   Either error message or transaction hash
      */
    def buildAndSubmitUpdateTransaction(
        account: Account,
        backendService: BackendService,
        scriptAddress: Address,
        oracleTxHash: String,
        oracleOutputIndex: Int,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: scalus.prelude.List[BlockHeader],
        validityIntervalTimeSeconds: BigInt
    ): Either[String, String] = {
        Try {
            // Get the script
            val script = getCompiledScript()

            // Fetch the specific UTxO
            val utxoService = backendService.getUtxoService
            val utxos = utxoService.getUtxos(scriptAddress.getAddress, 100, 1)

            if !utxos.isSuccessful then {
                throw new RuntimeException(s"Failed to fetch UTxOs: ${utxos.getResponse}")
            }

            val allUtxos = utxos.getValue.asScala.toList
            val targetUtxo = allUtxos
                .find(u => u.getTxHash == oracleTxHash && u.getOutputIndex == oracleOutputIndex)
                .getOrElse {
                    throw new RuntimeException(s"UTxO not found: $oracleTxHash:$oracleOutputIndex")
                }
            
            // CRITICAL: Verify that the input UTXO's datum matches currentChainState
            // This ensures we're computing the new state from the correct starting point
            val inputDatum = targetUtxo.getInlineDatum
            if (inputDatum == null || inputDatum.isEmpty) {
                throw new RuntimeException(s"Input UTxO has no inline datum: $oracleTxHash:$oracleOutputIndex")
            }
            val inputState = Data.fromCbor(scalus.utils.Hex.hexToBytes(inputDatum)).to[ChainState]
            
            if (inputState.blockHeight != currentChainState.blockHeight ||
                inputState.blockHash != currentChainState.blockHash) {
                throw new RuntimeException(
                    s"Input UTxO state does not match provided currentChainState!\n" +
                    s"  This means currentChainState is stale or wrong UTXO was selected.\n" +
                    s"  Provided currentChainState: height=${currentChainState.blockHeight}, hash=${currentChainState.blockHash.toHex}\n" +
                    s"  Input UTxO state: height=${inputState.blockHeight}, hash=${inputState.blockHash.toHex}\n" +
                    s"  Input UTxO: $oracleTxHash:$oracleOutputIndex"
                )
            }
            
            println(s"[DEBUG] Verified input UTXO datum matches currentChainState")

            // Retrieve slot config that will be used for all time conversions
            val slotConfig = SlotConfigHelper.retrieveSlotConfig(backendService)
            println(
              s"[DEBUG] SlotConfig: zeroTime=${slotConfig.zeroTime}, zeroSlot=${slotConfig.zeroSlot}, slotLength=${slotConfig.slotLength}"
            )

            // Get the current blockchain slot that will be used for validFrom
            val currentBlockchainSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot
            
            // Compute what time the validator will see based on validFrom slot
            // This is: slotToTime(validFrom) which equals zeroTime + (slot - zeroSlot) * slotLength
            val intervalMs =
                slotConfig.zeroTime + (currentBlockchainSlot - slotConfig.zeroSlot) * slotConfig.slotLength
            val validatorWillSeeTime = BigInt(intervalMs / 1000)

            println(s"[DEBUG] Computing time that validator will see:")
            println(s"  currentBlockchainSlot (for validFrom): $currentBlockchainSlot")
            println(s"  intervalMs: $intervalMs")
            println(s"  validatorWillSeeTime (seconds): $validatorWillSeeTime")

            // The redeemer time must be the SAME time used to compute the provided newChainState
            // The validator will compute state using redeemerTime and compare with the datum
            // So we must use validityIntervalTimeSeconds in the redeemer to ensure consistency
            
            // Verify that provided time is within tolerance of what validator will see from tx.validRange
            // This ensures the on-chain time tolerance check passes
            val TimeToleranceSeconds = 5 // Must match validator's tolerance
            val timeDiff = if validityIntervalTimeSeconds > validatorWillSeeTime then 
                validityIntervalTimeSeconds - validatorWillSeeTime 
            else 
                validatorWillSeeTime - validityIntervalTimeSeconds
            
            println(s"[DEBUG] Provided time: $validityIntervalTimeSeconds, Validator will see from tx.validRange: $validatorWillSeeTime")
            println(s"  Time difference: $timeDiff seconds")
            
            if (timeDiff > TimeToleranceSeconds) {
                throw new RuntimeException(
                    s"Time mismatch: provided time ($validityIntervalTimeSeconds) differs from tx.validRange time ($validatorWillSeeTime) by $timeDiff seconds (tolerance: $TimeToleranceSeconds s). " +
                    s"This will cause on-chain validation failure. " +
                    s"The on-chain time tolerance check will fail."
                )
            }
            println(s"  Time difference within tolerance - using provided time for redeemer")
            
            val redeemerTime = validityIntervalTimeSeconds // Use the time that was used to compute the state

            // Create UpdateOracle redeemer with the time that was used to compute the state
            val redeemer = createUpdateOracleRedeemer(blockHeaders, redeemerTime)

            // Convert new ChainState to PlutusData for output
            val newStateData = newChainState.toData
            val newDatum = toPlutusData(newStateData)
            
            println(s"[DEBUG] New state forksTree size: ${newChainState.forksTree.toList.size}")

            // Calculate amount (same as input)
            val lovelaceAmount = targetUtxo.getAmount.asScala.head.getQuantity
            val amount = Amount.lovelace(lovelaceAmount)

            // Build transaction using ScriptTx
            // Configure ScalusEvaluator for this transaction to use PlutusVM instead of default Rust evaluator
            val protocolParams = backendService.getEpochService.getProtocolParameters.getValue

            // Wrap UtxoService as UtxoSupplier
            val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)

            val scalusEvaluator =
                new scalus.bloxbean.ScalusTransactionEvaluator(
                  slotConfig,
                  protocolParams,
                  utxoSupplier
                )
            println("[DEBUG] Using ScalusEvaluator (PlutusVM) for script cost evaluation")

            val quickTxBuilder = new QuickTxBuilder(backendService)

            val scriptTx = new ScriptTx()
                .collectFrom(targetUtxo, redeemer.getData)
                .payToContract(scriptAddress.getAddress, amount, newDatum)
                .attachSpendingValidator(script)

            // Build, sign, and submit using the current blockchain slot for validFrom
            // This ensures tx.validRange.from corresponds to the time we used in the redeemer
            println(s"[DEBUG] Using validFrom slot: $currentBlockchainSlot")

            val result = quickTxBuilder
                .compose(scriptTx)
                .feePayer(account.baseAddress())
                .withSigner(SignerProviders.signerFrom(account))
                .withTxEvaluator(scalusEvaluator)
                .validFrom(currentBlockchainSlot)
                .completeAndWait()

            if result.isSuccessful then {
                Right(result.getValue)
            } else {
                Left(s"Transaction failed: ${result.getResponse}")
            }
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building UpdateOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}"
                )
        }
    }
}
