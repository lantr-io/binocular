package binocular

import binocular.util.SlotConfigHelper
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.{PlutusData, PlutusV3Script, Redeemer}
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import com.bloxbean.cardano.client.util.HexUtil
import scalus.builtin.{ByteString, Data, FromData, ToData}

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Get compiled PlutusV3 script */
    def getCompiledScript(): PlutusV3Script = {
        val program = BitcoinContract.bitcoinProgram
        val scriptCborHex = program.doubleCborHex

        PlutusV3Script.builder()
          .`type`("PlutusScriptV3")
          .cborHex(scriptCborHex)
          .build()
          .asInstanceOf[PlutusV3Script]
    }

    /** Convert Scalus Data to CCL PlutusData */
    def scalusDataToPlutusData(data: Data): com.bloxbean.cardano.client.plutus.spec.PlutusData = {
        val cborBytes = data.toCbor
        val cborHex = HexUtil.encodeHexString(cborBytes)
        com.bloxbean.cardano.client.plutus.spec.PlutusData.deserialize(HexUtil.decodeHexString(cborHex))
    }

    /** Convert CCL PlutusData to Scalus Data */
    def plutusDataToScalusData(plutusData: PlutusData): Data = {
        val hexString = plutusData.serializeToHex
        Data.fromCbor(ByteString.fromHex(hexString))
    }

    /**
     * Compute the validity interval start time that will be used on-chain.
     * This matches the computation in buildAndSubmitUpdateTransaction.
     *
     * The on-chain validator reads time from tx.validRange.from, which is set
     * using .validFrom(slot). The slot is computed from System.currentTimeMillis().
     * To ensure offline computation matches on-chain validation, we must use
     * the same time derivation.
     *
     * @param backendService Backend service to retrieve slot configuration
     * @return The validity interval start time in seconds (POSIX time)
     */
    def computeValidityIntervalTime(backendService: BackendService): BigInt = {
        val slotConfig = SlotConfigHelper.retrieveSlotConfig(backendService)
        val currentPosixTimeMs = System.currentTimeMillis()
        val currentSlot = slotConfig.timeToSlot(currentPosixTimeMs)
        // Manually compute the slot start time: zeroTime + (slot - zeroSlot) * slotLength
        val intervalStartMs = slotConfig.zeroTime + (currentSlot - slotConfig.zeroSlot) * slotConfig.slotLength
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
        currentState: BitcoinValidator.ChainState,
        headers: scalus.prelude.List[BitcoinValidator.BlockHeader],
        currentTime: BigInt
    ): BitcoinValidator.ChainState = {
        headers.foldLeft(currentState) { (state, header) =>
            BitcoinValidator.updateTip(state, header, currentTime)
        }
    }

    /** Create UpdateOracle redeemer */
    def createUpdateOracleRedeemer(
        blockHeaders: scalus.prelude.List[BitcoinValidator.BlockHeader],
        currentTime: BigInt
    ): Redeemer = {
        val action = BitcoinValidator.Action.UpdateOracle(blockHeaders, currentTime)
        val actionData = ToData.toData(action)(using BitcoinValidator.Action.derived$ToData)
        val redeemerData = scalusDataToPlutusData(actionData)

        Redeemer.builder()
          .data(redeemerData)
          .build()
    }

    /** Build and submit initialization transaction
      *
      * Creates a new oracle UTxO with initial ChainState datum.
      *
      * @param account Account for signing the transaction
      * @param backendService Backend service for querying and submitting
      * @param scriptAddress Oracle script address
      * @param initialState Initial ChainState datum
      * @param lovelaceAmount Amount of ADA to lock (default: 5 ADA)
      * @return Either error message or transaction hash
      */
    def buildAndSubmitInitTransaction(
        account: Account,
        backendService: BackendService,
        scriptAddress: Address,
        initialState: BitcoinValidator.ChainState,
        lovelaceAmount: Long = 5000000L // 5 ADA in lovelace
    ): Either[String, String] = {
        Try {
            // Convert ChainState to PlutusData
            val chainStateData = ToData.toData(initialState)(using BitcoinValidator.ChainState.derived$ToData)
            val plutusDatum = scalusDataToPlutusData(chainStateData)

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
              .withSigner(com.bloxbean.cardano.client.function.helper.SignerProviders.signerFrom(account))
              .completeAndWait()

            if (result.isSuccessful) {
                Right(result.getValue)
            } else {
                Left(s"Transaction failed: ${result.getResponse}")
            }
        } match {
            case Success(result) => result
            case Failure(ex) => Left(s"Error building transaction: ${ex.getMessage}")
        }
    }

    /** Build and submit UpdateOracle transaction
      *
      * Updates the oracle with new Bitcoin block headers.
      *
      * @param account Account for signing the transaction
      * @param backendService Backend service for querying and submitting
      * @param scriptAddress Oracle script address
      * @param oracleTxHash Transaction hash of the current oracle UTxO
      * @param oracleOutputIndex Output index of the current oracle UTxO
      * @param currentChainState Current ChainState datum
      * @param newChainState New ChainState datum (pre-computed using computeUpdateOracleState)
      * @param blockHeaders Bitcoin block headers to submit
      * @param validityIntervalTimeSeconds Optional validity interval time. If not provided, current time is used.
      * @return Either error message or transaction hash
      */
    def buildAndSubmitUpdateTransaction(
        account: Account,
        backendService: BackendService,
        scriptAddress: Address,
        oracleTxHash: String,
        oracleOutputIndex: Int,
        currentChainState: BitcoinValidator.ChainState,
        newChainState: BitcoinValidator.ChainState,
        blockHeaders: scalus.prelude.List[BitcoinValidator.BlockHeader],
        validityIntervalTimeSeconds: Option[BigInt] = None
    ): Either[String, String] = {
        Try {
            // Get the script
            val script = getCompiledScript()

            // Fetch the specific UTxO
            val utxoService = backendService.getUtxoService
            val utxos = utxoService.getUtxos(scriptAddress.getAddress, 100, 1)

            if (!utxos.isSuccessful) {
                throw new RuntimeException(s"Failed to fetch UTxOs: ${utxos.getResponse}")
            }

            val allUtxos = utxos.getValue.asScala.toList
            val targetUtxo = allUtxos.find(u => u.getTxHash == oracleTxHash && u.getOutputIndex == oracleOutputIndex)
              .getOrElse {
                  throw new RuntimeException(s"UTxO not found: $oracleTxHash:$oracleOutputIndex")
              }

            // Get the time that will be seen by the validator
            // Use the current blockchain slot to compute the time
            val currentBlockchainSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot
            val slotConfigEarly = SlotConfigHelper.retrieveSlotConfig(backendService)
            val intervalMs = slotConfigEarly.zeroTime + (currentBlockchainSlot - slotConfigEarly.zeroSlot) * slotConfigEarly.slotLength
            val computationTime = BigInt(intervalMs / 1000)
            
            println(s"[DEBUG] Computing redeemer time from blockchain slot:")
            println(s"  currentBlockchainSlot: $currentBlockchainSlot")
            println(s"  intervalMs: $intervalMs")
            println(s"  computationTime (seconds): $computationTime")

            // Create UpdateOracle redeemer with the time used for state computation
            val redeemer = createUpdateOracleRedeemer(blockHeaders, computationTime)

            // Convert new ChainState to PlutusData for output
            val newStateData = ToData.toData(newChainState)(using BitcoinValidator.ChainState.derived$ToData)
            val newDatum = scalusDataToPlutusData(newStateData)

            // Calculate amount (same as input)
            val lovelaceAmount = targetUtxo.getAmount.asScala.head.getQuantity
            val amount = Amount.lovelace(lovelaceAmount)

            // Build transaction using ScriptTx
            // Configure ScalusEvaluator for this transaction to use PlutusVM instead of default Rust evaluator
            val protocolParams = backendService.getEpochService.getProtocolParameters.getValue

            // Wrap UtxoService as UtxoSupplier
            val utxoSupplier = new com.bloxbean.cardano.client.api.UtxoSupplier {
                override def getPage(address: String, nrOfItems: Integer, page: Integer, order: com.bloxbean.cardano.client.api.common.OrderEnum): java.util.List[com.bloxbean.cardano.client.api.model.Utxo] = {
                    utxoService.getUtxos(address, nrOfItems, page, order).getValue
                }

                override def getTxOutput(txHash: String, outputIndex: Int): java.util.Optional[com.bloxbean.cardano.client.api.model.Utxo] = {
                    val result = utxoService.getTxOutput(txHash, outputIndex)
                    if (result.isSuccessful && result.getValue != null) {
                        java.util.Optional.of(result.getValue)
                    } else {
                        java.util.Optional.empty()
                    }
                }
            }

            val slotConfig = SlotConfigHelper.retrieveSlotConfig(backendService)
            println(s"[DEBUG] SlotConfig: zeroTime=${slotConfig.zeroTime}, zeroSlot=${slotConfig.zeroSlot}, slotLength=${slotConfig.slotLength}")

            val scalusEvaluator = new scalus.bloxbean.ScalusTransactionEvaluator(slotConfig, protocolParams, utxoSupplier)
            println("[DEBUG] Using ScalusEvaluator (PlutusVM) for script cost evaluation")

            val quickTxBuilder = new QuickTxBuilder(backendService)

            val scriptTx = new ScriptTx()
              .collectFrom(targetUtxo, redeemer.getData)
              .payToContract(scriptAddress.getAddress, amount, newDatum)
              .attachSpendingValidator(script)

            // Get current time and convert to slot for validity interval
            // The validator requires a finite validity start interval to access tx.validRange
            // If validityIntervalTimeSeconds is provided, use it to compute the slot
            // Otherwise, use current time
            val currentPosixTimeMs = validityIntervalTimeSeconds match {
                case Some(timeSeconds) =>
                    println(s"[DEBUG buildAndSubmitUpdateTransaction] Using provided validityIntervalTimeSeconds: $timeSeconds")
                    timeSeconds.toLong * 1000
                case None =>
                    val freshTime = System.currentTimeMillis()
                    println(s"[DEBUG buildAndSubmitUpdateTransaction] No validityIntervalTimeSeconds provided, using fresh time: $freshTime")
                    freshTime
            }
            val currentSlotFromTime = slotConfig.timeToSlot(currentPosixTimeMs)

            // Compute what the validator will see (slot -> POSIX ms / 1000)
            val validatorWillSeeMs = slotConfig.zeroTime + (currentSlotFromTime - slotConfig.zeroSlot) * slotConfig.slotLength
            val validatorWillSeeSeconds = validatorWillSeeMs / 1000

            println(s"[DEBUG buildAndSubmitUpdateTransaction]")
            println(s"  Input validityIntervalTimeSeconds: $validityIntervalTimeSeconds")
            println(s"  Computed currentPosixTimeMs: $currentPosixTimeMs")
            println(s"  Computed currentSlot: $currentSlotFromTime")
            println(s"  Validator will see (ms): $validatorWillSeeMs")
            println(s"  Validator will see (seconds): $validatorWillSeeSeconds")
            println(s"  MATCH: ${validityIntervalTimeSeconds.map(_ == validatorWillSeeSeconds).getOrElse(false)}")

            // Build, sign, and submit
            // Get current slot from the blockchain (not from time calculation)
            val latestBlock = backendService.getBlockService.getLatestBlock.getValue
            val currentSlot = latestBlock.getSlot
            println(s"[DEBUG] Current slot from blockchain: $currentSlot")
            
            val result = quickTxBuilder.compose(scriptTx)
              .feePayer(account.baseAddress())
              .withSigner(com.bloxbean.cardano.client.function.helper.SignerProviders.signerFrom(account))
              .withTxEvaluator(scalusEvaluator)
              .validFrom(currentSlot)
              .completeAndWait()

            if (result.isSuccessful) {
                Right(result.getValue)
            } else {
                Left(s"Transaction failed: ${result.getResponse}")
            }
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(s"Error building UpdateOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}")
        }
    }
}
