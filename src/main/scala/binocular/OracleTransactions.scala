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
        blockHeaders: scalus.prelude.List[BitcoinValidator.BlockHeader]
    ): Redeemer = {
        val action = BitcoinValidator.Action.UpdateOracle(blockHeaders)
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
      * @param newChainState New ChainState datum after applying headers
      * @param blockHeaders Bitcoin block headers to submit
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

            // Convert current ChainState to PlutusData
            val currentStateData = ToData.toData(currentChainState)(using BitcoinValidator.ChainState.derived$ToData)
            val currentDatum = scalusDataToPlutusData(currentStateData)

            // DEBUG: Print the datum structure
            println(s"[DEBUG] Current datum CBOR: ${com.bloxbean.cardano.client.util.HexUtil.encodeHexString(currentDatum.serializeToBytes())}")
            println(s"[DEBUG] Current ChainState: blockHeight=${currentChainState.blockHeight}, recentTimestamps.size=${currentChainState.recentTimestamps.size}")

            // Create UpdateOracle redeemer
            val redeemer = createUpdateOracleRedeemer(blockHeaders)
            println(s"[DEBUG] Redeemer CBOR: ${com.bloxbean.cardano.client.util.HexUtil.encodeHexString(redeemer.getData.serializeToBytes())}")

            // Convert new ChainState to PlutusData
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

            val scalusEvaluator = new scalus.bloxbean.ScalusTransactionEvaluator(slotConfig, protocolParams, utxoSupplier)
            println("[DEBUG] Using ScalusEvaluator (PlutusVM) for script cost evaluation")

            val quickTxBuilder = new QuickTxBuilder(backendService)

            val scriptTx = new ScriptTx()
              .collectFrom(targetUtxo, currentDatum, redeemer.getData)
              .payToContract(scriptAddress.getAddress, amount, newDatum)
              .attachSpendingValidator(script)

            // Get current slot for validity interval
            // The validator requires a finite validity start interval to access tx.validRange
            val blockResult = backendService.getBlockService.getLatestBlock()
            if (!blockResult.isSuccessful) {
                throw new RuntimeException(s"Failed to get current slot: ${blockResult.getResponse}")
            }
            val currentSlot = blockResult.getValue.getSlot



            // Build, sign, and submit
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
