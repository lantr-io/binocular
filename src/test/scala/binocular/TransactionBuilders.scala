package binocular

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.{Address, AddressProvider}
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.{PlutusData, PlutusV3Script, Redeemer}
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.{Transaction, TransactionOutput}
import com.bloxbean.cardano.client.util.HexUtil
import scalus.builtin.{ByteString, Data, ToData}

import scala.jdk.CollectionConverters.*

/** Helper functions for building Binocular Oracle transactions on Yaci DevKit
  *
  * Provides utilities for:
  * - Creating script UTXOs with ChainState datum
  * - Building UpdateOracle redeemer
  * - Constructing and signing transactions
  * - Working with the Cardano transaction builder API
  */
object TransactionBuilders {

    /** Convert Scalus Data to Cardano PlutusData */
    def scalusDataToPlutusData(data: Data): PlutusData = {
        val cborBytes = data.toCbor // Array[Byte]
        val hexString = ByteString.fromArray(cborBytes).toHex
        PlutusData.deserialize(HexUtil.decodeHexString(hexString))
    }

    /** Convert Cardano PlutusData to Scalus Data */
    def plutusDataToScalusData(plutusData: PlutusData): Data = {
        val hexString = plutusData.serializeToHex
        Data.fromCbor(ByteString.fromHex(hexString))
    }

    /** Create a script address from a PlutusV3Script */
    def getScriptAddress(script: PlutusV3Script): Address = {
        AddressProvider.getEntAddress(script, Networks.testnet())
    }

    /** Build a transaction output at script address with datum
      *
      * @param scriptAddress the script address to send funds to
      * @param lovelaceAmount amount of lovelace to lock at the script
      * @param datum the datum to attach (as Scalus Data)
      * @return TransactionOutput
      */
    def buildScriptOutput(
        scriptAddress: Address,
        lovelaceAmount: Long,
        datum: Data
    ): TransactionOutput = {
        val plutusDatum = scalusDataToPlutusData(datum)

        TransactionOutput
          .builder()
          .address(scriptAddress.getAddress)
          .value(new com.bloxbean.cardano.client.transaction.spec.Value(
            java.math.BigInteger.valueOf(lovelaceAmount),
            List.empty.asJava
          ))
          .inlineDatum(plutusDatum)
          .build()
    }

    /** Create UpdateOracle redeemer
      *
      * @param blockHeaders list of Bitcoin block headers to include
      * @return Redeemer with UpdateOracle action
      */
    def createUpdateOracleRedeemer(
        blockHeaders: scalus.prelude.List[BitcoinValidator.BlockHeader]
    ): Redeemer = {
        val action = BitcoinValidator.Action.UpdateOracle(blockHeaders)
        // Action derives ToData, so we can use the derived instance
        val actionData = ToData.toData(action)(using BitcoinValidator.Action.derived$ToData)
        val redeemerData = scalusDataToPlutusData(actionData)

        Redeemer
          .builder()
          .data(redeemerData)
          .build()
    }

    /** Build UpdateOracle transaction
      *
      * NOTE: This is a simplified stub. Full implementation requires understanding
      * the cardano-client-lib TxBuilder API which varies by version.
      *
      * For actual usage, construct the transaction manually using:
      * 1. Query script UTXO with backendService.getUtxoService
      * 2. createUpdateOracleRedeemer() to build the redeemer
      * 3. buildScriptOutput() to create the new output with updated datum
      * 4. Use QuickTxBuilder or TxBuilder to construct the transaction with:
      *    - Script input with redeemer
      *    - Script reference or script witness
      *    - New script output
      *    - Collateral input
      *    - Balance and fees
      * 5. account.sign() to sign
      *
      * @param account the account to sign the transaction
      * @param backendService backend service for querying chain state
      * @param scriptAddress the Binocular script address
      * @param prevChainState the previous ChainState datum
      * @param newChainState the new ChainState datum after update
      * @param blockHeaders the Bitcoin block headers to submit
      * @param script the Binocular PlutusV3 script
      * @return Either error message or signed transaction
      */
    def buildUpdateOracleTransaction(
        account: Account,
        backendService: BackendService,
        scriptAddress: Address,
        prevChainState: BitcoinValidator.ChainState,
        newChainState: BitcoinValidator.ChainState,
        blockHeaders: scalus.prelude.List[BitcoinValidator.BlockHeader],
        script: PlutusV3Script
    ): Either[String, Transaction] = {
        Left("Not implemented - requires cardano-client-lib TxBuilder integration. See docstring for manual approach.")
    }

    /** Create initial script UTXO with genesis ChainState
      *
      * Uses QuickTx API to create a transaction that locks funds at the script address
      * with the genesis ChainState as an inline datum.
      *
      * @param account the account to send funds from
      * @param backendService backend service for submitting transaction
      * @param scriptAddress the Binocular script address
      * @param genesisState the initial ChainState
      * @param lovelaceAmount amount to lock at script (default: 5 ADA)
      * @return Either error message or transaction hash
      */
    def createInitialScriptUtxo(
        account: Account,
        backendService: BackendService,
        scriptAddress: Address,
        genesisState: BitcoinValidator.ChainState,
        lovelaceAmount: Long = 5_000_000L
    ): Either[String, String] = {
        try {
            // Convert ChainState to PlutusData for inline datum
            val stateData = ToData.toData(genesisState)(using BitcoinValidator.ChainState.derived$ToData)
            val plutusDatum = scalusDataToPlutusData(stateData)

            // Create amount
            val amount = Amount.lovelace(java.math.BigInteger.valueOf(lovelaceAmount))

            // Build transaction using QuickTx API
            val quickTxBuilder = new QuickTxBuilder(backendService)

            val tx = new Tx()
              .payToContract(scriptAddress.getAddress, amount, plutusDatum)
              .from(account.baseAddress())

            val result = quickTxBuilder.compose(tx)
              .withSigner(SignerProviders.signerFrom(account))
              .completeAndWait((msg: String) => println(s"[QuickTx] $msg"))

            if (result.isSuccessful) {
                Right(result.getValue)
            } else {
                Left(s"Transaction failed: ${result.getResponse}")
            }
        } catch {
            case e: Exception =>
                Left(s"Failed to create initial UTXO: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}")
        }
    }

    /** Helper to get protocol parameters as Scalus values for testing */
    def getProtocolParamsHelper(backendService: BackendService): Either[String, ProtocolParamsInfo] = {
        try {
            val params = backendService.getEpochService.getProtocolParameters().getValue
            Right(ProtocolParamsInfo(
              minFeeA = params.getMinFeeA,
              minFeeB = params.getMinFeeB,
              maxTxSize = params.getMaxTxSize,
              maxBlockHeaderSize = params.getMaxBlockHeaderSize
            ))
        } catch {
            case e: Exception =>
                Left(s"Failed to fetch protocol params: ${e.getMessage}")
        }
    }

    case class ProtocolParamsInfo(
        minFeeA: Int,
        minFeeB: Int,
        maxTxSize: Int,
        maxBlockHeaderSize: Int
    )
}
