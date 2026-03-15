package binocular

import binocular.util.SlotConfigHelper
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Script, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}

import scalus.utils.await

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext

/** Helper functions for building Binocular Oracle transactions on Yaci DevKit
  *
  * Provides utilities for:
  *   - Creating script UTXOs with ChainState datum
  *   - Building UpdateOracle redeemer
  *   - Constructing and signing transactions
  *   - Working with the Scalus TxBuilder API
  */
object TransactionBuilders {

    /** Get the compiled BitcoinValidator as PlutusV3 script (IT-specific, changPV) */
    def compiledBitcoinScript(): Script.PlutusV3 = {
        IntegrationTestContract.itPlutusScript
    }

    /** Build UpdateOracle transaction
      *
      * @param signer
      *   TransactionSigner to sign the transaction
      * @param provider
      *   BlockchainProvider for querying and submitting
      * @param scriptAddress
      *   the Binocular script address
      * @param sponsorAddress
      *   Address to pay fees from
      * @param prevChainState
      *   the previous ChainState datum
      * @param newChainState
      *   the new ChainState datum after update
      * @param blockHeaders
      *   the Bitcoin block headers to submit
      * @return
      *   Either error message or transaction hash
      */
    def buildUpdateOracleTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        prevChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader]
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        try {
            val script = compiledBitcoinScript()
            val cardanoInfo = provider.cardanoInfo

            // 1. Query script UTXOs to find the oracle UTXO
            val utxosResult = provider.findUtxos(scriptAddress).await(30.seconds)

            val scriptUtxos: List[Utxo] = utxosResult match {
                case Right(u) =>
                    u.map { case (input, output) => Utxo(input, output) }
                        .toList
                        .filter(_.output.inlineDatum.isDefined)
                case Left(err) =>
                    return Left(s"No UTXOs found at script address: $err")
            }

            if scriptUtxos.isEmpty then {
                return Left(s"No UTXOs with inline datum found at script address")
            }

            val scriptUtxo = scriptUtxos.head
            println(
              s"[UpdateOracle] Found script UTXO: ${scriptUtxo.input.transactionId.toHex}#${scriptUtxo.input.index}"
            )

            // 2. Create UpdateOracle redeemer
            val parentPath = prevChainState.forkTree.findTipPath
            val action = UpdateOracle(blockHeaders, parentPath, ScalusList.Nil)
            println(s"[UpdateOracle] Created redeemer with ${blockHeaders.length} headers")

            // 3. Calculate amount to lock (same as input)
            val lovelaceAmount = scriptUtxo.output.value.coin.value
            println(s"[UpdateOracle] Locking $lovelaceAmount lovelace at script")

            // 4. Build transaction using TxBuilder
            // Validator requires finite validity interval <= MaxValidityWindow (10 min)
            val (validityInstant, _) =
                SlotConfigHelper.computeValidityIntervalTime(cardanoInfo)
            val validToInstant =
                validityInstant.plusMillis(BitcoinValidator.MaxValidityWindow.toLong)

            val tx = TxBuilder(cardanoInfo)
                .spend(scriptUtxo, action, script)
                .payTo(scriptAddress, scriptUtxo.output.value, newChainState)
                .validFrom(validityInstant)
                .validTo(validToInstant)
                .complete(provider, sponsorAddress)
                .await(60.seconds)
                .sign(signer)
                .transaction

            // 5. Submit
            val result = provider.submit(tx).await(30.seconds)
            result match {
                case Right(txHash) =>
                    val hash = txHash.toHex
                    println(s"[UpdateOracle] Transaction submitted: $hash")
                    Right(hash)
                case Left(err) =>
                    Left(s"Transaction failed: $err")
            }
        } catch {
            case e: Exception =>
                Left(
                  s"Failed to build UpdateOracle transaction: ${e.getMessage}\n${e.getStackTrace.take(10).mkString("\n")}"
                )
        }
    }

    /** Create initial script UTXO with genesis ChainState
      *
      * @param signer
      *   TransactionSigner to sign
      * @param provider
      *   BlockchainProvider for querying and submitting
      * @param scriptAddress
      *   the Binocular script address
      * @param sponsorAddress
      *   Address to pay fees from
      * @param genesisState
      *   the initial ChainState
      * @param lovelaceAmount
      *   amount to lock at script (default: 5 ADA)
      * @return
      *   Either error message or transaction hash
      */
    def createInitialScriptUtxo(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        genesisState: ChainState,
        lovelaceAmount: Long = 5_000_000L
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        try {
            val tx = TxBuilder(provider.cardanoInfo)
                .payTo(scriptAddress, Value.lovelace(lovelaceAmount), genesisState)
                .complete(provider, sponsorAddress)
                .await(30.seconds)
                .sign(signer)
                .transaction

            val result = provider.submit(tx).await(30.seconds)
            result match {
                case Right(txHash) => Right(txHash.toHex)
                case Left(err)     => Left(s"Transaction failed: $err")
            }
        } catch {
            case e: Exception =>
                Left(
                  s"Failed to create initial UTXO: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}"
                )
        }
    }

}
