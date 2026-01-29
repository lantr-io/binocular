package binocular

import scalus.cardano.address.Address
import scalus.cardano.ledger.{CardanoInfo, DatumOption, PlutusScript, Script, ScriptRef, Transaction, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** Helper functions for building Binocular Oracle transactions on Yaci DevKit
  *
  * Provides utilities for:
  *   - Creating script UTXOs with ChainState datum
  *   - Building UpdateOracle redeemer
  *   - Constructing and signing transactions
  *   - Working with the Scalus TxBuilder API
  */
object TransactionBuilders {

    /** Get the compiled BitcoinValidator as PlutusV3 script */
    def compiledBitcoinScript(): Script.PlutusV3 = {
        Script.PlutusV3(BitcoinContract.bitcoinProgram.cborByteString)
    }

    /** Create a script address from the PlutusV3 script
      *
      * @param network
      *   Scalus network (Testnet/Mainnet)
      */
    def getScriptAddress(network: scalus.cardano.address.Network): Address = {
        val script = compiledBitcoinScript()
        val credential = scalus.cardano.ledger.Credential.ScriptHash(script.scriptHash)
        Address(network, credential)
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
            val utxosResult = Await.result(
              provider.findUtxos(scriptAddress),
              30.seconds
            )

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

            // 2. Create UpdateOracle action as redeemer
            val dummyHash = ByteString.empty
            val currentTime = BigInt(System.currentTimeMillis() / 1000)
            val action = Action.UpdateOracle(blockHeaders, currentTime, dummyHash)
            println(s"[UpdateOracle] Created redeemer with ${blockHeaders.length} headers")

            // 3. Calculate amount to lock (same as input)
            val lovelaceAmount = scriptUtxo.output.value.coin.value
            println(s"[UpdateOracle] Locking $lovelaceAmount lovelace at script")

            // 4. Build transaction using TxBuilder
            val tx = Await
                .result(
                  TxBuilder(cardanoInfo)
                      .spend(scriptUtxo, action, script)
                      .payTo(scriptAddress, scriptUtxo.output.value, newChainState)
                      .complete(provider, sponsorAddress),
                  60.seconds
                )
                .sign(signer)
                .transaction

            // 5. Submit
            val result = Await.result(provider.submit(tx), 30.seconds)
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
            val tx = Await
                .result(
                  TxBuilder(provider.cardanoInfo)
                      .payTo(scriptAddress, Value.lovelace(lovelaceAmount), genesisState)
                      .complete(provider, sponsorAddress),
                  30.seconds
                )
                .sign(signer)
                .transaction

            val result = Await.result(provider.submit(tx), 30.seconds)
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

    /** Apply block headers to ChainState to calculate new state */
    def applyHeaders(
        currentState: ChainState,
        headers: ScalusList[BlockHeader],
        currentTime: BigInt
    ): ChainState = {
        headers.foldLeft(currentState) { (state, header) =>
            BitcoinValidator.updateTip(state, header, currentTime)
        }
    }
}
