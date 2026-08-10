package binocular.cli.commands

import binocular.BinocularConfig
import binocular.bitcoin.SimpleBitcoinRpc
import binocular.cli.{Command, CommandHelpers, Console}
import binocular.watchtower.PegInProofBundle

import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Serve the [OB-12] deposit-inclusion bundle for one Bitcoin deposit outpoint.
  *
  * Thin transport over [[PegInProofBundle.produceForOutpoint]]. Prints a JSON object carrying the
  * four items the `PegInRequest` mint redeemer needs — the 80-byte block header, the tx merkle
  * proof with its index, the MPF membership proof of the block hash against the oracle's
  * `confirmed_blocks_root`, and the raw deposit transaction — plus the convenience fields the rest
  * of the `PegInDatum` copies. [OF-8]: a frontend builds its `PegInRequest` from this bundle
  * instead of assembling the proofs itself.
  *
  * [OB-13]: served to any caller, on the same terms as [SPI-4] — the mint handler verifies every
  * element on-chain against the oracle, so a wrong bundle fails at submission and this server needs
  * no trust.
  */
case class DepositProofCommand(
    outpoint: String
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val pegInUtxoId = CommandHelpers.parseBtcOutpoint(outpoint) match {
            case Right(b)  => b
            case Left(err) => Console.error(s"Invalid outpoint: $err"); break(1)
        }

        // 1. Oracle state + its confirmed-blocks MPF mirror (same rebuild as pegin-request).
        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val oracleUtxo =
            try
                CommandHelpers
                    .findOracleUtxo(setup.provider, setup.script.scriptHash)
                    .await(timeout)
            catch { case e: Exception => Console.error(e.getMessage); break(1) }
        val chainState = CommandHelpers
            .parseChainState(oracleUtxo)
            .getOrElse { Console.error("Oracle UTxO has no valid ChainState datum"); break(1) }
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val mpf = CommandHelpers
            .reconstructMpf(rpc, chainState, config.oracle.startHeight)
            .valueOr { err =>
                Console.error(s"Rebuilding confirmed-blocks MPF: $err"); break(1)
            }
        Console.info("oracle height", chainState.ctx.height.toString)

        // 2. The bundle, keyed by the deposit outpoint ([OB-12]).
        PegInProofBundle.produceForOutpoint(rpc, mpf, pegInUtxoId).await(timeout) match {
            case Left(err) =>
                Console.error(s"Proof bundle: $err")
                1
            case Right(bundle) =>
                val json = ujson.Obj(
                  "peg_in_utxo_id" -> bundle.pegInUtxoId.toHex,
                  // PegInDatum.source_chain_peg_in_raw_tx (witness-stripped, hashes to the txid).
                  "raw_tx" -> bundle.rawTxHex.toHex,
                  // PegInDatum.source_chain_peg_in_raw_tx_index.
                  "tx_index" -> bundle.txIndex,
                  // PegInRequest.block_header, 80 bytes.
                  "block_header" -> bundle.blockHeader.toHex,
                  // PegInRequest.tx_in_block_header_inclusion_proof.
                  "tx_merkle_proof" -> ujson.Arr.from(bundle.txInBlockMerklePath.map(_.toHex)),
                  // PegInRequest.block_header_in_source_chain_inclusion_proof, as Data CBOR.
                  "block_mpf_proof_cbor" ->
                      ByteString.fromArray(bundle.mpfHeaderInclusionProof.toData.toCbor).toHex,
                  // Convenience fields for the rest of the PegInDatum.
                  "peg_in_vout" -> bundle.pegInVout,
                  "peg_in_amount_sat" -> bundle.pegInAmountSat,
                  "user_source_chain_pub_key" -> bundle.userSourceChainPubKey.toHex
                )
                println(ujson.write(json, indent = 2))
                0
        }
    }
}
