package binocular.watchtower

import binocular.bitcoin.{BitcoinHelpers, BlockInfo, RawTransactionInfo, SimpleBitcoinRpc}
import binocular.oracle.{BlockHeader, MerkleTree}

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}

/** Inclusion proof for a confirmed Treasury Movement (TM) tx, for the F4 completion path.
  *
  * Unlike [[PegInProofBundle]] (which parses the peg-in deposit's BFR OP_RETURN + P2TR output and
  * stores the tx witness-stripped), a TM is a plain sweep: this keeps the **full segwit
  * serialization** in `rawTxFull` because `peg_in.ak`'s `legit_TM_verifier` inspects the TM's input
  * witnesses to classify the spend path. `peg-in.ak` strips the witness on-chain to recover the
  * txid for the Merkle inclusion check (the block's tx-merkle-root commits to txids), so the bytes
  * here must be the full form.
  *
  *   - `blockHeader` + `mpfHeaderInclusionProof` prove the block is in the oracle's
  *     `confirmed_blocks_root`.
  *   - `txIndex` + `txInBlockMerklePath` prove the TM is in that block (over txids).
  */
case class TmProofBundle(
    rawTxFull: ByteString,
    blockHeader: ByteString,
    txIndex: Int,
    txInBlockMerklePath: Seq[ByteString],
    mpfHeaderInclusionProof: ScalusList[ProofStep]
)

object TmProofBundle {

    sealed trait ProduceError extends Product with Serializable
    final case class TxNotConfirmed(txId: String) extends ProduceError
    final case class TxNotInBlock(txId: String, blockHash: String) extends ProduceError
    /** The TM is confirmed on Bitcoin but its block is not (yet) in the oracle's
      * `confirmed_blocks_root` — the oracle lags Bitcoin; retry once it catches up.
      */
    final case class BlockNotConfirmedByOracle(txId: String, blockHash: String) extends ProduceError

    /** Build the inclusion proof for `tmTxId`.
      *
      * @param confirmedBlocksMpf
      *   off-chain mirror of the oracle's `confirmed_blocks_root`; must already contain the block
      *   holding the TM.
      */
    def produce(
        rpc: SimpleBitcoinRpc,
        confirmedBlocksMpf: OffChainMPF,
        tmTxId: String
    )(using ec: ExecutionContext): Future[Either[ProduceError, TmProofBundle]] = {
        rpc.getRawTransaction(tmTxId).flatMap { raw =>
            raw.blockhash match
                case None => Future.successful(Left(TxNotConfirmed(tmTxId)))
                case Some(blockHashHex) =>
                    for
                        headerHex <- rpc.getBlockHeaderRaw(blockHashHex)
                        block <- rpc.getBlock(blockHashHex)
                    yield assemble(raw, block, headerHex, confirmedBlocksMpf)
        }
    }

    private def assemble(
        raw: RawTransactionInfo,
        block: BlockInfo,
        headerHex: String,
        mpf: OffChainMPF
    ): Either[ProduceError, TmProofBundle] = {
        val txIndex = block.tx.indexWhere(_.txid == raw.txid)
        if txIndex < 0 then return Left(TxNotInBlock(raw.txid, block.hash))

        val ourTx = block.tx(txIndex)

        // Bitcoin tx Merkle tree over txids in internal (little-endian) byte order — each RPC txid
        // (display/big-endian) reversed. This is exactly what the block header merkleroot commits to.
        val txHashes = block.tx.map(tx => ByteString.fromArray(tx.txid.hexToBytes.reverse))
        val merklePath = MerkleTree.fromHashes(txHashes).makeMerkleProof(txIndex)

        val blockHashLE =
            BitcoinHelpers.blockHeaderHash(BlockHeader(ByteString.fromHex(headerHex)))
        // proveMembership throws if the block isn't in the MPF; the oracle lagging Bitcoin is a
        // normal state, so surface it as a structured error instead of an uncaught crash.
        if mpf.get(blockHashLE).isEmpty then return Left(BlockNotConfirmedByOracle(raw.txid, block.hash))
        val mpfProof = mpf.proveMembership(blockHashLE)

        Right(
          TmProofBundle(
            // FULL serialization (with witness) — peg-in.ak strips on-chain for the txid.
            rawTxFull = ByteString.fromHex(ourTx.hex),
            blockHeader = ByteString.fromHex(headerHex),
            txIndex = txIndex,
            txInBlockMerklePath = merklePath.toIndexedSeq,
            mpfHeaderInclusionProof = mpfProof
          )
        )
    }
}
