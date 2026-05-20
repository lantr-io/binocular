package binocular.watchtower

import binocular.bitcoin.{BitcoinHelpers, BlockInfo, RawTransactionInfo, SimpleBitcoinRpc}
import binocular.oracle.{BlockHeader, MerkleTree}

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}

/** Everything needed to satisfy `peg_in.ak`'s mint handler for one BTC peg-in.
  *
  *   - `rawTxHex` goes into `PegInDatum.sourceChainPegInRawTx`.
  *   - `blockHeader` + `mpfHeaderInclusionProof` together prove the BTC block is in binocular's
  *     `confirmed_blocks_root` MPF.
  *   - `txInBlockMerklePath` + `txIndex` prove the BTC tx is in that block.
  *   - `pegInVout`, `pegInAmountSat`, `userSourceChainPubKey` are convenience fields parsed from
  *     the tx that the caller (PegInRequest tx builder) copies into the rest of the `PegInDatum`.
  *
  * Produced by [[PegInProofBundle.produce]].
  */
case class PegInProofBundle(
    rawTxHex: ByteString,
    blockHeader: ByteString,
    txIndex: Int,
    txInBlockMerklePath: Seq[ByteString],
    mpfHeaderInclusionProof: ScalusList[ProofStep],
    pegInVout: Int,
    pegInAmountSat: Long,
    userSourceChainPubKey: ByteString
)

object PegInProofBundle {

    // OP_RETURN (0x6a) + PUSH35 (0x23) + "BFR" (0x42 0x46 0x52).
    // 35 = 3 ("BFR") + 32 (xonly payload).
    private val BfrOpReturnPrefix = "6a23424652"

    // OP_1 (0x51) + PUSH32 (0x20) = the leading 2 bytes of every P2TR scriptPubKey.
    private val P2trPrefix = "5120"

    sealed trait ProduceError extends Product with Serializable
    final case class TxNotConfirmed(txId: String) extends ProduceError
    final case class TxNotInBlock(txId: String, blockHash: String) extends ProduceError
    final case class NoBfrOpReturn(txId: String) extends ProduceError
    final case class NoP2trOutput(txId: String) extends ProduceError

    /** Build the proof bundle for `btcTxId`.
      *
      * @param confirmedBlocksMpf
      *   Off-chain MPF mirror of the oracle's `confirmed_blocks_root` — must already contain the
      *   BTC block holding the tx, otherwise `proveMembership` will produce a useless proof.
      */
    def produce(
        rpc: SimpleBitcoinRpc,
        confirmedBlocksMpf: OffChainMPF,
        btcTxId: String
    )(using ec: ExecutionContext): Future[Either[ProduceError, PegInProofBundle]] = {
        rpc.getRawTransaction(btcTxId).flatMap { raw =>
            raw.blockhash match
                case None =>
                    Future.successful(Left(TxNotConfirmed(btcTxId)))
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
    ): Either[ProduceError, PegInProofBundle] = {
        val txIndex = block.tx.indexWhere(_.txid == raw.txid)
        if txIndex < 0 then return Left(TxNotInBlock(raw.txid, block.hash))

        val ourTx = block.tx(txIndex)

        val bfr = ourTx.vouts
            .find(_.scriptPubKey.startsWith(BfrOpReturnPrefix))
            .toRight(NoBfrOpReturn(raw.txid)) match
            case Left(e)  => return Left(e)
            case Right(v) => v

        val pegIn = ourTx.vouts
            .find(v => v.scriptPubKey.startsWith(P2trPrefix) && v.index != bfr.index)
            .toRight(NoP2trOutput(raw.txid)) match
            case Left(e)  => return Left(e)
            case Right(v) => v

        val xonlyHex = bfr.scriptPubKey.drop(BfrOpReturnPrefix.length).take(64)
        val userXonly = ByteString.fromHex(xonlyHex)

        // Bitcoin tx Merkle tree over the block's txids in internal (little-endian) byte order —
        // i.e. each RPC-reported `txid` (display/big-endian) reversed. This is exactly what the
        // block header's `merkleroot` commits to. (Computing leaves locally via sha256d of a
        // witness-stripped serialization is equivalent only if stripping is exact; using the
        // node's authoritative txid avoids that dependency.)
        val txHashes =
            block.tx.map(tx => ByteString.fromArray(tx.txid.hexToBytes.reverse))
        val merklePath = MerkleTree.fromHashes(txHashes).makeMerkleProof(txIndex)

        val blockHashLE =
            BitcoinHelpers.blockHeaderHash(BlockHeader(ByteString.fromHex(headerHex)))
        val mpfProof = mpf.proveMembership(blockHashLE)

        // Store the witness-stripped (legacy) serialization: the on-chain
        // `bitcoin_hash(source_chain_peg_in_raw_tx)` is a plain double-sha256 with no witness
        // stripping, and it must equal the txid leaf the block Merkle root commits to. A
        // witness-included serialization would hash to the wtxid and fail the inclusion proof.
        Right(
          PegInProofBundle(
            rawTxHex = BitcoinHelpers.stripWitnessData(ByteString.fromHex(ourTx.hex)),
            blockHeader = ByteString.fromHex(headerHex),
            txIndex = txIndex,
            txInBlockMerklePath = merklePath.toIndexedSeq,
            mpfHeaderInclusionProof = mpfProof,
            pegInVout = pegIn.index,
            pegInAmountSat = math.round(pegIn.valueBtc * 1e8),
            userSourceChainPubKey = userXonly
          )
        )
    }
}
