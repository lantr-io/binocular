package binocular.watchtower

import binocular.bitcoin.{BitcoinHelpers, BlockInfo, RawTransactionInfo, SimpleBitcoinRpc}
import binocular.oracle.{reverse, BlockHeader, MerkleTree}

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}

/** Everything needed to satisfy `peg_in.ak`'s mint handler for one BTC peg-in — the [OB-12]
  * deposit-inclusion bundle. The four items are exactly what the `PegInRequest` mint redeemer
  * carries:
  *
  *   - `rawTxHex` goes into `PegInDatum.source_chain_peg_in_raw_tx` (the raw deposit transaction);
  *   - `blockHeader` (80 bytes) + `mpfHeaderInclusionProof` together prove the BTC block is in the
  *     oracle's `confirmed_blocks_root` MPF;
  *   - `txInBlockMerklePath` + `txIndex` prove the BTC tx is in that block.
  *
  * `pegInVout`, `pegInAmountSat`, `userSourceChainPubKey` are convenience fields parsed from the tx
  * that the caller (the PegInRequest tx builder, or a frontend per [OF-8]) copies into the rest of
  * the `PegInDatum`.
  *
  * Produced by [[PegInProofBundle.produce]] (keyed by txid) or
  * [[PegInProofBundle.produceForOutpoint]] (keyed by the 36-byte deposit outpoint, [OB-12]).
  * [OB-13]: served to any caller on the same terms as [SPI-4] — the mint handler verifies every
  * element on-chain against the oracle, so a wrong bundle fails at submission and the server needs
  * no trust.
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
) {

    /** The deposit outpoint this bundle proves: txid (internal LE, 32 bytes) ++ vout (4 bytes LE).
      * This is the `PegInDatum.peg_in_utxo_id`.
      */
    def pegInUtxoId: ByteString =
        CpoTrieMirror.hintBytes(BitcoinHelpers.getTxHash(rawTxHex), pegInVout.toLong)
}

object PegInProofBundle {

    // The one-key BFR deposit beacon, always at vout 1:
    // OP_RETURN (0x6a) + PUSH35 (0x23) + "BFR" (0x42 0x46 0x52) + Q_auth(32).
    // Mirrors bifrost/bitcoin.ak::beacon_payload. Q_auth is the depositor's Taproot OUTPUT key and
    // serves both roles — the deposit's refund leaf commits it and the completion's BIP-322
    // signature verifies against it — so there is no second key to carry. The retired dual-key
    // form (PUSH67, "BFR" + D + Q_auth) is REFUSED there, so recognizing it here would serve
    // bundles whose PegInRequest mint fails `deposit_binding_ok` at submission.
    private val BfrOpReturnPrefix = "6a23424652"

    // Total beacon script: 6a + 23 + "BFR"(3) + 32 payload bytes = 37 bytes = 74 hex chars.
    private val BfrOpReturnScriptHexLength = 74

    // OP_1 (0x51) + PUSH32 (0x20) = the leading 2 bytes of every P2TR scriptPubKey.
    private val P2trPrefix = "5120"

    sealed trait ProduceError extends Product with Serializable
    final case class TxNotConfirmed(txId: String) extends ProduceError
    final case class TxNotInBlock(txId: String, blockHash: String) extends ProduceError
    final case class NoBfrOpReturn(txId: String) extends ProduceError
    final case class NoP2trOutput(txId: String) extends ProduceError

    /** The request named an outpoint that cannot be parsed at all. */
    final case class BadOutpoint(message: String) extends ProduceError

    /** The requested vout exists but is not a P2TR deposit output (`0x5120 ‖ 32B`), or the tx has
      * no such vout. `deposit_binding_ok` enforces the same on-chain, so a bundle for this outpoint
      * could never mint.
      */
    final case class VoutNotDeposit(txId: String, vout: Int) extends ProduceError

    /** The deposit is confirmed on Bitcoin but its block is not (yet) in the oracle's
      * `confirmed_blocks_root` — the oracle lags Bitcoin; retry once it catches up.
      */
    final case class BlockNotConfirmedByOracle(txId: String, blockHash: String) extends ProduceError

    /** Build the proof bundle for `btcTxId`, auto-detecting the deposit vout (the first P2TR output
      * that is not the beacon).
      *
      * @param confirmedBlocksMpf
      *   Off-chain MPF mirror of the oracle's `confirmed_blocks_root` — must already contain the
      *   BTC block holding the tx, otherwise `proveMembership` will produce a useless proof.
      */
    def produce(
        rpc: SimpleBitcoinRpc,
        confirmedBlocksMpf: OffChainMPF,
        btcTxId: String
    )(using ec: ExecutionContext): Future[Either[ProduceError, PegInProofBundle]] =
        fetchAndAssemble(rpc, confirmedBlocksMpf, btcTxId, requestedVout = None)

    /** Build the [OB-12] bundle for one 36-byte deposit outpoint (`txid LE ‖ vout LE`) — the
      * `peg_in_utxo_id` a `PegInRequest` names. Unlike [[produce]], the vout is the caller's, and
      * it must be the P2TR deposit output, exactly as `deposit_binding_ok` requires at mint.
      */
    def produceForOutpoint(
        rpc: SimpleBitcoinRpc,
        confirmedBlocksMpf: OffChainMPF,
        pegInUtxoId: ByteString
    )(using ec: ExecutionContext): Future[Either[ProduceError, PegInProofBundle]] =
        CpoTrieMirror.parseHint(pegInUtxoId) match {
            case None =>
                Future.successful(
                  Left(
                    BadOutpoint(
                      s"the deposit outpoint must be 36 bytes (txid ++ vout LE), got " +
                          s"${pegInUtxoId.size}"
                    )
                  )
                )
            case Some((txidLE, vout)) =>
                fetchAndAssemble(
                  rpc,
                  confirmedBlocksMpf,
                  txidLE.reverse.toHex,
                  requestedVout = Some(vout.toInt)
                )
        }

    private def fetchAndAssemble(
        rpc: SimpleBitcoinRpc,
        confirmedBlocksMpf: OffChainMPF,
        btcTxId: String,
        requestedVout: Option[Int]
    )(using ec: ExecutionContext): Future[Either[ProduceError, PegInProofBundle]] = {
        rpc.getRawTransaction(btcTxId).flatMap { raw =>
            raw.blockhash match
                case None =>
                    Future.successful(Left(TxNotConfirmed(btcTxId)))
                case Some(blockHashHex) =>
                    for
                        headerHex <- rpc.getBlockHeaderRaw(blockHashHex)
                        block <- rpc.getBlock(blockHashHex)
                    yield assemble(raw, block, headerHex, confirmedBlocksMpf, requestedVout)
        }
    }

    /** Pure assembly step, package-visible for tests.
      *
      * @param requestedVout
      *   `Some(v)` pins the deposit output to vout `v` (the outpoint-keyed [OB-12] path) and
      *   requires it to be P2TR; `None` auto-detects it (the txid-keyed path).
      */
    private[binocular] def assemble(
        raw: RawTransactionInfo,
        block: BlockInfo,
        headerHex: String,
        mpf: OffChainMPF,
        requestedVout: Option[Int]
    ): Either[ProduceError, PegInProofBundle] = {
        val txIndex = block.tx.indexWhere(_.txid == raw.txid)
        if txIndex < 0 then return Left(TxNotInBlock(raw.txid, block.hash))

        val ourTx = block.tx(txIndex)

        // The BFR beacon sits at vout 1, and ONLY vout 1 — bitcoin.ak::beacon_payload
        // reads get_vout_scriptpubkey(raw_tx, 1). A beacon anywhere else can never mint.
        val bfr = ourTx.vouts
            .find(v =>
                v.index == 1 && v.scriptPubKey.startsWith(BfrOpReturnPrefix) &&
                    v.scriptPubKey.length == BfrOpReturnScriptHexLength
            )
            .toRight(NoBfrOpReturn(raw.txid)) match
            case Left(e)  => return Left(e)
            case Right(v) => v

        val pegIn = requestedVout match {
            case Some(v) =>
                // The caller named the outpoint: its vout must be the P2TR deposit output, exactly
                // as deposit_binding_ok checks on-chain.
                ourTx.vouts
                    .find(o => o.index == v && o.scriptPubKey.startsWith(P2trPrefix))
                    .toRight(VoutNotDeposit(raw.txid, v)) match
                    case Left(e)  => return Left(e)
                    case Right(o) => o
            case None =>
                ourTx.vouts
                    .find(v => v.scriptPubKey.startsWith(P2trPrefix) && v.index != bfr.index)
                    .toRight(NoP2trOutput(raw.txid)) match
                    case Left(e)  => return Left(e)
                    case Right(v) => v
        }

        // The beacon payload after "BFR" is Q_auth(32) and nothing else — the depositor's Taproot
        // output key, which IS the `user_source_chain_pub_key` (mirrors
        // bitcoin.ak::get_op_return_depositor_key). It is also the key the deposit's refund leaf
        // commits, so no refund key has to be recovered or carried.
        val xonlyHex = bfr.scriptPubKey
            .drop(BfrOpReturnPrefix.length)
            .take(64)
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
        // proveMembership throws if the block isn't in the MPF; the oracle lagging Bitcoin is a
        // normal state, so surface it as a structured error instead of an uncaught crash.
        if mpf.get(blockHashLE).isEmpty then
            return Left(BlockNotConfirmedByOracle(raw.txid, block.hash))
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
            // Exact 8-byte LE amount from the raw tx (matches on-chain parsing), not a rounded Double.
            pegInAmountSat = BitcoinHelpers
                .outputValueSat(ByteString.fromHex(ourTx.hex), BigInt(pegIn.index))
                .toLong,
            userSourceChainPubKey = userXonly
          )
        )
    }
}
