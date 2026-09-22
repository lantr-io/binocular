package binocular.bitcoin

import org.bitcoins.core.crypto.{TaprootSerializationOptions, TaprootTxSigComponent, TransactionSignatureSerializer}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey, ScriptWitness, TaprootScriptPath}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint, TransactionOutput, TransactionWitness, WitnessTransaction}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.crypto.SIGHASH_DEFAULT
import scodec.bits.ByteVector

import scala.util.Try

/** The depositor REFUND of a peg-in deposit (spec §10.1, TRF-75 to TRF-78, TRF-117).
  *
  * One input, the deposit, spent through the refund leaf; one output, the deposit less the fee.
  * There is no change: the refund takes the whole deposit back, so a second output would only make
  * dust, and the fee comes out of the payout because the deposit is the only money available.
  *
  * The refund is built up to five days after the deposit, and every mistake in it is silent until
  * the coins are already unrecoverable. So nothing here is supplied by the caller that could be
  * derived instead: the leaf, the control block and the spent scriptPubKey all come from `tree`,
  * which TRF-116 requires the caller to have MATCHED against the deposit's own script.
  */
object PegInRefund {

    /** The dust floor of a P2TR output. TRF-117 refuses a refund that would pay below it. */
    val DustSat: Long = 330

    /** A BIP-340 SIGHASH_DEFAULT signature is exactly 64 bytes, so a witness sized with this is the
      * size of the witness that finally ships.
      */
    private val DummySignature: ByteVector = ByteVector.low(64)

    /** `OP_CHECKSEQUENCEVERIFY OP_DROP`, the two bytes that follow the block count in the leaf. */
    private val CsvDrop: ByteVector = ByteVector(0xb2.toByte, 0x75.toByte)

    /** Build and sign the refund, or say why it cannot be built. Never throws.
      *
      * @param timeoutBlocks
      *   `pegin_refund_timeout_blocks`, as a BIP-68 block count. It is both the input's `nSequence`
      *   and the delay `OP_CHECKSEQUENCEVERIFY` inside the leaf compares against, so the two are
      *   checked to be the same number here rather than at broadcast, days later.
      */
    def build(
        tree: PeginTree,
        deposit: TransactionOutPoint,
        depositAmountSat: Long,
        timeoutBlocks: Int,
        payoutScriptPubKey: ByteVector,
        identity: Bip86Key,
        feeRateSatPerKvb: Long
    ): Either[String, Transaction] =
        for
            // First, and before anything is sized: a negative rate makes the fee negative, so the
            // payout EXCEEDS the deposit and the dust check below passes. The builder would return
            // a transaction that creates value out of nothing.
            _ <- Either.cond(feeRateSatPerKvb >= 0, (), s"negative fee rate $feeRateSatPerKvb")
            _ <- Either.cond(
              timeoutBlocks >= 1 && timeoutBlocks <= Taproot.MaxRelativeTimelockBlocks,
              (),
              s"timeoutBlocks ($timeoutBlocks) must be in 1..${Taproot.MaxRelativeTimelockBlocks}"
            )
            _ <- Either.cond(
              tree.refundLeaf.startsWith(Taproot.scriptNumPush(timeoutBlocks) ++ CsvDrop),
              (),
              s"timeoutBlocks ($timeoutBlocks) is not the refund leaf's OP_CSV delay"
            )
            // MEASURED, not estimated: the shape below is serialised with a witness of the exact
            // size the real one occupies, so the fee cannot disagree with the vsize that ships.
            sized <- attempt("refund sizing failed") {
                body(deposit, timeoutBlocks, 0, payoutScriptPubKey, witness(tree, DummySignature))
            }
            feeSat = (sized.vsize * feeRateSatPerKvb + 999) / 1000
            payoutSat = depositAmountSat - feeSat
            // Checked BEFORE the real output is built: a fee above the deposit makes this
            // negative, and output construction would throw instead of returning this Left.
            _ <- Either.cond(
              payoutSat >= DustSat,
              (),
              s"refund payout $payoutSat sat (from $depositAmountSat sat less $feeSat sat fee) " +
                  s"is below the $DustSat sat dust floor"
            )
            signed <- attempt("refund build failed") {
                // The digest commits to the leaf and to the spent output, so it must be taken over
                // the body that finally ships, with the leaf and the control block already in
                // place and only the signature missing.
                val unsigned =
                    body(deposit, timeoutBlocks, payoutSat, payoutScriptPubKey, witness(tree))
                val digest = sighash(tree, depositAmountSat, unsigned)
                // The refund leaf names Q_auth, the TWEAKED output key, so this signs with it.
                WitnessTransaction(
                  unsigned.version,
                  unsigned.inputs,
                  unsigned.outputs,
                  unsigned.lockTime,
                  TransactionWitness(Vector(witness(tree, identity.signDigest(digest))))
                )
            }
        yield signed

    /** The BIP-341 script-path digest input 0 of `tx` signs, under SIGHASH_DEFAULT and no annex.
      *
      * The witness of `tx` is ignored and replaced: a signature is not part of what it commits to,
      * so the digest of a refund can be recomputed from the signed refund itself.
      */
    def sighash(tree: PeginTree, depositAmountSat: Long, tx: Transaction): ByteVector = {
        val prevouts = PreviousOutputMap(
          Map(
            tx.inputs.head.previousOutput ->
                TransactionOutput(Satoshis(depositAmountSat), tree.scriptPubKey)
          )
        )
        val committed = WitnessTransaction(
          tx.version,
          tx.inputs,
          tx.outputs,
          tx.lockTime,
          TransactionWitness(Vector(witness(tree)))
        )
        TransactionSignatureSerializer
            .hashForSignature(
              TaprootTxSigComponent(committed, UInt32.zero, prevouts, Policy.standardFlags),
              SIGHASH_DEFAULT,
              TaprootSerializationOptions(Some(Taproot.leafHash(tree.refundLeaf)), None, None)
            )
            .bytes
    }

    /** The witness, optionally signed. TRF-78 fixes the WIRE order as
      * `<sig> <refund leaf> <control block>`; bitcoin-s stores the stack top-first, so this is its
      * reverse.
      */
    private def witness(tree: PeginTree, signature: ByteVector*): ScriptWitness =
        TaprootScriptPath.fromStack(
          Vector(tree.refundControlBlock, tree.refundLeaf) ++ signature.toVector
        )

    private def body(
        deposit: TransactionOutPoint,
        timeoutBlocks: Int,
        payoutSat: Long,
        payoutScriptPubKey: ByteVector,
        witness: ScriptWitness
    ): WitnessTransaction = WitnessTransaction(
      // TRF-70: BIP-68 relative locktime is enforced only from version 2.
      Int32.two,
      // TRF-77: a BIP-68 block count is the count itself, with the disable bit and the type bit
      // clear. 0xFFFFFFFF would disable relative locktime and fail the leaf's OP_CSV.
      Vector(TransactionInput(deposit, EmptyScriptSignature, UInt32(timeoutBlocks))),
      Vector(
        TransactionOutput(Satoshis(payoutSat), ScriptPubKey.fromAsmBytes(payoutScriptPubKey))
      ),
      UInt32.zero,
      TransactionWitness(Vector(witness))
    )

    private def attempt[A](context: String)(f: => A): Either[String, A] =
        Try(f).toEither.left.map(e => s"$context: $e")
}
