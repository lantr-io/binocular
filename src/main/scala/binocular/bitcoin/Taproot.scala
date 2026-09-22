package binocular.bitcoin

import org.bitcoins.core.protocol.script.{LeafVersion, ScriptPubKey, TapBranch, TapLeaf, TaprootScriptPubKey, TapscriptControlBlock}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.crypto.{Sha256Digest, XOnlyPubKey}
import scodec.bits.ByteVector

import scala.util.Try

/** Parameters of one peg-in Taproot tree.
  *
  * All four values are published by the bridge (`y_federation` and `federation_csv_blocks` live in
  * the Config datum, `pegin_refund_timeout_blocks` beside them, `y51` in the treasury state), so a
  * depositor, a watchtower and an SPO each rebuild the same address. They are INPUTS TO THE
  * ADDRESS: one wrong value yields a well-formed P2TR that the federation cannot sweep and the
  * depositor cannot refund.
  *
  * @param y51
  *   x-only FROST group key of the 51% quorum. It is the Taproot internal key, so the quorum sweeps
  *   by key path and reveals no script.
  * @param yFederation
  *   x-only federation key of the emergency-sweep leaf.
  * @param federationCsvBlocks
  *   relative timelock of the federation sweep leaf, in blocks.
  * @param peginRefundTimeoutBlocks
  *   relative timelock of the depositor refund leaf, in blocks.
  */
final case class PeginTreeParams(
    y51: ByteVector,
    yFederation: ByteVector,
    federationCsvBlocks: Int,
    peginRefundTimeoutBlocks: Int
) {

    /** `Right(this)` when the parameters can build a usable tree, `Left(reason)` otherwise.
      *
      * Why the timeout must be strictly above the CSV delay: the federation's recovery window has
      * to open BEFORE the depositor's refund window. If it did not, a depositor could take the
      * deposit back while the federation was still recovering it.
      */
    def validate: Either[String, PeginTreeParams] =
        if y51.length != 32 then Left(s"y51 must be 32 bytes, got ${y51.length}")
        else if yFederation.length != 32 then
            Left(s"yFederation must be 32 bytes, got ${yFederation.length}")
        else if federationCsvBlocks < 1 then
            Left(s"federationCsvBlocks must be at least 1, got $federationCsvBlocks")
        else if peginRefundTimeoutBlocks <= federationCsvBlocks then
            Left(
              s"peginRefundTimeoutBlocks ($peginRefundTimeoutBlocks) must be above " +
                  s"federationCsvBlocks ($federationCsvBlocks)"
            )
        else if peginRefundTimeoutBlocks > Taproot.MaxRelativeTimelockBlocks then
            Left(
              s"peginRefundTimeoutBlocks ($peginRefundTimeoutBlocks) must not exceed " +
                  s"${Taproot.MaxRelativeTimelockBlocks} (BIP-68 block count is 16 bits)"
            )
        else Right(this)
}

/** One derived peg-in Taproot tree: where a deposit is paid, and what opens it again.
  *
  * The two refund fields are the whole of what the depositor needs to spend the deposit back
  * through the refund leaf, and neither can be recovered from the address: a P2TR reveals nothing
  * of its tree until it is spent.
  *
  * @param scriptPubKey
  *   the peg-in output. The 51% quorum sweeps it by key path and reveals no script.
  * @param refundLeaf
  *   the refund leaf SCRIPT, `<timeout> OP_CSV OP_DROP <qAuth> OP_CHECKSIG`.
  * @param refundControlBlock
  *   `(0xc0 | parity of the output key) ‖ y51 ‖ federation leaf hash` (spec TRF-78). The tree has
  *   exactly two leaves, so the merkle path to the refund leaf is the federation leaf's hash.
  */
final case class PeginTree(
    scriptPubKey: TaprootScriptPubKey,
    refundLeaf: ByteVector,
    refundControlBlock: ByteVector
)

/** BIP-341 derivation for the peg-in Taproot tree.
  *
  * The tree has exactly two leaves (spec §Peg-in Taproot tree):
  *   - federation sweep: `<federationCsvBlocks> OP_CSV OP_DROP <yFederation> OP_CHECKSIG`
  *   - depositor refund: `<peginRefundTimeoutBlocks> OP_CSV OP_DROP <qAuth> OP_CHECKSIG`
  *
  * `qAuth` is the depositor's Taproot OUTPUT key, the same key the `BFR` beacon carries and the
  * same key the BIP-322 completion signature verifies against (see [[Bip322]]).
  *
  * Cross-checked against `documentation/pegin_deposit.py` in `ft-bifrost-bridge`; the vectors live
  * in `src/test/resources/fixtures/taproot-pegin-vectors.json`.
  */
object Taproot {

    /** BIP-68 encodes a relative timelock in blocks in 16 bits. */
    val MaxRelativeTimelockBlocks: Int = 0xffff

    /** The minimal script encoding that pushes `n`, as Bitcoin Core's `CScript::operator<<(int64)`
      * writes it.
      *
      * 0 becomes `OP_0`. 1 to 16 become `OP_1` to `OP_16`, a single byte `0x50 + n`. Larger values
      * become a length-prefixed little-endian CScriptNum, with a `0x00` sign byte appended when the
      * top byte would otherwise set the sign bit.
      */
    def scriptNumPush(n: Int): ByteVector = {
        require(n >= 0, s"scriptNumPush is defined for non-negative values, got $n")
        val number = BitcoinScriptUtil.minimalScriptNumberRepresentation(ScriptNumber(n.toLong))
        ByteVector.concat((BitcoinScriptUtil.calculatePushOp(number) :+ number).map(_.bytes))
    }

    /** `<blocks> OP_CSV OP_DROP <xOnlyKey> OP_CHECKSIG` – the one leaf shape the protocol uses, for
      * both the federation sweep and the depositor refund.
      */
    def csvChecksigLeaf(blocks: Int, xOnlyKey: ByteVector): ByteVector = {
        require(xOnlyKey.length == 32, s"xOnlyKey must be 32 bytes, got ${xOnlyKey.length}")
        scriptNumPush(blocks) ++
            ByteVector(0xb2.toByte) ++ // OP_CHECKSEQUENCEVERIFY
            ByteVector(0x75.toByte) ++ // OP_DROP
            ByteVector(0x20.toByte) ++ xOnlyKey ++ // push 32 bytes
            ByteVector(0xac.toByte) // OP_CHECKSIG
    }

    /** The peg-in tree for a depositor output key `qAuth`, or why it cannot be derived. */
    def peginTree(params: PeginTreeParams, qAuth: ByteVector): Either[String, PeginTree] =
        for
            valid <- params.validate
            _ <- Either.cond(qAuth.length == 32, (), s"qAuth must be 32 bytes, got ${qAuth.length}")
            tree <- Try {
                XOnlyPubKey.fromBytes(valid.yFederation)
                XOnlyPubKey.fromBytes(qAuth)
                val internalKey = XOnlyPubKey.fromBytes(valid.y51)
                val federation = leaf(csvChecksigLeaf(valid.federationCsvBlocks, valid.yFederation))
                val refundScript = csvChecksigLeaf(valid.peginRefundTimeoutBlocks, qAuth)
                val (parity, spk) = TaprootScriptPubKey.fromInternalKeyTapscriptTree(
                  internalKey,
                  TapBranch(federation, leaf(refundScript))
                )
                PeginTree(
                  scriptPubKey = spk,
                  refundLeaf = refundScript,
                  // The sibling of the refund leaf is the federation leaf, and the control block
                  // carries the parity of the OUTPUT key, not of the internal key.
                  refundControlBlock = TapscriptControlBlock(
                    LeafVersion.Tapscript,
                    internalKey,
                    parity,
                    Vector(federation.sha256)
                  ).bytes
                )
            }.toEither.left.map(e => s"peg-in tree derivation failed: ${e.getMessage}")
        yield tree

    /** The BIP-341 TapLeaf hash of `script` at leaf version 0xc0, which the script-path signature
      * hash commits to.
      */
    def leafHash(script: ByteVector): Sha256Digest = leaf(script).sha256

    private def leaf(script: ByteVector): TapLeaf =
        TapLeaf(LeafVersion.Tapscript, ScriptPubKey.fromAsmBytes(script))
}
