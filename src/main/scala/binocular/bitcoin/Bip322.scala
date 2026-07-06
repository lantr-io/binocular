package binocular.bitcoin

import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, FieldElement}
import scodec.bits.ByteVector

/** BIP-322 "simple" message signing — Taproot (P2TR) key-path.
  *
  * Mirrors the on-chain reconstruction in `ft-bifrost-bridge` `lib/bifrost/bip322.ak`: a wallet's
  * `signMessage(msg, "bip322-simple")` for a Taproot address is a BIP-340 Schnorr signature over the
  * BIP-341 key-path sighash of a virtual `to_sign` tx that commits
  * `tagged_hash("BIP0322-signed-message", msg)`. The depositor authorization in `peg_in.ak`
  * verifies exactly this; `signKeypath` lets the toolchain produce such a signature from a WIF (so
  * the demo flow needs no browser wallet), and any wallet's bip322-simple signature works too.
  *
  * Scope: Taproot **key-path**, SIGHASH_DEFAULT, no annex. Tag is `"BIP0322-signed-message"` (what
  * bip322-js / UniSat use — NOT the spec text's `"BIP0322-signed"`).
  */
object Bip322 {

    private def b(h: String): ByteVector = ByteVector.fromValidHex(h)

    private def sha256(x: ByteVector): ByteVector = CryptoUtil.sha256(x).bytes

    /** tagged_hash(tag, msg) = SHA256( SHA256(tag) ++ SHA256(tag) ++ msg )  (BIP-340). */
    private def tagged(tag: String, msg: ByteVector): ByteVector = {
        val t = sha256(ByteVector(tag.getBytes("US-ASCII")))
        sha256(t ++ t ++ msg)
    }

    /** The 32-byte BIP-322 key-path sighash a bip322-simple signature signs, for `message` and the
      * 32-byte Taproot output key `outputKey`.
      */
    def keypathSighash(message: ByteVector, outputKey: ByteVector): ByteVector = {
        require(outputKey.length == 32, s"outputKey must be 32 bytes, got ${outputKey.length}")
        val messageHash = tagged("BIP0322-signed-message", message)
        // to_spend (legacy serialization): commits messageHash in scriptSig, the address spk in vout 0.
        val toSpend =
            b("00000000") ++ // nVersion = 0
                b("01") ++ // vin count
                b("00" * 32) ++ // prevout hash
                b("ffffffff") ++ // prevout n
                b("22") ++ b("0020") ++ messageHash ++ // scriptSig = OP_0 PUSH32 <messageHash>
                b("00000000") ++ // nSequence
                b("01") ++ // vout count
                b("0000000000000000") ++ // value 0
                b("22") ++ b("5120") ++ outputKey ++ // scriptPubKey = OP_1 PUSH32 <Q>
                b("00000000") // nLockTime
        val spendTxid = sha256(sha256(toSpend))
        // BIP-341 key-path sighash of to_sign (spends to_spend:0 → single OP_RETURN output).
        val shaPrevouts = sha256(spendTxid ++ b("00000000"))
        val shaAmounts = sha256(b("0000000000000000"))
        val shaScriptPubkeys = sha256(b("22") ++ b("5120") ++ outputKey)
        val shaSequences = sha256(b("00000000"))
        val shaOutputs = sha256(b("0000000000000000") ++ b("01") ++ b("6a"))
        val preimage =
            b("00") ++ // sighash epoch
                b("00") ++ // hash_type SIGHASH_DEFAULT
                b("00000000") ++ b("00000000") ++ // nVersion, nLockTime
                shaPrevouts ++ shaAmounts ++ shaScriptPubkeys ++ shaSequences ++ shaOutputs ++
                b("00") ++ // spend_type (key path, no annex)
                b("00000000") // input_index
        tagged("TapSighash", preimage)
    }

    /** Sign `message` with `priv` via BIP-322 simple (Taproot key-path). Returns
      * `(outputKey, signature)` — `outputKey` is the 32-byte Taproot output key the signature
      * verifies against (it must equal the depositor `user_source_chain_pub_key` in the PIR datum),
      * `signature` is the 64-byte Schnorr signature.
      */
    def signKeypath(priv: ECPrivateKey, message: ByteVector): (ByteVector, ByteVector) = {
        // even-y internal key d_even (so d_even·G = lift_x(internal x-only))
        val dEven = if priv.publicKey.bytes.head == 0x03.toByte then priv.negate else priv
        val internalXOnly = dEven.schnorrPublicKey.bytes
        val tweak = ECPrivateKey.fromFieldElement(FieldElement(tagged("TapTweak", internalXOnly)))
        val dTweaked = dEven.add(tweak)
        val outputKey = dTweaked.schnorrPublicKey.bytes
        val sig = dTweaked.schnorrSign(keypathSighash(message, outputKey)).bytes
        (outputKey, sig)
    }
}
