package binocular

import binocular.bitcoin.Bip322

import org.bitcoins.crypto.{ECPrivateKey, SchnorrDigitalSignature, SchnorrPublicKey}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

/** BIP-322 simple (Taproot key-path) — cross-checked against the validated pure-Python reference and
  * the Aiken `bifrost/bip322` module (tag `"BIP0322-signed-message"`). The Aiken side additionally
  * verifies a real UniSat signature on-chain (`bip322_unisat_vector`), so a matching sighash here
  * means signatures produced by `signKeypath` verify in `peg_in.ak`.
  */
class Bip322Spec extends AnyFunSuite {

    private def ascii(s: String): ByteVector = ByteVector(s.getBytes("US-ASCII"))

    test("keypath sighash matches the python/Aiken reference (message 'hello', Q=0x11*32)") {
        val sighash = Bip322.keypathSighash(ascii("hello"), ByteVector.fromValidHex("11" * 32))
        assert(
          sighash.toHex == "b31652fca4ab51235384ade30e3cfe3b5bee8364eced7251aa5d8fccd38aaea0"
        )
    }

    test("signKeypath: output key = taproot tweak, and the signature verifies against it") {
        val priv = ECPrivateKey.fromBytes(ByteVector.fromValidHex("00" * 31 + "03")) // d = 3
        val msg = ascii("BFR-mint-v1:" + "ab" * 32)
        val (q, sig) = Bip322.signKeypath(priv, msg)

        // Q must equal the deposit builder's key-path output key for d=3 (else it won't match the beacon).
        assert(q.toHex == "418c46636d9e1a683f58e35b42336e776fdcc3b2d4e39e7a0bf1ab0716e3c5fa")
        assert(sig.length == 64)

        // The signature must verify against Q over the BIP-322 sighash (what peg_in.ak checks).
        val ok = SchnorrPublicKey
            .fromBytes(q)
            .verify(Bip322.keypathSighash(msg, q), SchnorrDigitalSignature.fromBytes(sig))
        assert(ok, "BIP-322 signature must verify against the output key")
    }
}
