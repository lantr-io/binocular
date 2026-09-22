package binocular

import scodec.bits.ByteVector

/** Reader for `src/test/resources/fixtures/taproot-pegin-vectors.json` (spec TRF-97).
  *
  * The fixture carries two kinds of vector, and every suite that needs one reads it from here:
  *   - `vectors`, the peg-in Taproot TUPLES: leaves, leaf hashes, merkle root, tweak, output key,
  *     control blocks and address, for four sets of parameters;
  *   - `bip322`, the BIP-322 "simple" virtual transactions: the `to_spend` txid and the key-path
  *     digest `to_sign` signs, for one message and one Taproot output key.
  *
  * Why a file and not literals: TRF-97 requires every derivation vector to be DATA, because the
  * same numbers must later prove that the Scala and the TypeScript derivations agree. A literal
  * copied into a Scala suite is not something a TypeScript test can read.
  *
  * `scripts/gen-taproot-vectors.py` regenerates the whole file from the two seed private keys, so
  * every number here can be reproduced rather than trusted.
  *
  * The file is read from the working directory, not from the classpath, so an edit to it is visible
  * to the next test run without a `cleanFull`.
  */
object TaprootVectors {

    val file: os.Path =
        os.pwd / "src" / "test" / "resources" / "fixtures" / "taproot-pegin-vectors.json"

    private val root: ujson.Value = ujson.read(os.read(file))

    /** Every peg-in Taproot tuple, in fixture order. */
    def tuples: Seq[ujson.Value] = root("vectors").arr.toSeq

    def tuple(name: String): ujson.Value =
        tuples
            .find(_("name").str == name)
            .getOrElse(throw IllegalArgumentException(s"$name is missing from $file"))

    /** The tuple the deposit tests build under: parity 0, 144 and 720 blocks. */
    def tupleA: ujson.Value = tuple("tuple_a_parity_zero")

    /** The BIP-322 "simple" anchors: the virtual `to_spend` txid and the key-path digest, for one
      * message and one Taproot output key.
      */
    def bip322: ujson.Value = root("bip322")

    /** A hex field, as bytes. */
    def hex(value: ujson.Value): ByteVector = ByteVector.fromValidHex(value.str)
}
