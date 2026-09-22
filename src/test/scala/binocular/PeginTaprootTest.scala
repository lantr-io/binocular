package binocular

import binocular.bitcoin.{PeginTree, PeginTreeParams, Taproot}

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.script.{TaprootScriptPath, TapscriptControlBlock}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

/** Peg-in Taproot tree – shared test vectors.
  *
  * The vectors in `src/test/resources/fixtures/taproot-pegin-vectors.json` are produced by
  * `scripts/gen-taproot-vectors.py`, which follows `documentation/pegin_deposit.py` in
  * `ft-bifrost-bridge`, the pure-Python reference for `documentation/bitcoin_tx_construction.md`
  * §1. Run it with `--check` to confirm every value still regenerates. A depositor, a watchtower
  * and an SPO each rebuild the same address from the published parameters, so a disagreement here
  * is a deposit nobody can sweep and nobody can refund.
  */
class PeginTaprootTest extends AnyFunSuite {

    private def hex(s: String): ByteVector = ByteVector.fromValidHex(s)

    private val vectors = TaprootVectors.tuples

    private def paramsOf(v: ujson.Value): PeginTreeParams = PeginTreeParams(
      y51 = hex(v("y51").str),
      yFederation = hex(v("y_federation").str),
      federationCsvBlocks = v("federation_csv_blocks").num.toInt,
      peginRefundTimeoutBlocks = v("pegin_refund_timeout_blocks").num.toInt
    )

    private def treeOf(v: ujson.Value): PeginTree =
        Taproot.peginTree(paramsOf(v), hex(v("q_auth").str)) match
            case Right(tree) => tree
            case Left(err)   => fail(s"${v("name").str}: peginTree returned Left($err)")

    private def network(hrp: String): NetworkParameters = hrp match
        case "bc"   => MainNet
        case "bcrt" => RegTest
        case _      => TestNet3

    test("output key, scriptPubKey and address match every tuple in taproot-pegin-vectors.json") {
        assert(vectors.size == 4, "the fixture must carry four tuples")
        for v <- vectors do
            val name = v("name").str
            val spk = treeOf(v).scriptPubKey
            withClue(s"$name: ") {
                assert(spk.pubKey.bytes.toHex == v("output_key").str)
                assert(spk.asmBytes.toHex == v("script_pub_key").str)
                assert(
                  Bech32mAddress(spk, network(v("address_human_readable_part").str)).value ==
                      v("address").str
                )
            }
    }

    test("the TapLeaf hash of each leaf matches every tuple") {
        // Without this the refund's signature is checked only against a digest computed the same
        // way it was signed, so a wrong tag or a missing compact-size prefix would pass unseen.
        for v <- vectors do
            withClue(s"${v("name").str}: ") {
                assert(
                  Taproot.leafHash(hex(v("federation_leaf").str)).hex == v(
                    "federation_leaf_hash"
                  ).str
                )
                assert(Taproot.leafHash(hex(v("refund_leaf").str)).hex == v("refund_leaf_hash").str)
            }
    }

    test("the refund leaf and its control block match every tuple, and open the deposit") {
        // TRF-78: the tree has exactly two leaves, so the merkle path to the refund leaf is the
        // single sibling hash of the federation leaf.
        for v <- vectors do
            val tree = treeOf(v)
            withClue(s"${v("name").str}: ") {
                assert(tree.refundLeaf.toHex == v("refund_leaf").str)
                assert(tree.refundControlBlock.toHex == v("refund_control_block").str)
                assert(
                  TaprootScriptPath.verifyTaprootCommitment(
                    TapscriptControlBlock.fromBytes(tree.refundControlBlock),
                    tree.scriptPubKey,
                    Taproot.leafHash(tree.refundLeaf)
                  )
                )
            }
    }

    test("the two leaves are <blocks> OP_CSV OP_DROP <key> OP_CHECKSIG, as every tuple pins") {
        for v <- vectors do
            val p = paramsOf(v)
            withClue(s"${v("name").str}: ") {
                assert(
                  Taproot.csvChecksigLeaf(p.federationCsvBlocks, p.yFederation).toHex ==
                      v("federation_leaf").str
                )
                assert(
                  Taproot.csvChecksigLeaf(p.peginRefundTimeoutBlocks, hex(v("q_auth").str)).toHex ==
                      v("refund_leaf").str
                )
            }
    }

    test(
      "scriptNum push is minimal: 1 -> 51, 16 -> 60, 17 -> 0111, 127 -> 017f, " +
          "128 -> 028000, 144 -> 029000, 720 -> 02d002"
    ) {
        val expected = Seq(
          1 -> "51",
          16 -> "60",
          17 -> "0111",
          127 -> "017f",
          128 -> "028000",
          144 -> "029000",
          720 -> "02d002"
        )
        for (n, encoded) <- expected do
            withClue(s"scriptNumPush($n): ")(assert(Taproot.scriptNumPush(n).toHex == encoded))
    }

    test("a tuple whose timeouts are at or below 16 pushes them as OP_5 and OP_16") {
        // 144 and 720 are ALWAYS length-prefixed, so no other tuple can tell a minimal push from a
        // non-minimal one. 5 is OP_5 (0x55), not 0x0105. Getting that wrong is a different leaf,
        // a different output key and a different address, with no error anywhere to say so.
        // `documentation/pegin_deposit.py` has a `_pushnum` that is NOT minimal below 17.
        val v = TaprootVectors.tuple("tuple_d_minimal_script_numbers")
        val p = paramsOf(v)
        assert(p.federationCsvBlocks == 5 && p.peginRefundTimeoutBlocks == 16)

        // OP_5 OP_CSV OP_DROP PUSH32, then OP_16 OP_CSV OP_DROP PUSH32; 37 bytes each, not 38.
        val federation = Taproot.csvChecksigLeaf(5, p.yFederation)
        val refund = Taproot.csvChecksigLeaf(16, hex(v("q_auth").str))
        assert(federation.take(4).toHex == "55b27520" && federation.length == 37)
        assert(refund.take(4).toHex == "60b27520" && refund.length == 37)

        // Only the two timeouts differ from tuple A, and a different leaf is a different address.
        val a = TaprootVectors.tupleA
        for field <- Seq("y51", "y_federation", "q_auth") do assert(v(field).str == a(field).str)
        assert(
          treeOf(v).scriptPubKey.pubKey != treeOf(a).scriptPubKey.pubKey,
          "different timeouts must move the output key"
        )
    }

    test("refund timeout not above federation_csv_blocks is refused") {
        val v = vectors.head
        val base = paramsOf(v)
        val qAuth = hex(v("q_auth").str)

        for timeout <- Seq(0, 100, 144) do
            val bad = base.copy(federationCsvBlocks = 144, peginRefundTimeoutBlocks = timeout)
            withClue(s"validate($timeout): ")(assert(bad.validate.isLeft))
            withClue(s"peginTree($timeout): ")(assert(Taproot.peginTree(bad, qAuth).isLeft))

        val good = base.copy(federationCsvBlocks = 144, peginRefundTimeoutBlocks = 145)
        assert(good.validate.isRight, "a timeout one block above the CSV delay is allowed")
        assert(Taproot.peginTree(good, qAuth).isRight)
    }

    test("off-curve depositor and federation keys are refused before an address exists") {
        val params = paramsOf(vectors.head)
        assert(Taproot.peginTree(params, hex("11" * 32)).isLeft)
        assert(
          Taproot
              .peginTree(params.copy(yFederation = hex("11" * 32)), hex(vectors.head("q_auth").str))
              .isLeft
        )
    }
}
