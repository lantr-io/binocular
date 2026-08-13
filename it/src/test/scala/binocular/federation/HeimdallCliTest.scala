package binocular.federation

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** `frost-treasury` is the seam genesis hangs on, and it needs no chain — so it is tested here,
  * in seconds, rather than discovered inside a ten-minute scenario.
  */
class HeimdallCliTest extends AnyFunSuite {

    /** The 2-of-3 key `scripts/dkz/demo-spo-{1,2,3}.sh` document three heimdall instances
      * converging on over HTTP. Pinned here because the whole genesis order depends on the
      * one-process derivation agreeing with the ceremony: publish `y_federation` first, let the
      * roster re-derive it later. If heimdall ever changes the demo seed or the derivation, this
      * fails HERE with the two keys side by side, instead of as a TM whose treasury input will
      * not verify.
      */
    private val DkzGroupKey =
        "b1e15a532a4e816ec75af608256b0808e36fb7d22560605178850885e53f2854"

    /** A minimal 2-of-3 TOML. Only `demo.{min,max}_signers` feed the derivation. */
    private lazy val config: os.Path = {
        val dir = os.temp.dir(prefix = "frost-treasury-")
        val f = dir / "heimdall.toml"
        os.write(
          f,
          """[demo]
            |min_signers = 2
            |max_signers = 3
            |
            |[bitcoin]
            |network = "regtest"
            |""".stripMargin
        )
        f
    }

    test("frost-treasury derives the same 2-of-3 key the HTTP DKG converges on") {
        val ft = HeimdallCli.frostTreasury(config, federationCsvBlocks = 144)
        assert(ft.groupKey.toHex == DkzGroupKey)
        assert(ft.groupKey.bytes.length == 32, "x-only keys are 32 bytes")
    }

    test("with no --y-federation the leaf key collapses onto Y_51, the genesis tree") {
        val ft = HeimdallCli.frostTreasury(config, federationCsvBlocks = 144)
        // The parser must also drop the "(defaulted to Y_51 — genesis tree)" note heimdall
        // appends to that line; a value carrying it would fail to hex-decode.
        assert(ft.yFederation == ft.groupKey)
    }

    test("a different y_federation yields a different treasury address") {
        // The address is a function of BOTH keys. This is the property WI-074/WI-081 exist to
        // protect: a wrong y_federation gives a well-formed P2TR that holds nothing, so the
        // scenario must be able to tell the two trees apart.
        val genesis = HeimdallCli.frostTreasury(config, federationCsvBlocks = 144)

        // A second REAL key, derived rather than invented: an arbitrary 32 bytes is not an
        // x-only pubkey (it has to be a curve point's x-coordinate) and heimdall rightly
        // refuses it. A 2-of-4 roster gives a different group key with no magic constant.
        val fourSigners = os.temp.dir(prefix = "frost-treasury-4-") / "heimdall.toml"
        os.write(
          fourSigners,
          """[demo]
            |min_signers = 2
            |max_signers = 4
            |
            |[bitcoin]
            |network = "regtest"
            |""".stripMargin
        )
        val otherKey = HeimdallCli.frostTreasury(fourSigners, federationCsvBlocks = 144).groupKey
        assert(otherKey != genesis.groupKey, "a 2-of-4 roster must derive a different Y_51")

        val other =
            HeimdallCli.frostTreasury(config, federationCsvBlocks = 144, yFederation = Some(otherKey))
        assert(other.groupKey == genesis.groupKey, "Y_51 does not depend on the leaf key")
        assert(other.yFederation == otherKey)
        assert(other.treasuryAddress != genesis.treasuryAddress)
    }

    test("the CSV delay is an input to the address too") {
        val a = HeimdallCli.frostTreasury(config, federationCsvBlocks = 144)
        val b = HeimdallCli.frostTreasury(config, federationCsvBlocks = 145)
        assert(a.treasuryAddress != b.treasuryAddress)
    }

    test("the scriptPubKey is the P2TR encoding of the address") {
        val ft = HeimdallCli.frostTreasury(config, federationCsvBlocks = 144)
        // 5120 = OP_1 PUSH32, the witness-v1 program. Genesis funds this scriptPubKey, so a
        // silently different shape would send the treasury somewhere unspendable.
        assert(ft.scriptPubKey.startsWith("5120"), s"not a P2TR spk: ${ft.scriptPubKey}")
        assert(ft.scriptPubKey.length == 4 + 64)
    }
}
