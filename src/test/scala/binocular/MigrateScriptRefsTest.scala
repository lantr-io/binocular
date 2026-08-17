package binocular

import binocular.cli.CommandHelpers
import binocular.cli.commands.MigrateScriptRefsCommand
import binocular.cli.commands.MigrateScriptRefsCommand.ScriptResolution

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, Script, ScriptRef, Value}
import scalus.uplc.builtin.ByteString
import scalus.uplc.{Constant, Program, Term}

/** Pins the pure halves of `migrate-script-refs`: the script-bytes round trip that the on-chain
  * move depends on (a rebuilt script MUST hash back to the hash the ref UTxO declares, or the
  * migration would park the wrong bytes at the holding address), the shape of the recreated output,
  * and the "the backend's `reference_script_hash` is broken" guard. No network: every case here is
  * a pure function.
  */
class MigrateScriptRefsTest extends AnyFunSuite {

    /** The cheapest possible real PlutusV3 script: `(program 1.1.0 (con unit ()))`. Any compiled
      * contract would do; this one needs no blueprint and no parameter application, and it
      * exercises exactly the same `flat -> CBOR -> blake2b_224(0x03 ++ bytes)` path a 13 KB bridge
      * script does.
      */
    private val program = Program((1, 1, 0), Term.Const(Constant.Unit))
    private val original: Script.PlutusV3 = Script.PlutusV3(program)

    /** The preprod sponsor wallet base address (same fixture as RefScriptDiscoveryTest). */
    private val sponsor = Address.fromBech32(
      "addr_test1qzwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clq68fray"
    )
    private val holding = CommandHelpers.refScriptHoldingAddress(sponsor.getNetwork.get, sponsor)

    test("script bytes -> Script.PlutusV3 round-trips to the same script hash") {
        val rebuilt = Script.PlutusV3(original.script)
        assert(rebuilt.scriptHash == original.scriptHash)
    }

    test("migration output preserves value and scriptRef at the holding address") {
        val out = MigrateScriptRefsCommand.migratedOutput(
          holding,
          Value(Coin(50_000_000L)),
          original
        )
        assert(out.address == holding)
        assert(out.scriptRef.contains(ScriptRef(original)))
        assert(out.value.coin.value == 50_000_000L)
        assert(out.datumOption.isEmpty)
    }

    test("migration output carries the source UTxO's full value, not a fixed 50 ADA") {
        val out =
            MigrateScriptRefsCommand.migratedOutput(holding, Value(Coin(1_234_567L)), original)
        assert(out.value.coin.value == 1_234_567L)
    }

    test("resolveScript accepts the flat CBOR the script hash is taken over") {
        val resolved =
            MigrateScriptRefsCommand.resolveScript(
              "plutusV3",
              original.script.toHex,
              original.scriptHash
            )
        assert(resolved == ScriptResolution.Resolved(original))
    }

    test("resolveScript unwraps one extra CBOR bytestring layer (double-encoded backends)") {
        val resolved =
            MigrateScriptRefsCommand.resolveScript(
              "plutusV3",
              program.doubleCborHex,
              original.scriptHash
            )
        assert(resolved == ScriptResolution.Resolved(original))
        // and the unwrapped bytes really are the single-CBOR form the hash is taken over
        assert(
          MigrateScriptRefsCommand
              .cborUnwrapOnce(ByteString.fromHex(program.doubleCborHex))
              .contains(original.script)
        )
    }

    test("resolveScript fails loudly with both hashes when nothing hashes to the expected value") {
        val other = Script.PlutusV3(Program((1, 1, 0), Term.Const(Constant.Integer(BigInt(42)))))
        val resolved =
            MigrateScriptRefsCommand.resolveScript(
              "plutusV3",
              other.script.toHex,
              original.scriptHash
            )
        val reason = resolved match {
            case ScriptResolution.Failed(r) => r
            case other                      => fail(s"expected a loud failure, got $other")
        }
        assert(reason.contains(original.scriptHash.toHex)) // expected
        assert(reason.contains(other.scriptHash.toHex)) // what we actually rebuilt
    }

    test("resolveScript reports an unsupported script type instead of failing") {
        val resolved =
            MigrateScriptRefsCommand.resolveScript("timelock", "82008200", original.scriptHash)
        assert(resolved == ScriptResolution.Unsupported("timelock"))
    }

    test("resolveScript rejects a non-hex cbor payload") {
        assert(
          MigrateScriptRefsCommand
              .resolveScript("plutusV3", "not-hex", original.scriptHash)
              .isInstanceOf[ScriptResolution.Failed]
        )
    }

    /** Blockfrost `/addresses/{addr}/utxos` item, ADA-only unless `extraAsset` is set. */
    private def utxo(lovelace: Long, extraAsset: Boolean = false): ujson.Value = {
        val amounts = ujson.Arr(ujson.Obj("unit" -> "lovelace", "quantity" -> lovelace.toString))
        if extraAsset then
            amounts.arr += ujson.Obj("unit" -> ("aa" * 28 + "4e4654"), "quantity" -> "1")
        ujson.Obj("tx_hash" -> ("1" * 64), "output_index" -> 0, "amount" -> amounts)
    }

    test("refShapedUtxoCount counts ADA-only 50 ADA UTxOs — the reference-UTxO shape") {
        val items = Seq(
          utxo(50_000_000L),
          utxo(50_000_000L),
          utxo(9_500_000L), // ordinary wallet UTxO
          utxo(50_000_000L, extraAsset = true) // 50 ADA but carries a token: not a ref UTxO
        )
        assert(MigrateScriptRefsCommand.refShapedUtxoCount(items) == 2)
    }

    test("an empty scan next to an unreachable provider is fatal, never 'nothing to migrate'") {
        // A total backend outage empties the raw scan AND fails the provider query. Reading that as
        // a finished migration would be a false success — the one outcome this guard exists to stop.
        assert(
          MigrateScriptRefsCommand.emptyScanVerdict(Left("NetworkError(connection refused)")) ==
              MigrateScriptRefsCommand.EmptyScanVerdict.ProviderUnavailable(
                "NetworkError(connection refused)"
              )
        )
    }

    test("an empty scan while the provider sees UTxOs is a blind scan, not an empty wallet") {
        assert(
          MigrateScriptRefsCommand.emptyScanVerdict(Right(7)) ==
              MigrateScriptRefsCommand.EmptyScanVerdict.ScanBlind(7)
        )
    }

    test("an empty scan the provider agrees with is a genuinely empty wallet") {
        assert(
          MigrateScriptRefsCommand.emptyScanVerdict(Right(0)) ==
              MigrateScriptRefsCommand.EmptyScanVerdict.WalletEmpty
        )
    }

    test("refShapedUtxoCount tolerates a numeric quantity (Yaci-style JSON)") {
        val item = ujson.Obj(
          "tx_hash" -> ("1" * 64),
          "output_index" -> 0,
          "amount" -> ujson.Arr(ujson.Obj("unit" -> "lovelace", "quantity" -> 50000000))
        )
        assert(MigrateScriptRefsCommand.refShapedUtxoCount(Seq(item)) == 1)
    }
}
