package binocular

import binocular.blueprint.{BlueprintGenerator, PinnedBlueprint}
import binocular.oracle.BitcoinContract

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Script
import scalus.uplc.builtin.ByteString

import scala.io.Source

/** Strategy A guarantees:
  *   1. frozen-guard — the frozen blueprint still pins the exact deployed hashes;
  *   2. round-trip — stored bytes hash to the stored hash, and the runtime loader reproduces it;
  *   3. drift — fresh compilation under the current toolchain matches the current-deps blueprint.
  */
class PinnedBlueprintTest extends AnyFunSuite {

    private def resourceJson(name: String): ujson.Value =
        ujson.read(Source.fromResource(name).mkString)

    private val frozen =
        BlueprintGenerator.parse(resourceJson("blueprints/binocular-blueprint-scalus-0.18.1-scala-3.3.7.json"))

    private def hashOf(title: String, paramsKey: String): String =
        frozen
            .find(e => e.title == title && e.paramsKey == paramsKey)
            .map(_.hash)
            .getOrElse(fail(s"no frozen entry for ($title, $paramsKey)"))

    test("frozen-guard: deployed hashes are pinned and never silently move") {
        // Deployed policies recorded in application-preprod.conf.
        assert(frozen.exists(_.hash == "758e8e7d3ffa043f3c89bf7c38b69c2508a7d5610ebcf03aaafb944d")) // oracle
        assert(frozen.exists(_.hash == "eafdc4d9733275d3e06cfe5fe54a13fff3ba5baa8d65636260537907")) // tmtx
        assert(frozen.exists(_.hash == "412491a7be58276e165265e7e5c43e20cdb7091f6bd10d364f16b54f")) // TM
    }

    test("round-trip: stored bytes hash to the stored hash") {
        frozen.foreach { e =>
            val recomputed = Script.PlutusV3(ByteString.fromHex(e.compiledCode)).scriptHash.toHex
            assert(recomputed == e.hash, s"hash mismatch for ${e.title}/${e.paramsKey}")
        }
    }

    test("loader: PinnedBlueprint.pinned returns the frozen bytes verbatim") {
        frozen.foreach { e =>
            val script = PinnedBlueprint.pinned(e.title, e.paramsKey)(fail("should not compile fresh"))
            assert(script.scriptHash.toHex == e.hash)
        }
    }

    test("BitcoinContract.script loads frozen bytes that equal a fresh compile (0.18.1)") {
        val preprod = BinocularConfig.load(Some("application-preprod.conf"))
        val params = preprod.oracle
            .toBitcoinValidatorParams(preprod.bitcoinNode.bitcoinNetwork)
            .getOrElse(fail("preprod oracle not configured"))
        // Pinned load (from the frozen blueprint) must reproduce the source-compiled hash exactly.
        assert(BitcoinContract.script(params).scriptHash.toHex ==
            BitcoinContract.makeContract(params).script.scriptHash.toHex)
        // And it is one of the pinned oracle entries (not a fresh-compile fallback).
        assert(frozen.exists(e => e.title == "oracle" && e.hash == BitcoinContract.script(params).scriptHash.toHex))
    }

    test("drift: fresh compilation matches the current-deps blueprint (fails on drift)") {
        val currentDepsFile =
            s"blueprints/${BlueprintGenerator.fileName(BuildInfo.scalusVersion, BuildInfo.scalaVersion)}"
        val committed = BlueprintGenerator.parse(resourceJson(currentDepsFile)).toSet

        val preprod = BinocularConfig.load(Some("application-preprod.conf"))
        val testnet4 = BinocularConfig.load(Some("application-testnet4.conf"))
        val regenerated = BlueprintGenerator
            .merge(BlueprintGenerator.entriesFor(preprod), BlueprintGenerator.entriesFor(testnet4))
            .toSet

        assert(regenerated == committed)
    }
}
