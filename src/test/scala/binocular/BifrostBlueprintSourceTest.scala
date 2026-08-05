package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Path, Paths}

/** The confirm worker derives the completed-peg-outs trie validator on every startup, so it must be
  * able to load the ft-bifrost-bridge blueprint with NO sibling checkout on disk — a Docker image
  * and a systemd unit have none, and a failure there is a crash-loop, not a warning.
  */
class BifrostBlueprintSourceTest extends AnyFunSuite {

    private val titles = List(
      "bitcoin/config.config.mint",
      "bitcoin/bridged_token.bridged_token.mint",
      "bitcoin/completed_peg_ins_merkle_tree.completed_peg_ins_merkle_tree_validator.mint",
      "bitcoin/completed_peg_outs_merkle_tree.completed_peg_outs_merkle_tree_validator.mint",
      "bitcoin/peg_in.peg_in_validator.mint",
      "bitcoin/peg_out.peg_out_validator.withdraw"
    )

    test("the packaged blueprint resource is on the classpath") {
        assert(BifrostBlueprint.packaged != null)
    }

    test("the packaged blueprint has every validator binocular applies parameters to") {
        val bp = BifrostBlueprint.packaged
        titles.foreach(t => assert(bp.compiledCode(t).nonEmpty, s"missing $t"))
    }

    test("resolve falls back to the packaged resource when the path does not exist") {
        val (bp, source) = BifrostBlueprint.resolve("/no/such/plutus.json")
        assert(source.contains(BifrostBlueprint.PackagedResource))
        titles.foreach(t => assert(bp.compiledCode(t).nonEmpty))
    }

    test("resolve falls back to the packaged resource for an empty path") {
        assert(BifrostBlueprint.resolve("")._2.contains(BifrostBlueprint.PackagedResource))
        assert(BifrostBlueprint.resolve("   ")._2.contains(BifrostBlueprint.PackagedResource))
    }

    // The override must still win, or a developer editing the Aiken validators would silently keep
    // testing against the vendored copy.
    test("resolve prefers an existing on-disk file over the packaged resource") {
        val tmp = Files.createTempFile("bifrost-plutus", ".json")
        try {
            Files.writeString(
              tmp,
              """{"validators":[{"title":"bitcoin/config.config.mint","compiledCode":"deadbeef"}]}"""
            )
            val (bp, source) = BifrostBlueprint.resolve(tmp.toString)
            assert(source == tmp.toString)
            assert(bp.compiledCode("bitcoin/config.config.mint") == "deadbeef")
        } finally Files.deleteIfExists(tmp)
    }

    /** Candidate locations of a sibling ft-bifrost-bridge checkout's blueprint, most explicit
      * first.
      */
    private def ftBlueprint: Option[Path] =
        (sys.env.get("BIFROST_PLUTUS_JSON").toList ++ List(
          "../ft-bifrost-bridge/onchain/plutus.json",
          "../../ft-bifrost-bridge/onchain/plutus.json",
          "../../FluidTokens/ft-bifrost-bridge/onchain/plutus.json"
        )).map(Paths.get(_)).find(Files.isReadable)

    /** The freshness guard the policy-id pins are NOT.
      *
      * The pins compute their hashes from the vendored resource, so they lock it against itself; a
      * copy that has fallen behind ft keeps them green while every transaction built against it is
      * rejected on-chain. That is not hypothetical — it is how a stale `peg_out` survived the
      * rev-5.1 validator rewrite.
      *
      * Skipped (not failed) when no ft checkout is present: CI and release builds have none, and
      * the vendored resource exists precisely so they do not need one.
      */
    test("the vendored blueprint matches a sibling ft checkout, when one is present") {
        ftBlueprint match {
            case None =>
                cancel(
                  "no ft-bifrost-bridge checkout found (set BIFROST_PLUTUS_JSON to enable this check)"
                )
            case Some(path) =>
                val ft = BifrostBlueprint.fromFile(path.toString)
                val packaged = BifrostBlueprint.packaged
                val stale = titles.filter(t => ft.compiledCode(t) != packaged.compiledCode(t))
                assert(
                  stale.isEmpty,
                  s"src/main/resources/bifrost-plutus-min.json has fallen behind $path for: " +
                      s"${stale.mkString(", ")}. Copy the compiledCode across and move the " +
                      "affected pins in BifrostContractsTest in the same commit."
                )
        }
    }
}
