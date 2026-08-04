package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files

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
}
