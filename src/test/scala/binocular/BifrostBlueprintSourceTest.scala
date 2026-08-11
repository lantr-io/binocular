package binocular

import binocular.watchtower.*

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

/** The confirm worker derives the completed-peg-outs trie validator on every startup, so it must be
  * able to load the ft-bifrost-bridge blueprint with NO sibling checkout on disk — a Docker image
  * and a systemd unit have none, and a failure there is a crash-loop, not a warning.
  *
  * ==Residual gap (read before trusting this suite)==
  * `src/main/resources/bifrost-plutus-min.json` is vendored BY HAND from another repository, and
  * binocular's CI has no ft-bifrost-bridge checkout. Cross-repo drift is therefore caught only:
  *   1. on a developer machine that has a sibling ft checkout;
  *   2. in any CI job that sets `BIFROST_PLUTUS_JSON` (where a missing file is a failure, not a
  *      skip — an operator who asked for the check must get it);
  *   3. indirectly, by `src/test/resources/ft-blueprint-pin.conf`, when someone re-vendors the
  *      resource without refreshing the recorded digests.
  *
  * Case 3 is the only one that runs unconditionally, and it detects a careless re-vendor, not a
  * stale one: an ft build that moves ahead while nobody touches the vendored copy stays invisible
  * here. Closing that would need binocular's CI to check out ft, which is a separate repository
  * whose access credentials cannot be verified from this side, so `.github/workflows/ci.yml`
  * deliberately does NOT do it. Anyone wiring up such a job should set `BIFROST_PLUTUS_JSON` and
  * let case 2 do the work.
  */
class BifrostBlueprintSourceTest extends AnyFunSuite {

    private val titles = List(
      "bitcoin/config.config.mint",
      "bitcoin/bridged_token.bridged_token.mint",
      "bitcoin/completed_peg_ins_merkle_tree.completed_peg_ins_merkle_tree_validator.mint",
      "bitcoin/completed_peg_outs_merkle_tree.completed_peg_outs_merkle_tree_validator.mint",
      "bitcoin/peg_in.peg_in_validator.mint",
      "bitcoin/peg_out.peg_out_validator.withdraw",
      // Bridge genesis (WI-068) derives the federation policies from these: the registry, the ban
      // list parameterized by it, treasury_info, and the three fault verifiers spo_bans authorizes.
      "bitcoin/spos_registry.spo_registry.mint",
      "bitcoin/spo_bans.spo_bans.mint",
      "bitcoin/treasury.treasury_info.mint",
      "bitcoin/fault_verifier_round1.fault_verifier_round1.mint",
      "bitcoin/fault_verifier_round2.fault_verifier_round2.mint",
      "bitcoin/fault_verifier_equivocation.fault_verifier_equivocation.mint"
    )

    private val PinFile = "src/test/resources/ft-blueprint-pin.conf"

    private lazy val pinRecord = ConfigFactory.parseResources("ft-blueprint-pin.conf")

    private def recordedPlutusJsonSha: String = pinRecord.getString("plutus-json-sha256")

    /** Validators the vendored copy deliberately keeps BEHIND ft, title -> reason. */
    private lazy val heldBack: Map[String, String] = {
        val obj = pinRecord.getObject("held-back")
        obj.keySet.toArray.map(k => k.toString -> obj.get(k.toString).unwrapped.toString).toMap
    }

    /** Looked up through the `ConfigObject` map, not by path: the titles contain dots. */
    private def recordedCompiledCodeSha(title: String): String = {
        val obj = pinRecord.getObject("compiled-code-sha256")
        val v = obj.get(title)
        assert(v != null, s"$PinFile has no compiled-code-sha256 entry for $title")
        v.unwrapped.toString
    }

    private def sha256(bytes: Array[Byte]): String =
        MessageDigest.getInstance("SHA-256").digest(bytes).map("%02x".format(_)).mkString

    private def sha256(s: String): String = sha256(s.getBytes("US-ASCII"))

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

    /** Where to find an ft checkout's blueprint, and what it means when there is none.
      *
      *   - `Right(path)` — compare against this file.
      *   - `Left(Some(msg))` — `BIFROST_PLUTUS_JSON` is set but unreadable. That is an operator
      *     asking for the cross-repo check and not getting it, so it MUST fail. Falling through to
      *     a sibling path here would answer a question nobody asked.
      *   - `Left(None)` — nothing to compare against; cancel. CI and release builds have no
      *     checkout, and the vendored resource exists precisely so they do not need one.
      */
    private def ftBlueprint: Either[Option[String], Path] =
        sys.env.get("BIFROST_PLUTUS_JSON").map(_.trim).filter(_.nonEmpty) match {
            case Some(raw) =>
                val p = Paths.get(raw)
                if Files.isReadable(p) then Right(p)
                else
                    Left(
                      Some(
                        s"BIFROST_PLUTUS_JSON=$raw is set but no readable file is there. The " +
                            "cross-repo blueprint check was requested and cannot run — point the " +
                            "variable at an ft-bifrost-bridge onchain/plutus.json or unset it."
                      )
                    )
            case None =>
                List(
                  "../ft-bifrost-bridge/onchain/plutus.json",
                  "../../ft-bifrost-bridge/onchain/plutus.json",
                  "../../FluidTokens/ft-bifrost-bridge/onchain/plutus.json"
                ).map(Paths.get(_)).find(Files.isReadable).toRight(None)
        }

    private val NoCheckout =
        "no ft-bifrost-bridge checkout found (set BIFROST_PLUTUS_JSON to enable this check)"

    /** The one cross-repo guard that needs no ft checkout.
      *
      * It cannot see ft moving ahead of us — only a re-vendor that forgot the record. That is worth
      * having anyway: re-vendoring is exactly when the pins in `BifrostContractsTest` must move
      * too, and this test refuses to let the resource change quietly.
      */
    test("the vendored blueprint matches the digests recorded in ft-blueprint-pin.conf") {
        val packaged = BifrostBlueprint.packaged
        val drifted =
            titles.filter(t => sha256(packaged.compiledCode(t)) != recordedCompiledCodeSha(t))
        assert(
          drifted.isEmpty,
          s"the vendored blueprint changed without updating $PinFile — re-vendor from ft and " +
              s"refresh the record in the same commit. Drifted: ${drifted.mkString(", ")}"
        )
    }

    /** The freshness guard the policy-id pins are NOT.
      *
      * The pins compute their hashes from the vendored resource, so they lock it against itself; a
      * copy that has fallen behind ft keeps them green while every transaction built against it is
      * rejected on-chain. That is not hypothetical — it is how a stale `peg_out` survived the
      * rev-5.1 validator rewrite.
      */
    test("the vendored blueprint matches a sibling ft checkout, when one is present") {
        ftBlueprint match {
            case Left(Some(msg)) => fail(msg)
            case Left(None)      => cancel(NoCheckout)
            case Right(path) =>
                val ft = BifrostBlueprint.fromFile(path.toString)
                val packaged = BifrostBlueprint.packaged
                val stale = titles
                    .filter(t => ft.compiledCode(t) != packaged.compiledCode(t))
                    .filterNot(heldBack.contains)
                assert(
                  stale.isEmpty,
                  s"src/main/resources/bifrost-plutus-min.json has fallen behind $path for: " +
                      s"${stale.mkString(", ")}. Copy the compiledCode across and move the " +
                      "affected pins in BifrostContractsTest in the same commit."
                )
                // A hold-back is a promise to come back. Once ft and the vendored copy agree the
                // exception is not merely redundant, it is a lie the next reader will believe --
                // so it fails here rather than quietly outliving its reason.
                val caughtUp =
                    heldBack.keys.filter(t => ft.compiledCode(t) == packaged.compiledCode(t))
                assert(
                  caughtUp.isEmpty,
                  s"$PinFile holds these back but they already match $path: " +
                      s"${caughtUp.mkString(", ")}. Delete the held-back entries."
                )
        }
    }

    /** Whole-file identity, not just the six tracked validators.
      *
      * The test above only compares `compiledCode`, so an ft rebuild that changes a schema, a
      * title, or a validator binocular does not use slips past it. Pinning the file digest makes
      * every ft regeneration a visible, deliberate refresh of the record.
      */
    test("ft's plutus.json is the exact file the vendored blueprint was cut from") {
        ftBlueprint match {
            case Left(Some(msg)) => fail(msg)
            case Left(None)      => cancel(NoCheckout)
            case Right(path) =>
                val actual = sha256(Files.readAllBytes(path))
                if actual != recordedPlutusJsonSha then {
                    val ft = BifrostBlueprint.fromFile(path.toString)
                    val packaged = BifrostBlueprint.packaged
                    val drifted = titles
                        .filter(t => ft.compiledCode(t) != packaged.compiledCode(t))
                        .filterNot(heldBack.contains)
                    if drifted.isEmpty then
                        fail(
                          s"ft's plutus.json was regenerated but no tracked validator changed — " +
                              s"refresh plutus-json-sha256 in $PinFile (recorded " +
                              s"$recordedPlutusJsonSha, $path is now $actual)"
                        )
                    else
                        fail(
                          s"ft's plutus.json changed and these tracked validators drifted: " +
                              s"${drifted.mkString(", ")}. Re-vendor " +
                              s"src/main/resources/bifrost-plutus-min.json from $path, refresh " +
                              s"$PinFile, and move the pins in BifrostContractsTest in the same " +
                              "commit."
                        )
                }
        }
    }
}
