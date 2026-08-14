package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

import java.nio.file.{Files, Path, Paths}

/** Tests for the F3 bridge-contract hash computation in [[BifrostContracts]].
  *
  * The known-answer cases are REGRESSION LOCKS over the CIP-57 parameter-application encoding
  * (`Data.B` for byte params, `Data.I` for `index0`, the one-shot `OutputReference` as `Data`, and
  * param order). They are re-validated at the next `deploy-bridge` run. They use a trimmed
  * `plutus.json` (only the referenced validators' `compiledCode`), so the test runs without the
  * sibling ft-bifrost-bridge checkout.
  *
  * That trimmed blueprint now lives in `src/main/resources`, not `src/test/resources`: it is the
  * runtime DEFAULT source (see `BifrostBlueprint.packaged`) so a deployed image needs no sibling
  * checkout. These pins are therefore also its freshness guard — any silent edit to the shipped
  * resource moves a policy id and fails here.
  *
  * Refreshed 2026-07-20 (Route-1 alignment) to the current ft blueprint: the 17-field ConfigDatum
  * (upstream's `initial_btc_treasury_utxo` #11 + our tunables #12-16) changed `config.config`'s
  * compiledCode, which cascades through the config policy into every config-parameterized contract,
  * so all four policy pins moved. The min-json was also brought up to ft's current `compiledCode`
  * for the other validators (it had drifted behind upstream's own regenerated blueprint).
  *
  * Refreshed again for the peg-out trie v2 rewrite: the min-json now also carries `peg_out`, and
  * both peg-out-side validators changed parameter lists — `peg_out_validator` dropped
  * `oracle_policy_id` (2 params), `completed_peg_outs_merkle_tree_validator` swapped the config NFT
  * pair for `tm_nft_policy_id` (2 params).
  *
  * Refreshed again for the rev-5.4 eight-field Config datum (spec §Config datum, 2026-08): the
  * genesis full cast in `config.config` and the getter indexes in every reader moved, so the config
  * policy pin and everything parameterized by it cascaded. `peg_in_validator` also dropped its
  * `tm_nft_policy_id` param (3 params now). `completed_peg_outs_merkle_tree_validator` is WITHDRAWN
  * in the ft tree (replaced by `bridge-state.ak`); the min-json keeps its LAST published
  * compiledCode for the interim TM Confirm trie flow, so its pin did not move.
  *
  * Refreshed again for WI-090 (2026-08-14): ConfigDatum gained #12 `federation_one_shot`, and the
  * genesis full cast moved `config.config` once more, cascading through the config policy into all
  * five pins below. The min-json re-vendor also absorbed `treasury_info`'s pre-existing build
  * drift, which is why [[RebuildDrift]] could go empty in the same commit.
  */
class BifrostContractsTest extends AnyFunSuite {

    // The SAME object the runtime falls back to, so these pins lock what a deployed binary uses.
    private val blueprint = BifrostBlueprint.packaged

    // Exact inputs from the live deploy.
    private val oraclePolicy =
        ByteString.fromHex("7ba9aae06f9b3e9810bc469b9b6a4e60fcf0d405075f9427f8ddbe17")
    private val configAssetName = ByteString.fromString("BIFCFG") // 424946434647
    private val configRef =
        TxOutRef(
          TxId(
            ByteString.fromHex("231b92c928c2bac84280330881ad92084a2d616fab3c6a6321080fa0f29ad5a4")
          ),
          BigInt(0)
        )
    private val cpiRef =
        TxOutRef(
          TxId(
            ByteString.fromHex("6a6cbf274df3f0402bd48f7706e3cf1f39e15b3b7af465a55c233889f8785c53")
          ),
          BigInt(0)
        )

    private def hex(c: scalus.cardano.ledger.ScriptHash): String = c.toHex

    private def configContract =
        ConfigContract(blueprint, configRef.id.hash, configRef.idx)
    private def configPolicy = ByteString.fromArray(configContract.policyId.bytes)

    // --- known-answer (regression lock, re-validated at next deploy) ---
    //
    // All five moved together in the rev-5.4 federation reconciliation, and that is one cause, not
    // five. `config.ak`'s mint handler full-casts the genesis datum, so widening ConfigDatum to
    // fifteen fields moved the config script's own hash — which IS the config NFT policy id, and
    // that id is a parameter to every contract below. A different config policy is a different
    // bridge instance by construction (spec §Config UTxO), so the whole instance redeploys.

    test("config NFT policy matches the deployed value") {
        assert(
          hex(configContract.policyId) == "c4402d1ada1c5db4af451082f8ef2caf214097dd919148c5efe4c909"
        )
    }

    test("bridged_token policy matches the deployed value") {
        val bt = BridgedTokenContract(blueprint, configPolicy)
        assert(hex(bt.policyId) == "84c085c4fa43eec1de31a7f459fa12d18f857e4bb671bf0bb0ed665b")
    }

    test("completed-peg-ins policy + asset name match the Variant B rebuild") {
        // policyId regression lock over the Variant B rebuild. The asset name is now the constant
        // "CPI" (bytes 435049), independent of the one-shot ref and the compiledCode.
        val cpi = CompletedPegInsContract(blueprint, configPolicy, cpiRef)
        assert(hex(cpi.policyId) == "fa4698345161667fe72decbc075e2be8325f458069d2205455cc9026")
        assert(CompletedPegInsContract.assetName == ByteString.fromString("CPI"))
    }

    // The TM-NFT policy placeholder the completed-peg-outs pin below is computed against. Rev 5.4
    // removed it from peg_in's parameter list.
    private val tmNftPolicy =
        ByteString.fromHex("11111111111111111111111111111111111111111111111111111111")

    test("peg_in policy (= withdraw hash) is stable for the rev-5.4 3-param encoding") {
        // Rev 5.4 dropped the tm_nft_policy_id param: peg_in.ak reads the bridge-state singleton
        // through Config field 3 at runtime, so three params remain. PIRs minted under the old
        // policy are orphaned and must be re-minted under this one.
        //
        // Moved again (2026-08) when `utils.get_mpf_from_output` became a TYPED decode of the CPI
        // trie datum instead of a blind field-0 read. peg_in.ak is its only caller, so only this
        // hash moved; ConfigDatum field 5 must name the new one at deployment.
        val pegIn =
            PegInContract(blueprint, oraclePolicy, configPolicy)
        assert(hex(pegIn.policyId) == "aa767109f00f937fae6299315bae9a98de6a4406765f94fe3425b42c")
    }

    test("peg_out policy (= withdraw hash) is stable for the trie-v2 2-param encoding") {
        // Trie v2 dropped `oracle_policy_id`: peg_out.ak no longer parses the TM itself, it proves
        // membership in the completed-peg-outs trie the TM Confirm wrote. Two params now, so the
        // hash moved and ConfigDatum field 6 must be swapped by a config Update.
        //
        // Moved again for rev 5.1 (2026-08): the min-json's `peg_out` compiledCode was refreshed to
        // ft's current build, in which Complete dropped its `owner_auth` check (permissionless
        // cleanup), the datum became 4 fields, and the withdraw redeemer gained
        // `completed_peg_outs_ref_input_index`. The POR sweeper builds against exactly these bytes —
        // [[PegOutCompleteCekTest]] runs them — so the stale copy would have made every completion
        // fail on-chain. No other validator's compiledCode changed, so no other pin moved.
        val pegOut = PegOutContract(blueprint, configPolicy)
        assert(hex(pegOut.policyId) == "0fc35057187dccea740a3f6f5742e85ac89854eec3f8066b274eaa89")
    }

    // --- determinism + parameter-sensitivity ---

    test("hash computation is deterministic") {
        val a = ConfigContract(blueprint, configRef.id.hash, configRef.idx)
        val b = ConfigContract(blueprint, configRef.id.hash, configRef.idx)
        assert(a.policyId == b.policyId)
    }

    test("a different one-shot yields a different config policy") {
        val other = ConfigContract(blueprint, cpiRef.id.hash, BigInt(0))
        assert(other.policyId != configContract.policyId)
    }

    test("a different index yields a different config policy") {
        val other = ConfigContract(blueprint, configRef.id.hash, BigInt(1))
        assert(other.policyId != configContract.policyId)
    }

    test("completed-peg-ins asset name is the CPI constant") {
        assert(CompletedPegInsContract.assetName == ByteString.fromString("CPI"))
    }

    // --- rev-5.4 bridge-state singleton (spec [BSS-4], [BSS-5]) ---

    private val BridgeStateMintTitle = BridgeStateContract.ValidatorTitle
    private val BridgeStateSpendTitle = "bitcoin/bridge_state.bridge_state.spend"

    // The real first parameter: the TM NFT policy IS the TreasuryMovementValidator script hash.
    private val tmScriptHash = ByteString.fromArray(
      TreasuryMovementContract
          .script(oraclePolicy, configPolicy, configAssetName)
          .scriptHash
          .bytes
    )

    test("the vendored blueprint carries bridge_state mint and spend") {
        // `bridge-state.ak` replaces `completed-peg-outs-merkle-tree.ak` in ft rev 5.4. The
        // packaged min-json is the runtime DEFAULT source, so a deployed image can only derive the
        // singleton policy if both handler titles are vendored here.
        val codes = List(BridgeStateMintTitle, BridgeStateSpendTitle).map { title =>
            scala.util
                .Try(blueprint.compiledCode(title))
                .getOrElse(
                  fail(s"the vendored min-json has no $title – refresh it from ft's plutus.json")
                )
        }
        // All handlers of one Aiken validator share a single compiledCode.
        assert(codes.head.nonEmpty)
        assert(codes.head == codes.last)
    }

    test("bridge-state policy is stable for the (tm-policy, one-shot) encoding") {
        // Known-answer REGRESSION LOCK, the same kind every sibling contract above carries, and the
        // only bridge_state check that runs on CI (the ft comparison below cancels there). Computed
        // against the fixed 28-byte TM placeholder so it depends on the vendored `bridge_state`
        // compiledCode and the CIP-57 param encoding ALONE, not on binocular's TM validator.
        //
        // If this moves, someone re-vendored `bitcoin/bridge_state.bridge_state.mint` or edited the
        // min-json. Then the deployed image derives a policy the ConfigDatum does not name and every
        // TM Confirm is rejected on-chain, so re-check the vendoring before you move the pin.
        val bss = BridgeStateContract(blueprint, tmNftPolicy, cpiRef)
        assert(hex(bss.policyId) == "0b8626761055d00dd58c99646bd379b3881f15477852755a5d74c1f8")
        // Asset name is the constant "BSS" per [BSS-5], shared with the TM validator's mirror.
        assert(BridgeStateContract.assetName == ByteString.fromString("BSS"))
    }

    test("BridgeStateContract applies (tm_nft_policy_id, one_shot_input_ref) in [BSS-4] order") {
        // Param order per spec [BSS-4]: (tm_nft_policy_id, one_shot_input_ref). The first param is
        // the TreasuryMovementValidator script hash, which is also the TM NFT policy id, so the TM
        // script hash must be derived BEFORE the singleton at deploy.
        val bss = BridgeStateContract(blueprint, tmScriptHash, cpiRef)

        // Parameter sensitivity: both params must reach the script.
        assert(bss.policyId != BridgeStateContract(blueprint, tmNftPolicy, cpiRef).policyId)
        assert(bss.policyId != BridgeStateContract(blueprint, tmScriptHash, configRef).policyId)
    }

    /** An ft-bifrost-bridge checkout to compare the vendored copy against, when one is on disk.
      *
      * The policy-id pins above are computed from the vendored resource, so they lock it against
      * ITSELF and say nothing about drift from ft. Only this comparison does. It cancels with no
      * checkout, because CI and release builds have none.
      */
    private def ftPlutusJson: Option[Path] =
        sys.env.get("BIFROST_PLUTUS_JSON").map(_.trim).filter(_.nonEmpty) match {
            // An explicitly named blueprint that cannot be read is a broken setup, never a
            // skip: silently falling through to a sibling checkout (or to cancel) is exactly
            // how a stale vendored copy stays green on the one machine that could catch it.
            case Some(p) =>
                val path = Paths.get(p)
                if !Files.isReadable(path) then
                    fail(
                      s"BIFROST_PLUTUS_JSON is set to '$p' but not readable — refusing to " +
                          "silently skip the drift check"
                    )
                Some(path)
            case None =>
                List(
                  "../ft-bifrost-bridge/onchain/plutus.json",
                  "../../ft-bifrost-bridge/onchain/plutus.json",
                  "../../FluidTokens/ft-bifrost-bridge/onchain/plutus.json"
                ).map(Paths.get(_)).find(Files.isReadable)
        }

    /** Validators exempt from the freshness check below. EMPTY, and that is the point.
      *
      * It used to hold `config` and `treasury`, because ft's `plutus.json` did not reproduce from
      * its own source — the committed copy came from a local `aiken v1.1.23+unknown` while CI
      * installs `v1.1.23+8949565`, so every ft commit that rebuilt the blueprint moved those two
      * whether or not their `.ak` changed. Carrying the deployed bytes and exempting them here was
      * the honest position while that held, since `config`'s hash IS the config NFT policy id and
      * absorbing a rebuild-only change re-identifies the whole bridge for no semantic reason.
      *
      * Measured 2026-08-14 and no longer true: a pristine rebuild at the pinned `compiler =
      * "v1.1.23"` (local `v1.1.23+8949565`, the version CI installs) reproduces ft's committed
      * blueprint byte for byte — only the validator actually edited moves. The exemption is
      * therefore removed as its own comment asked, and a difference here is real again.
      *
      * The one-time cost of removing it was absorbed by the WI-090 re-vendor: `treasury_info` was
      * carrying pre-existing drift, which re-identifies its policy id. That is free HERE only
      * because `treasury_info` is parameterized by the config policy id, which the WI-090 datum
      * append moves regardless — a new bridge instance either way. Do not read it as a precedent
      * for absorbing drift cheaply.
      */
    private val RebuildDrift: Set[String] = Set.empty

    test("every vendored compiledCode is ft's current one") {
        // Freshness against ft, for the WHOLE vendored set — a stale peg_in or peg_out is just
        // as fatal as a stale bridge_state (the watchtower derives their script hashes, and
        // every transaction it builds against a stale hash is rejected on-chain). Cancels
        // without a checkout, so it guards a developer machine only — the per-contract policy
        // pins above are what guard CI. Keep BOTH: the pins cannot see ft, this cannot run
        // there.
        ftPlutusJson match {
            case None =>
                cancel(
                  "no ft-bifrost-bridge checkout found (set BIFROST_PLUTUS_JSON to enable this check)"
                )
            case Some(path) =>
                val ft = BifrostBlueprint.fromFile(path.toString)
                val stale =
                    blueprint.validatorTitles.filterNot(RebuildDrift.contains).filter { title =>
                        val ftCode =
                            try ft.compiledCode(title)
                            catch {
                                case _: RuntimeException =>
                                    fail(
                                      s"validator '$title' is vendored but absent from $path — " +
                                          "the two blueprints no longer describe the same bridge"
                                    )
                            }
                        ftCode != blueprint.compiledCode(title)
                    }
                assert(
                  stale.isEmpty,
                  s"vendored compiledCode is stale against $path for: ${stale.mkString(", ")} " +
                      "– re-vendor bifrost-plutus-min.json"
                )
        }
    }
}
