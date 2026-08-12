package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** Cross-implementation lock on the federation policy ids.
  *
  * These are not arbitrary known-answers. Every hash below was derived by **heimdall's**
  * independently written Rust parameterization from this same vendored blueprint, and heimdall's
  * own suite pins that implementation against `aiken blueprint apply`. So a change here that moves
  * a hash means binocular and heimdall would derive different addresses for the same bridge — the
  * exact failure the published Config fields exist to prevent, since a wrong derivation yields a
  * well-formed address holding nothing rather than an error.
  *
  * The `spo_bans` case is the sharp one: its `List<PolicyId>` parameter must be a canonical
  * INDEFINITE-length CBOR array, and aiken is not length-form agnostic. If `Data.List` ever encoded
  * definite-length, only this test would say so.
  */
class FederationContractsTest extends AnyFunSuite {

    private val blueprint = BifrostBlueprint.packaged

    /** The one-shot outref and Config policy the vectors were derived with. */
    private val oneShot = ByteString.fromHex("bb" * 32)
    private val oneShotIndex = BigInt(2)
    private val configPolicy = ByteString.fromHex("c0" * 28)

    /** Rev 5.5 chain: Config identity -> treasury -> registry. Rev 5.4 ran it the other way, so
      * every vector below moved when the arrow flipped.
      */
    private def treasury = TreasuryInfoContract(blueprint, oneShot, oneShotIndex, configPolicy)
    private def treasuryPolicy = ByteString.fromArray(treasury.policyId.bytes)
    private def registry =
        SposRegistryContract(blueprint, oneShot, oneShotIndex, treasuryPolicy)
    private def registryPolicy = ByteString.fromArray(registry.policyId.bytes)

    test("spos_registry policy matches heimdall's derivation") {
        assert(
          registry.policyId.toHex == "b45580ee0d3aa620e593b8a53843444785256d93d26809efe44f39e3"
        )
    }

    test("the three fault verifiers match heimdall's derivation, in spo_bans order") {
        assert(
          FaultVerifierContract.all(blueprint, registryPolicy).map(_.toHex) == List(
            "4a053562875c1b23dac1829acd144ef359cf8421b9f968ab941d8a7a",
            "2eb09eaa681d5d5a50f0ad7772b39e859f78d5dfb2e42f37eb2cc43c",
            "d623b499008b04ba2141dfd215a3c11ad16657ef804ae96a69c94a3b"
          )
        )
    }

    test("treasury_info policy matches heimdall's derivation") {
        // Rev 5.5: its OWN one-shot outpoint plus the Config policy ([PRE-1] revised, [PRE-3]).
        // The registry parameter is gone — it was the cycle that made the [REG-6] pin impossible.
        assert(
          treasury.policyId.toHex == "27a61d3ce49a9ed491dfb262efef1fc9c252289da3595681b3442782"
        )
    }

    // The seven-parameter application, including the indefinite-array List<PolicyId>.
    test("spo_bans policy matches heimdall's derivation") {
        val bans = SpoBansContract(
          blueprint,
          registryPolicy,
          FaultVerifierContract.all(blueprint, registryPolicy),
          baseBanDurationMs = BigInt(600000),
          maxFaultsBeforePermanent = BigInt(3),
          maxValidityWindowMs = BigInt(3600000),
          bootstrapTxId = oneShot,
          bootstrapIndex = oneShotIndex
        )
        assert(bans.policyId.toHex == "9c645d0c9f2efc5d67df8c27b9e850873edccfd3d0609b1aa5f7b4f4")
    }

    // The fault-policy list is baked in UNSORTED, so a permutation is a different bridge. Worth a
    // test because the list looks like a set and reordering it looks harmless.
    test("permuting the fault verifier list derives a different ban policy") {
        val ordered = FaultVerifierContract.all(blueprint, registryPolicy)
        def bans(policies: List[ByteString]) = SpoBansContract(
          blueprint,
          registryPolicy,
          policies,
          BigInt(600000),
          BigInt(3),
          BigInt(3600000),
          oneShot,
          oneShotIndex
        ).policyId.toHex
        assert(bans(ordered) != bans(ordered.reverse))
    }

    // Every schedule number is baked into the hash, so "the same ban list with a different
    // schedule" is not a thing that exists.
    test("any ban-schedule change derives a different ban policy") {
        val policies = FaultVerifierContract.all(blueprint, registryPolicy)
        def bans(base: BigInt, maxFaults: BigInt, window: BigInt) = SpoBansContract(
          blueprint,
          registryPolicy,
          policies,
          base,
          maxFaults,
          window,
          oneShot,
          oneShotIndex
        ).policyId.toHex
        val baseline = bans(BigInt(600000), BigInt(3), BigInt(3600000))
        assert(bans(BigInt(600001), BigInt(3), BigInt(3600000)) != baseline)
        assert(bans(BigInt(600000), BigInt(4), BigInt(3600000)) != baseline)
        assert(bans(BigInt(600000), BigInt(3), BigInt(3600001)) != baseline)
    }

    test("spo_bans rejects a fault-policy list that is not 3 distinct ids") {
        val p = FaultVerifierContract.all(blueprint, registryPolicy)
        def build(policies: List[ByteString]) = SpoBansContract(
          blueprint,
          registryPolicy,
          policies,
          BigInt(600000),
          BigInt(3),
          BigInt(3600000),
          oneShot,
          oneShotIndex
        )
        intercept[IllegalArgumentException](build(p.take(2)))
        intercept[IllegalArgumentException](build(p :+ p.head))
        intercept[IllegalArgumentException](build(List(p.head, p.head, p(1))))
    }

    // -- genesis root elements ------------------------------------------------

    // Byte-exact against heimdall's encoder, because heimdall READS these bytes back when it walks
    // the linked list: `Element = Constr(0, [Root{RootData}, None])`. Both lists share the shape.
    test("the root element datum is byte-identical to heimdall's") {
        val cbor = scalus.uplc.builtin.Builtins.serialiseData(FederationRoot.Datum).toHex
        assert(cbor == "d8799fd8799fd87980ffd87a80ff")
    }

    test("the root asset names are the on-chain constants") {
        assert(SposRegistryContract.RootAssetName.toHex == "7265672d726f6f74") // "reg-root"
        assert(SpoBansContract.RootAssetName.toHex == "62616e2d726f6f74") // "ban-root"
    }

    // spos_registry's Bootstrap is field-less; spo_bans' carries the outref, and the validator
    // checks it equals the one baked into its own policy id — so the two bootstraps are NOT
    // interchangeable even though they look alike.
    test("the two bootstrap redeemers differ exactly where the validators do") {
        import scalus.uplc.builtin.Builtins.serialiseData
        assert(serialiseData(FederationRoot.RegistryBootstrapRedeemer).toHex == "d87980")
        val banRedeemer = FederationRoot.banBootstrapRedeemer(oneShot, oneShotIndex)
        assert(
          serialiseData(banRedeemer).toHex ==
              "d8799fd8799f5820" + ("bb" * 32) + "02ffff"
        )
    }

    // The derivation chain is strictly ordered: a different one-shot changes the registry, which
    // changes the fault verifiers, which changes the ban policy.
    test("the one-shot outref propagates through the whole chain") {
        val other =
            SposRegistryContract(
              blueprint,
              ByteString.fromHex("cc" * 32),
              oneShotIndex,
              treasuryPolicy
            )
        val otherPolicy = ByteString.fromArray(other.policyId.bytes)
        assert(other.policyId.toHex != registry.policyId.toHex)
        assert(
          FaultVerifierContract.all(blueprint, otherPolicy) !=
              FaultVerifierContract.all(blueprint, registryPolicy)
        )
    }
}
