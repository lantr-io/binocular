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

    /** The one-shot outref and TM policy the vectors were derived with. */
    private val oneShot = ByteString.fromHex("bb" * 32)
    private val oneShotIndex = BigInt(2)
    private val tmNftPolicy = ByteString.fromHex("11" * 28)

    private def registry = SposRegistryContract(blueprint, oneShot, oneShotIndex)
    private def registryPolicy = ByteString.fromArray(registry.policyId.bytes)

    test("spos_registry policy matches heimdall's derivation") {
        assert(
          registry.policyId.toHex == "55c7620a3ccedc1bcc4c1dac278a6f4d2df5eaf01e886a4d8c640d28"
        )
    }

    test("the three fault verifiers match heimdall's derivation, in spo_bans order") {
        assert(
          FaultVerifierContract.all(blueprint, registryPolicy).map(_.toHex) == List(
            "f8fbc8fa8382b9c34225fe377b874f781ef91a8285eecbcd6fdd7ba1",
            "cea188d392a812a0d47ec4982c822d635981e3d7a4b5b0a30430121e",
            "0d902a1dc650eefc54ff807ae0cb6edc6890a364799cf473d068f963"
          )
        )
    }

    test("treasury_info policy matches heimdall's derivation") {
        val ti = TreasuryInfoContract(blueprint, registryPolicy, tmNftPolicy)
        assert(ti.policyId.toHex == "24e8bf8028b0f35a0784a7f24b27f57d14169d69bc9eb6382c3ef14d")
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
        assert(bans.policyId.toHex == "de3287969e62db24075af0af309ed7ca595b9990b6b4dc89bfd2f957")
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

    // The derivation chain is strictly ordered: a different one-shot changes the registry, which
    // changes the fault verifiers, which changes the ban policy.
    test("the one-shot outref propagates through the whole chain") {
        val other = SposRegistryContract(blueprint, ByteString.fromHex("cc" * 32), oneShotIndex)
        val otherPolicy = ByteString.fromArray(other.policyId.bytes)
        assert(other.policyId.toHex != registry.policyId.toHex)
        assert(
          FaultVerifierContract.all(blueprint, otherPolicy) !=
              FaultVerifierContract.all(blueprint, registryPolicy)
        )
    }
}
