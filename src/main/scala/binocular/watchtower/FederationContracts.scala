package binocular.watchtower

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Script, ScriptHash}
import scalus.uplc.Program
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

/** The ft-bifrost-bridge validators that make up the SPO **federation**: the registry of registered
  * SPOs, the ban list parameterized by it, the `treasury_info` state UTxO their DKG rotates, and
  * the three fault verifiers the ban list authorizes.
  *
  * They live here rather than in [[BifrostContracts]] because they belong to a different half of
  * the bridge: nothing in the completion path (peg-in, peg-out, TM confirm) touches them. Bridge
  * genesis does, because a bridge that comes into existence without a registry and a ban list is
  * one whose SPOs must be told where those are — and every value that would tell them is an INPUT
  * to the policy id it identifies, so a hand-copied one that is wrong yields a well-formed address
  * holding nothing rather than an error.
  *
  * ==Derivation order==
  * The chain is: one-shot outref -> `spos_registry` -> the three fault verifiers (each takes the
  * registry hash) -> `spo_bans` (takes the registry hash AND the three fault policies). Applying
  * them in any other order is impossible; every step needs the one before it.
  *
  * ==The `spo_bans` list parameter==
  * `spo_bans` takes `List<PolicyId>`, and aiken is NOT length-form agnostic: the parameter must be
  * a canonical INDEFINITE-length CBOR array (`9f..ff`). A definite-length array (`83..`) hashes
  * differently and derives a policy no deployment has. `Data.List` encodes indefinite, which is
  * what `aiken blueprint apply` emits — and `FederationContractsTest` pins the resulting hash
  * against the value heimdall's independently-written Rust derives from the same blueprint, so the
  * two implementations can only agree by construction.
  */
final case class SposRegistryContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object SposRegistryContract {
    val ValidatorTitle = "bitcoin/spos_registry.spo_registry.mint"

    /** Asset name of the registry's linked-list root element (`registration_root_key`). */
    val RootAssetName: ByteString = ByteString.fromString("reg-root")

    def apply(
        blueprint: BifrostBlueprint,
        bootstrapTxId: ByteString,
        bootstrapIndex: BigInt
    ): SposRegistryContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(bootstrapTxId))
            .$(Data.I(bootstrapIndex))
        SposRegistryContract(Script.PlutusV3(applied.cborByteString))
    }
}

/** `treasury_info`, parameterized by the `spos_registry` policy alone.
  *
  * Rev 5.4 dropped the second parameter, the TM-NFT policy: it fed only the FederationReset spend
  * branch, which the revision withdrew (spec [UY-7]/[UY-8]).
  *
  * The one parameter must be applied identically by every reader or it computes a different hash, a
  * different address, and a state UTxO nobody can find — which is why bridge genesis publishes the
  * finished policy id in the Config (#12) instead of leaving each node to re-derive it.
  */
final case class TreasuryInfoContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object TreasuryInfoContract {
    val ValidatorTitle = "bitcoin/treasury.treasury_info.mint"

    def apply(
        blueprint: BifrostBlueprint,
        sposRegistryPolicyId: ByteString
    ): TreasuryInfoContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(sposRegistryPolicyId))
        TreasuryInfoContract(Script.PlutusV3(applied.cborByteString))
    }
}

/** One of the three DKG fault verifiers, each parameterized by the registry policy alone. Their
  * policy ids are what `spo_bans` authorizes, so they must be derived before it.
  */
final case class FaultVerifierContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
}

object FaultVerifierContract {
    val Round1Title = "bitcoin/fault_verifier_round1.fault_verifier_round1.mint"
    val Round2Title = "bitcoin/fault_verifier_round2.fault_verifier_round2.mint"
    val EquivocationTitle =
        "bitcoin/fault_verifier_equivocation.fault_verifier_equivocation.mint"

    /** The three titles in the order `spo_bans` is parameterized with. ORDER IS SIGNIFICANT: the
      * list is baked into the ban policy id as given, with no sorting, so a permutation derives a
      * different policy.
      */
    val Titles: List[String] = List(Round1Title, Round2Title, EquivocationTitle)

    def apply(
        blueprint: BifrostBlueprint,
        title: String,
        sposRegistryPolicyId: ByteString
    ): FaultVerifierContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(title))
            .$(Data.B(sposRegistryPolicyId))
        FaultVerifierContract(Script.PlutusV3(applied.cborByteString))
    }

    /** All three, in the order `spo_bans` expects. */
    def all(blueprint: BifrostBlueprint, sposRegistryPolicyId: ByteString): List[ByteString] =
        Titles.map(t =>
            ByteString.fromArray(apply(blueprint, t, sposRegistryPolicyId).policyId.bytes)
        )
}

/** `spo_bans`, the ban-list policy: seven parameters, of which the ban schedule is three. Its hash
  * is the value bridge genesis publishes as Config #17 — the one field that lets an SPO read the
  * ban list while configuring nothing at all.
  */
final case class SpoBansContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object FederationRoot {

    /** The linked-list ROOT element datum, shared by `spos_registry` and `spo_bans`:
      * `Element = Constr(0, [ElementData, Link])` with `ElementData = Root{RootData}` =
      * `Constr(0, [Constr(0, [])])` and `Link = None` = `Constr(1, [])`.
      *
      * Byte-exactness matters: `linked_list.init` re-derives this shape on chain, and heimdall
      * reads the same bytes back when it walks the list.
      */
    val Datum: Data = Data.Constr(
      0,
      PList(Data.Constr(0, PList(Data.Constr(0, PList()))), Data.Constr(1, PList()))
    )

    /** `spos_registry`'s `Bootstrap` mint redeemer: field-less, `Constr(0, [])`. */
    val RegistryBootstrapRedeemer: Data = Data.Constr(0, PList())

    /** `spo_bans`' `Bootstrap { input_ref }` mint redeemer — the one material difference from the
      * registry's. `spo_bans` checks `input_ref == OutputReference(bootstrap_tx_id,
      * bootstrap_output_index)`, i.e. the redeemer must restate the outref already baked into its
      * own policy id, so passing the wrong one fails on chain rather than silently.
      */
    def banBootstrapRedeemer(bootstrapTxId: ByteString, bootstrapIndex: BigInt): Data =
        Data.Constr(0, PList(Data.Constr(0, PList(Data.B(bootstrapTxId), Data.I(bootstrapIndex)))))
}

object SpoBansContract {
    val ValidatorTitle = "bitcoin/spo_bans.spo_bans.mint"

    /** Asset name of the ban list's linked-list root element (`ban_root_key`). */
    val RootAssetName: ByteString = ByteString.fromString("ban-root")

    /** @param faultProofPolicyIds
      *   exactly three DISTINCT verifier policies, in deployment order — `ban_config_ok` requires
      *   three distinct ids, and the list is baked into the hash unsorted.
      */
    def apply(
        blueprint: BifrostBlueprint,
        sposRegistryPolicyId: ByteString,
        faultProofPolicyIds: List[ByteString],
        baseBanDurationMs: BigInt,
        maxFaultsBeforePermanent: BigInt,
        maxValidityWindowMs: BigInt,
        bootstrapTxId: ByteString,
        bootstrapIndex: BigInt
    ): SpoBansContract = {
        require(
          faultProofPolicyIds.size == 3 && faultProofPolicyIds.distinct.size == 3,
          s"spo_bans takes exactly 3 DISTINCT fault verifier policies (ban_config_ok), got " +
              s"${faultProofPolicyIds.size} (${faultProofPolicyIds.distinct.size} distinct)"
        )
        require(baseBanDurationMs > 0, "base_ban_duration_ms must be > 0")
        require(maxFaultsBeforePermanent > 0, "max_faults_before_permanent must be > 0")
        require(maxValidityWindowMs >= 0, "max_validity_window_ms must be >= 0")
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(sposRegistryPolicyId))
            // Indefinite-length array — see the note on this file's header.
            .$(Data.List(PList.from(faultProofPolicyIds.map(p => Data.B(p): Data))))
            .$(Data.I(baseBanDurationMs))
            .$(Data.I(maxFaultsBeforePermanent))
            .$(Data.I(maxValidityWindowMs))
            .$(Data.B(bootstrapTxId))
            .$(Data.I(bootstrapIndex))
        SpoBansContract(Script.PlutusV3(applied.cborByteString))
    }
}
