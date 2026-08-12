package binocular.watchtower

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Script, ScriptHash}
import scalus.uplc.Program
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData}

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
        bootstrapIndex: BigInt,
        treasuryPolicyId: ByteString
    ): SposRegistryContract = {
        // spec [REG-6]: the third parameter is the Treasury state policy this registry PINS. It
        // could not have been a parameter before rev 5.5 — treasury_info took registry_policy_id,
        // so the dependency was a cycle — and without it the registry located the Treasury state
        // UTxO by redeemer index with no authentication at all.
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(bootstrapTxId))
            .$(Data.I(bootstrapIndex))
            .$(Data.B(treasuryPolicyId))
        SposRegistryContract(Script.PlutusV3(applied.cborByteString))
    }
}

/** Scalus mirror of ft-bifrost-bridge `lib/bifrost/types/treasury.ak::TreasuryDatum` — the Treasury
  * state UTxO's datum, spec [TSY-1].
  *
  * Positional in the Plutus Constr, so the field ORDER here is the contract with `treasury.ak` and
  * with heimdall's `cardano::treasury_bootstrap::bootstrap_datum`, which builds the same bytes from
  * Rust. Swapping the two fields still decodes: both are 32-byte strings, and the result is a
  * treasury whose identity trie root is a public key.
  *
  * Rev 5.5 cut it to these two fields. `last_reset_tm_txid` went with the withdrawn
  * `FederationReset` branch, and `y_federation` / `federation_csv_blocks` moved to the Config
  * ([CFG-6]) — nothing on-chain could ever rotate them here.
  *
  * Unlike [[ConfigDatum]] this type is NOT append-extensible: the validator decodes it as a type,
  * so the arity is part of the contract. The state UTxO sits behind its own NFT, and replacing it
  * is a bootstrap.
  *
  * @param bifrostIdentityRoot
  *   MPF root of the active `bifrost_id_pk -> pool_id` bindings, 32 bytes. `spos_registry` owns
  *   this value through its [REG-5] proof; `treasury.ak` never computes it.
  * @param currentSposFrostKey
  *   the treasury group key, 32 bytes x-only: $Y_{51}$ after the first DKG, $Y_{federation}$ until
  *   then. Update-Y rotates it, and the rotation message commits to the new value.
  */
case class TreasuryInfoDatum(
    bifrostIdentityRoot: ByteString,
    currentSposFrostKey: ByteString
) derives FromData,
      ToData

/** `treasury_info`, parameterized by its OWN one-shot outpoint and the Config NFT policy.
  *
  * Rev 5.4 dropped the TM-NFT policy parameter (it fed only the withdrawn FederationReset branch,
  * spec [UY-7]/[UY-8]); rev 5.5 dropped `registry_policy_id` too ([PRE-1] revised) and added the
  * one-shot outpoint ([PRE-3]).
  *
  * The one-shot is what makes the state NFT a singleton: rev 5.4 minted it one-shot per OUTPOINT
  * rather than per bridge, so anyone could mint a rival state UTxO with a datum of their choosing.
  * Dropping `registry_policy_id` is what broke the parameter cycle and let `spo_registry` pin this
  * policy ([REG-6]); `treasury.ak` reads the registry policy from Config #9 at run time instead
  * ([PRE-4]).
  *
  * The three parameters must be applied identically by every reader or it computes a different
  * hash, a different address, and a state UTxO nobody can find — which is why bridge genesis
  * publishes the finished policy id in the Config (#10).
  */
final case class TreasuryInfoContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object TreasuryInfoContract {
    val ValidatorTitle = "bitcoin/treasury.treasury_info.mint"

    /** Asset name of the Treasury state NFT — spec [CFG-4], a protocol constant since rev 5.5.
      * Uniqueness lives in the policy id, where the one-shot outpoint is a parameter.
      */
    val StateAssetName: ByteString = ByteString.fromString("BFRTRY")

    /** The mint redeemer. `treasury.ak::mint` ignores it (`_redeemer: Data`) — the one-shot is a
      * parameter and the asset name is a constant, so the mint has nothing left to be told.
      */
    val MintRedeemer: Data = Data.Constr(0, PList())

    def apply(
        blueprint: BifrostBlueprint,
        oneShotTxId: ByteString,
        oneShotIndex: BigInt,
        configPolicyId: ByteString
    ): TreasuryInfoContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(oneShotTxId))
            .$(Data.I(oneShotIndex))
            .$(Data.B(configPolicyId))
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
