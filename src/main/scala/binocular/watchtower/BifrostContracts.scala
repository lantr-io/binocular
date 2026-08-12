package binocular.watchtower

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Script, ScriptHash}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.uplc.Program
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData, toData}

import java.nio.file.{Files, Paths}

/** Reads ft-bifrost-bridge Aiken validators from a CIP-57 `plutus.json` blueprint.
  *
  * Parameter application mirrors `aiken blueprint apply` / Blaze `applyParamsToScript`: each
  * declared parameter is applied as a Plutus `Data` constant per its CIP-57 schema. For the
  * `ByteArray` params used here that is `Data.B(bytes)`, which is exactly what Scalus's
  * `Program.$(data: Data)` produces.
  */
final class BifrostBlueprint(json: ujson.Value) {

    /** Single-CBOR `compiledCode` hex for `title` (all handlers of one Aiken validator share it).
      */
    def compiledCode(title: String): String =
        json("validators").arr
            .find(_("title").str == title)
            .map(_("compiledCode").str)
            .getOrElse(throw new RuntimeException(s"validator not found in blueprint: $title"))

    /** Every validator title in the blueprint, in file order. Lets the drift test iterate the WHOLE
      * vendored set instead of naming validators one by one.
      */
    def validatorTitles: Seq[String] = json("validators").arr.toSeq.map(_("title").str)
}

object BifrostBlueprint {

    /** Classpath path of the blueprint vendored into binocular's own jar. */
    val PackagedResource = "/bifrost-plutus-min.json"

    def fromFile(path: String): BifrostBlueprint =
        fromString(Files.readString(Paths.get(path)))

    def fromString(json: String): BifrostBlueprint =
        new BifrostBlueprint(ujson.read(json))

    /** The blueprint vendored as a jar resource: the `compiledCode` of every ft-bifrost-bridge
      * validator binocular applies parameters to, copied byte for byte from
      * `ft-bifrost-bridge/onchain/plutus.json`.
      *
      * It exists so nothing at runtime depends on a sibling ft checkout. A Docker image or a
      * systemd unit has no `../../…/plutus.json`, and the confirm worker must derive the
      * completed-peg-outs trie validator on every startup — without this it would crash-loop.
      *
      * ==Freshness is NOT guarded by the policy-id pins==
      * `BifrostContractsTest`'s pins are computed from THIS resource, so they lock the resource
      * against ITSELF: they catch an accidental edit here, and they say nothing whatsoever about
      * whether it still matches ft's `plutus.json`. That belief is exactly how a stale `peg_out`
      * survived a validator rewrite (2026-08) — the pins stayed green while every completion
      * binocular built would have been rejected on-chain.
      *
      * Two things do guard it, and both must be kept:
      *   - `BifrostContractsTest` compares this resource against a sibling ft checkout's
      *     `plutus.json` when one is present, and cancels when it is not (CI has no ft checkout).
      *   - The CEK suites (`PegOutCompleteCekTest`) EVALUATE these bytes, so a validator whose
      *     semantics moved fails on behaviour rather than on a hash.
      *
      * Refresh with a straight copy of the `compiledCode` fields from ft's `plutus.json`, then move
      * the affected pins in the same commit.
      */
    def packaged: BifrostBlueprint = {
        val stream = getClass.getResourceAsStream(PackagedResource)
        if stream == null then
            throw new IllegalStateException(
              s"Blueprint resource $PackagedResource not found on the classpath — the jar is built wrong"
            )
        try fromString(scala.io.Source.fromInputStream(stream).mkString)
        finally stream.close()
    }

    /** Resolve the blueprint to use, preferring an on-disk override.
      *
      * `path` is `bridge.plutus-json` (env `BIFROST_PLUTUS_JSON`), whose default points at a
      * sibling ft checkout. When that file EXISTS it wins, so a developer working on the Aiken
      * validators sees their edits immediately. When it does not — the normal state of a deployed
      * image — the [[packaged]] resource is used, and startup succeeds.
      *
      * Returns the blueprint and a human-readable description of where it came from, so every
      * command can log which one it used instead of leaving it ambiguous.
      */
    def resolve(path: String): (BifrostBlueprint, String) = {
        val trimmed = Option(path).map(_.trim).getOrElse("")
        if trimmed.nonEmpty && Files.isReadable(Paths.get(trimmed)) then
            (fromFile(trimmed), trimmed)
        else (packaged, s"packaged $PackagedResource")
    }
}

/** The `peg_in_validator` parameterized with its on-chain params. The script hash is the peg-in NFT
  * `policyId` and the address that `PegInRequest` UTxOs are locked at.
  *
  * Rev 5.4: the `tm_nft_policy_id` parameter is GONE — `peg_in.ak` reads the bridge-state singleton
  * through Config field 3 at runtime instead of referencing a Confirmed TM record, so three params
  * remain: `(oracle_policy_id, config_nft_policy_id, config_nft_asset_name)`.
  */
final case class PegInContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object PegInContract {

    // All handlers share one compiledCode; any title for the validator works.
    val ValidatorTitle = "bitcoin/peg_in.peg_in_validator.mint"

    def apply(
        blueprint: BifrostBlueprint,
        oraclePolicyId: ByteString,
        configNftPolicyId: ByteString
    ): PegInContract = {
        val base = Program.fromCborHex(blueprint.compiledCode(ValidatorTitle))
        val applied = base
            .$(Data.B(oraclePolicyId))
            .$(Data.B(configNftPolicyId))
        PegInContract(Script.PlutusV3(applied.cborByteString))
    }

    /** Peg-in NFT asset name per `peg_in.ak`: `hash_output_ref(input_ref)` =
      * `sha2_256(serialise_data(output_ref))` (32 bytes, the Cardano asset-name maximum). The
      * output ref is the one-shot wallet UTxO consumed by the mint (the `input_ref` field of
      * `PegInMintRedeemer`). Matches the bare-hash convention of `treasury` / merkle-tree minters;
      * see internal-docs peg-in-assetname-bug.md (the original `0x00 ++ hash` was 33 bytes).
      */
    def assetName(inputRef: TxOutRef): ByteString =
        Builtins.sha2_256(Builtins.serialiseData(inputRef.toData))
}

/** The `config.config` one-shot NFT policy: `config(tx0, index0, config_asset_name)`. The script
  * hash is the config-NFT policyId; the ConfigDatum-bearing UTxO lives at this script's address and
  * is referenced (never spent — `spend = False`) by the completion path.
  */
final case class ConfigContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object ConfigContract {
    val ValidatorTitle = "bitcoin/config.config.mint"

    def apply(
        blueprint: BifrostBlueprint,
        tx0: ByteString,
        index0: BigInt
    ): ConfigContract = {
        // spec [CFG-7]: the asset name is the "BIFCFG" constant, not a parameter.
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(tx0))
            .$(Data.I(index0))
        ConfigContract(Script.PlutusV3(applied.cborByteString))
    }
}

/** The `bridged_token` (fBTC/fSAT) mint policy: params `(configNFTPolicyId, configNFTAssetName)`.
  * The script hash is the token policyId = ConfigDatum index 1. It reads the ConfigDatum from the
  * config ref input and enforces the Variant B mint/burn rules against the peg-in / peg-out
  * withdrawals directly.
  */
final case class BridgedTokenContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
}

object BridgedTokenContract {
    val ValidatorTitle = "bitcoin/bridged_token.bridged_token.mint"

    def apply(
        blueprint: BifrostBlueprint,
        configNftPolicyId: ByteString
    ): BridgedTokenContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(configNftPolicyId))
        BridgedTokenContract(Script.PlutusV3(applied.cborByteString))
    }
}

/** The `completed_peg_ins_merkle_tree` one-shot NFT policy + state validator: params
  * `(configNFTPolicyId, configNFTAssetName, one_shot_input_ref)`. policyId = ConfigDatum index 2
  * (`completed_peg_ins_policy`); asset name = the constant `"CPI"`. The MPF state UTxO (datum =
  * root, empty `0x00*32` at mint) lives at this script's address and is spent+recreated on each
  * completion.
  */
final case class CompletedPegInsContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object CompletedPegInsContract {
    val ValidatorTitle =
        "bitcoin/completed_peg_ins_merkle_tree.completed_peg_ins_merkle_tree_validator.mint"

    def apply(
        blueprint: BifrostBlueprint,
        configNftPolicyId: ByteString,
        oneShotInputRef: TxOutRef
    ): CompletedPegInsContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(configNftPolicyId))
            .$(oneShotInputRef.toData)
        CompletedPegInsContract(Script.PlutusV3(applied.cborByteString))
    }

    /** Constant per completed-peg-ins-merkle-tree.ak. */
    val assetName: ByteString = ByteString.fromString("CPI")
}

/** The `peg_out_validator` parameterized with `(config_nft_policy_id, config_nft_asset_name)`. The
  * script hash is the peg-out withdraw script hash = ConfigDatum index 6, and the address that
  * `PegOut` UTxOs are locked at. The completion path is a `withdraw` (`CompletePegOut`); creation
  * is a plain pay-to-this-address output.
  *
  * The `oracle_policy_id` parameter is GONE (peg-out trie v2, 2026-07): `peg_out.ak` no longer does
  * its own SPV parse of the Treasury Movement. Which Bitcoin payment settles which peg-out request
  * is recorded in the completed-peg-outs trie at TM Confirm, so completion is a single MPF
  * membership proof against a trie reference input and needs no oracle. Dropping the parameter
  * CHANGES this script's hash, so ConfigDatum field 6 must be swapped by a config Update (see
  * `update-config --peg-out-withdraw-hash`) before any peg-out completes.
  */
final case class PegOutContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object PegOutContract {
    // All handlers share one compiledCode; any title for the validator works.
    val ValidatorTitle = "bitcoin/peg_out.peg_out_validator.withdraw"

    def apply(
        blueprint: BifrostBlueprint,
        configNftPolicyId: ByteString
    ): PegOutContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(configNftPolicyId))
        PegOutContract(Script.PlutusV3(applied.cborByteString))
    }
}

/** The `bridge_state` one-shot NFT policy + singleton validator: params `(tm_nft_policy_id,
  * one_shot_input_ref)` per spec [BSS-4]. policyId = the bridge-state policy the ConfigDatum names;
  * asset name = the constant `"BSS"`. The singleton UTxO carries that NFT and the `BridgeState`
  * datum (both roots), and lives at this script's address.
  *
  * Rev 5.4 replaces `completed-peg-outs-merkle-tree.ak` with this validator: one UTxO now holds the
  * completed-peg-outs root and the swept-peg-ins root together, so a TM Confirm updates both in a
  * single spend.
  *
  * The first parameter is the TM NFT policy = the [[TreasuryMovementValidator]] script hash,
  * because the spend handler gates on a TM Confirm spend in the same transaction. There is no
  * parameterization cycle: the TM script hash is computable first (oracle hash + config NFT pair),
  * and the TM validator finds THIS policy at runtime through the ConfigDatum.
  *
  * Consequence for deploy ordering: derive the TM script hash BEFORE this contract, and put the
  * policy this constructor yields into the genesis ConfigDatum.
  */
final case class BridgeStateContract(script: Script.PlutusV3) {
    def policyId: ScriptHash = script.scriptHash
    def address(network: Network): Address =
        Address(network, Credential.ScriptHash(script.scriptHash))
}

object BridgeStateContract {

    // All handlers share one compiledCode; any title for the validator works.
    val ValidatorTitle = "bitcoin/bridge_state.bridge_state.mint"

    def apply(
        blueprint: BifrostBlueprint,
        tmNftPolicyId: ByteString,
        oneShotInputRef: TxOutRef
    ): BridgeStateContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(tmNftPolicyId))
            .$(oneShotInputRef.toData)
        BridgeStateContract(Script.PlutusV3(applied.cborByteString))
    }

    /** Constant per `bridge-state.ak`, mirrored by the TM validator's own copy. */
    val assetName: ByteString = TreasuryMovementValidator.BridgeStateAssetName
}

// ================================ the SPO federation ================================
// The validators below are the SPO **federation** half of the bridge: the registry of registered
// SPOs, the ban list parameterized by it, the `treasury_info` state UTxO their DKG rotates, and
// the three fault verifiers the ban list authorizes. Nothing in the completion path (peg-in,
// peg-out, TM confirm) touches them — but bridge genesis derives both halves in one chain from
// the same blueprint, which is why they share this file.
//
// Every value that identifies one of these scripts is an INPUT to the policy id it identifies, so
// an SPO handed a wrong one derives a well-formed address holding nothing rather than an error.
// That is what genesis publishing the finished ids in the Config is for.

/** Every federation script, derived from the two inputs that fix them all.
  *
  * The chain is strictly ordered and each step needs the one before it:
  * {{{
  *   (federation one-shot, config policy) -> treasury_info -> spos_registry
  *                                        -> 3 fault verifiers -> spo_bans
  * }}}
  * Rev 5.5 inverted the first hop. `treasury_info` used to take `registry_policy_id`, which made
  * the dependency a cycle and the [REG-6] pin impossible; it takes the Config policy now ([PRE-3])
  * and `treasury.ak` reads the registry policy from Config #9 at run time ([PRE-4]).
  *
  * It lives in one place because three commands need the same answer — genesis mints these
  * policies, `deploy-script-refs` publishes their scripts, `register-bridge-creds` registers the
  * ban list's reward account — and a chain applied in a different order, or with one parameter off,
  * yields well-formed addresses holding nothing rather than an error.
  */
final case class FederationScripts(
    treasury: TreasuryInfoContract,
    registry: SposRegistryContract,
    faultPolicies: List[ByteString],
    bans: SpoBansContract
)

object FederationScripts {

    /** @param banSchedule
      *   `(base_ban_duration_ms, max_faults_before_permanent, max_validity_window_ms)`. These are
      *   INPUTS to the ban policy id, so they are not governance-updatable in place: genesis reads
      *   them from local config, every later caller MUST read them back from the deployed Config's
      *   `params` (indices 4..6) rather than from its own file, or it derives a ban list that is
      *   not the bridge's.
      */
    def derive(
        blueprint: BifrostBlueprint,
        federationTxId: ByteString,
        federationIndex: BigInt,
        configPolicyId: ByteString,
        banSchedule: (BigInt, BigInt, BigInt)
    ): FederationScripts = {
        val treasury =
            TreasuryInfoContract(blueprint, federationTxId, federationIndex, configPolicyId)
        val treasuryPolicy = ByteString.fromArray(treasury.policyId.bytes)
        val registry =
            SposRegistryContract(blueprint, federationTxId, federationIndex, treasuryPolicy)
        val registryPolicy = ByteString.fromArray(registry.policyId.bytes)
        val faultPolicies = FaultVerifierContract.all(blueprint, registryPolicy)
        val (baseBanDurationMs, maxFaultsBeforePermanent, maxValidityWindowMs) = banSchedule
        val bans = SpoBansContract(
          blueprint,
          registryPolicy,
          faultPolicies,
          baseBanDurationMs = baseBanDurationMs,
          maxFaultsBeforePermanent = maxFaultsBeforePermanent,
          maxValidityWindowMs = maxValidityWindowMs,
          bootstrapTxId = federationTxId,
          bootstrapIndex = federationIndex
        )
        FederationScripts(treasury, registry, faultPolicies, bans)
    }

    /** Check a derivation against what the deployed Config publishes (#8 bans, #9 registry, #10
      * treasury).
      *
      * The federation one-shot is operator-supplied config, and a wrong one derives three
      * well-formed policies that name nothing. The Config is the authority on all three ids, so
      * comparing closes the loop before anything is published or registered against them.
      */
    def verifyAgainstConfig(scripts: FederationScripts, config: ConfigDatum): Either[String, Unit] = {
        def cmp(what: String, derived: ScriptHash, published: ByteString): Either[String, Unit] =
            Either.cond(
              derived.toHex == published.toHex,
              (),
              s"derived $what policy ${derived.toHex} does not match the Config's ${published.toHex} " +
                  "— bridge.federation-one-shot-ref is not the outpoint this bridge was deployed from"
            )
        for {
            _ <- cmp("treasury_info", scripts.treasury.policyId, config.treasuryInfoPolicyId)
            _ <- cmp("spos_registry", scripts.registry.policyId, config.sposRegistryPolicyId)
            _ <- cmp("spo_bans", scripts.bans.policyId, config.spoBansPolicyId)
        } yield ()
    }
}

/** `spos_registry`, the on-chain registry of registered SPOs.
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
  * is the value bridge genesis publishes as Config #8 — the one field that lets an SPO read the
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
            // Indefinite-length array — see the note on [[SposRegistryContract]].
            .$(Data.List(PList.from(faultProofPolicyIds.map(p => Data.B(p): Data))))
            .$(Data.I(baseBanDurationMs))
            .$(Data.I(maxFaultsBeforePermanent))
            .$(Data.I(maxValidityWindowMs))
            .$(Data.B(bootstrapTxId))
            .$(Data.I(bootstrapIndex))
        SpoBansContract(Script.PlutusV3(applied.cborByteString))
    }
}
