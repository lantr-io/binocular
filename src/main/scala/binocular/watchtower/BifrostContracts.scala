package binocular.watchtower

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Script, ScriptHash}
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.uplc.Program
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.Data.toData

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

    /** Every validator title in the blueprint, in file order. Lets the drift test iterate the
      * WHOLE vendored set instead of naming validators one by one.
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
        configNftPolicyId: ByteString,
        configNftAssetName: ByteString
    ): PegInContract = {
        val base = Program.fromCborHex(blueprint.compiledCode(ValidatorTitle))
        val applied = base
            .$(Data.B(oraclePolicyId))
            .$(Data.B(configNftPolicyId))
            .$(Data.B(configNftAssetName))
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
        index0: BigInt,
        configAssetName: ByteString
    ): ConfigContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(tx0))
            .$(Data.I(index0))
            .$(Data.B(configAssetName))
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
        configNftPolicyId: ByteString,
        configNftAssetName: ByteString
    ): BridgedTokenContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(configNftPolicyId))
            .$(Data.B(configNftAssetName))
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
        configNftAssetName: ByteString,
        oneShotInputRef: TxOutRef
    ): CompletedPegInsContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(configNftPolicyId))
            .$(Data.B(configNftAssetName))
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
        configNftPolicyId: ByteString,
        configNftAssetName: ByteString
    ): PegOutContract = {
        val applied = Program
            .fromCborHex(blueprint.compiledCode(ValidatorTitle))
            .$(Data.B(configNftPolicyId))
            .$(Data.B(configNftAssetName))
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
