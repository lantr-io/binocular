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
}

object BifrostBlueprint {
    def fromFile(path: String): BifrostBlueprint =
        new BifrostBlueprint(ujson.read(Files.readString(Paths.get(path))))
}

/** The `peg_in_validator` parameterized with its three on-chain params. The script hash is the
  * peg-in NFT `policyId` and the address that `PegInRequest` UTxOs are locked at.
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
