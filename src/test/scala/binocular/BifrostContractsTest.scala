package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

/** Tests for the F3 bridge-contract hash computation in [[BifrostContracts]].
  *
  * The known-answer cases are REGRESSION LOCKS over the CIP-57 parameter-application encoding
  * (`Data.B` for byte params, `Data.I` for `index0`, the one-shot `OutputReference` as `Data`, and
  * param order). They were recomputed after the Variant B on-chain rebuild (11-field ConfigDatum,
  * `bridged_token` enforcing the mint directly, cpi/cpo asset names as the "CPI"/"CPO" constants)
  * changed every compiled hash, and are re-validated at the next `deploy-bridge` run. They use a
  * trimmed `plutus.json` (only the referenced validators' `compiledCode`) checked in as a test
  * resource, so the test runs without the sibling ft-bifrost-bridge checkout.
  */
class BifrostContractsTest extends AnyFunSuite {

    private val blueprint = BifrostBlueprint.fromString(
      scala.io.Source
          .fromInputStream(getClass.getResourceAsStream("/bifrost-plutus-min.json"))
          .mkString
    )

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
        ConfigContract(blueprint, configRef.id.hash, configRef.idx, configAssetName)
    private def configPolicy = ByteString.fromArray(configContract.policyId.bytes)

    // --- known-answer (regression lock, re-validated at next deploy) ---

    test("config NFT policy matches the deployed value") {
        assert(
          hex(configContract.policyId) == "295ca1215bcd72b6912ac839c87700e0189b10a7e25fc7217ad48093"
        )
    }

    test("bridged_token policy matches the deployed value") {
        val bt = BridgedTokenContract(blueprint, configPolicy, configAssetName)
        assert(hex(bt.policyId) == "8f9e114bbededed8567e15638f3ece22b57ac2d17f6ac995661d0775")
    }

    test("completed-peg-ins policy + asset name match the Variant B rebuild") {
        // policyId regression lock over the Variant B rebuild. The asset name is now the constant
        // "CPI" (bytes 435049), independent of the one-shot ref and the compiledCode.
        val cpi = CompletedPegInsContract(blueprint, configPolicy, configAssetName, cpiRef)
        assert(hex(cpi.policyId) == "e55230769526deaf63e3081596c84345c6ca90087b241f4d44d20c59")
        assert(CompletedPegInsContract.assetName == ByteString.fromString("CPI"))
    }

    // The TM-NFT policy param peg_in.ak takes as its 4th argument (= the TreasuryMovementValidator
    // script hash). A fixed 28-byte placeholder here — B1 has not been deployed, so this test is a
    // regression lock over the (4-param) CIP-57 encoding, not an on-chain-validated value.
    private val tmNftPolicy =
        ByteString.fromHex("11111111111111111111111111111111111111111111111111111111")

    test("peg_in policy (= withdraw hash) is stable for the B1 4-param encoding") {
        // B1 rewrite (reference Confirmed TM UTxO + embed depositor auth) + the new tm_nft_policy_id
        // param change peg_in_validator's compiledCode and hash (was 7d66c4f3…). PIRs minted under
        // the old policy are orphaned and must be re-minted under this one.
        val pegIn =
            PegInContract(blueprint, oraclePolicy, configPolicy, configAssetName, tmNftPolicy)
        assert(hex(pegIn.policyId) == "5645bb289408342cc783df87808443da5a803c119be6f5d2d8023179")
    }

    // --- determinism + parameter-sensitivity ---

    test("hash computation is deterministic") {
        val a = ConfigContract(blueprint, configRef.id.hash, configRef.idx, configAssetName)
        val b = ConfigContract(blueprint, configRef.id.hash, configRef.idx, configAssetName)
        assert(a.policyId == b.policyId)
    }

    test("a different one-shot yields a different config policy") {
        val other = ConfigContract(blueprint, cpiRef.id.hash, BigInt(0), configAssetName)
        assert(other.policyId != configContract.policyId)
    }

    test("a different index yields a different config policy") {
        val other = ConfigContract(blueprint, configRef.id.hash, BigInt(1), configAssetName)
        assert(other.policyId != configContract.policyId)
    }

    test("completed-peg-ins/outs asset names are the CPI/CPO constants") {
        assert(CompletedPegInsContract.assetName == ByteString.fromString("CPI"))
        assert(CompletedPegOutsContract.assetName == ByteString.fromString("CPO"))
    }
}
