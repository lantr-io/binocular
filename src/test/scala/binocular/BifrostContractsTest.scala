package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

/** Tests for the F3 bridge-contract hash computation in [[BifrostContracts]].
  *
  * The known-answer cases are REGRESSION LOCKS over the CIP-57 parameter-application encoding
  * (`Data.B` for byte params, `Data.I` for `index0`, the one-shot `OutputReference` as `Data`, and
  * param order). They are re-validated at the next `deploy-bridge` run. They use a trimmed
  * `plutus.json` (only the referenced validators' `compiledCode`) checked in as a test resource, so
  * the test runs without the sibling ft-bifrost-bridge checkout.
  *
  * Refreshed 2026-07-20 (Route-1 alignment) to the current ft blueprint: the 17-field ConfigDatum
  * (upstream's `initial_btc_treasury_utxo` #11 + our tunables #12-16) changed `config.config`'s
  * compiledCode, which cascades through the config policy into every config-parameterized contract,
  * so all four policy pins moved. The min-json was also brought up to ft's current `compiledCode`
  * for the other validators (it had drifted behind upstream's own regenerated blueprint).
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
          hex(configContract.policyId) == "48273334c60ab4be2aea0df669f039817b7c078815e94cca45a2ca39"
        )
    }

    test("bridged_token policy matches the deployed value") {
        val bt = BridgedTokenContract(blueprint, configPolicy, configAssetName)
        assert(hex(bt.policyId) == "7e9b1c95e2b19bc384fa5df1ca024519f84a95a04bbf0e86b6405721")
    }

    test("completed-peg-ins policy + asset name match the Variant B rebuild") {
        // policyId regression lock over the Variant B rebuild. The asset name is now the constant
        // "CPI" (bytes 435049), independent of the one-shot ref and the compiledCode.
        val cpi = CompletedPegInsContract(blueprint, configPolicy, configAssetName, cpiRef)
        assert(hex(cpi.policyId) == "bc57e42636bab1756ede91ae3f1702ee6da1a877c2f5d98098dac40b")
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
        assert(hex(pegIn.policyId) == "80126462fdc2e1548c6c1852d0664606a6b9b4c73a3a8534d8e2d8cd")
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
