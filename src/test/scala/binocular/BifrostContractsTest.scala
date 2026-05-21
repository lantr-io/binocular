package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.uplc.builtin.Data.toData

/** Tests for the F3 bridge-contract hash computation in [[BifrostContracts]].
  *
  * The known-answer cases pin the exact policy IDs / asset name the live `deploy-bridge` run
  * produced on the devnet (2026-05-21) — where `config.ak` and `completed-peg-ins-merkle-tree.ak`'s
  * mint validators *accepted* the resulting NFTs on-chain. So these assertions lock in the whole
  * CIP-57 parameter-application encoding (`Data.B` for byte params, `Data.I` for `index0`, the
  * one-shot `OutputReference` as `Data`, and param order) against an on-chain-validated reference.
  * They use a trimmed `plutus.json` (only the four `.mint` validators' `compiledCode`) checked in
  * as a test resource, so the test runs without the sibling ft-bifrost-bridge checkout.
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

    // --- known-answer (on-chain-validated) ---

    test("config NFT policy matches the deployed value") {
        assert(
          hex(configContract.policyId) == "82cbd7e4d37868ac0f59961dadc789e755dfe2dd0135c63908282186"
        )
    }

    test("bridged_token (fBTC) policy matches the deployed value") {
        val bt = BridgedTokenContract(blueprint, configPolicy, configAssetName)
        assert(hex(bt.policyId) == "2fb4053064cd915983b4573967ae70c5c76c674d31107092d5122712")
    }

    test("completed-peg-ins policy + asset name match the deployed values") {
        val cpi = CompletedPegInsContract(blueprint, configPolicy, configAssetName, cpiRef)
        assert(hex(cpi.policyId) == "64b45a4901e58c1085790a34a0feb9c3fabd9e8bc38aa128a760cd05")
        assert(
          CompletedPegInsContract.assetName(cpiRef).toHex
              == "bc7b1b2eec39061b7e2561b81163c6b037e59e27930db71a704122d952ea772d"
        )
    }

    test("peg_in policy (= withdraw hash) matches the deployed value") {
        val pegIn = PegInContract(blueprint, oraclePolicy, configPolicy, configAssetName)
        assert(hex(pegIn.policyId) == "ced3ce66db0af2c6684685ca212bf3b4b0de4ae51494817be03b914e")
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

    test("completed-peg-ins asset name is sha2_256(serialiseData(ref)) and 32 bytes") {
        val expected = Builtins.sha2_256(Builtins.serialiseData(cpiRef.toData))
        assert(CompletedPegInsContract.assetName(cpiRef) == expected)
        assert(CompletedPegInsContract.assetName(cpiRef).length == BigInt(32))
    }
}
