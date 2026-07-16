package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.uplc.builtin.Data.toData

/** Tests for the F3 bridge-contract hash computation in [[BifrostContracts]].
  *
  * The known-answer cases are REGRESSION LOCKS over the CIP-57 parameter-application encoding
  * (`Data.B` for byte params, `Data.I` for `index0`, the one-shot `OutputReference` as `Data`, and
  * param order). They were recomputed after the delegated-fBTC-mint rewrite (ConfigDatum field 19,
  * `bridged_token` renamed + reduced to a delegator, new `fbtc_mint_checker`) changed every
  * compiled hash, and are NOT yet on-chain-validated: the next `deploy-bridge` run re-validates
  * them (the 2026-05-21 devnet deploy validated the pre-rewrite encoding). They use a trimmed
  * `plutus.json` (only the referenced validators' `compiledCode`) checked in as a test resource, so
  * the test runs without the sibling ft-bifrost-bridge checkout.
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
          hex(configContract.policyId) == "75886148b65c25ace525571f6d93f908387475c33a14d69cff7b970b"
        )
    }

    test("bridged_token (fBTC) policy matches the deployed value") {
        val bt = BridgedTokenContract(blueprint, configPolicy, configAssetName)
        assert(hex(bt.policyId) == "61ac3cdfc9b9069655fc7e8f8a09698739e55bc3ceed59bd29f22717")
    }

    test("fbtc_mint_checker hash is stable for the 2-param encoding") {
        val checker = FbtcMintCheckerContract(blueprint, configPolicy, configAssetName)
        assert(
          hex(checker.scriptHash) == "62562a7b9916c6d248e17f274831c8f8345ebfb507481dfa50bf1cd2"
        )
    }

    test("completed-peg-ins policy + asset name match the B1 rebuild") {
        // policyId changed after the B1 peg-in.ak rewrite: completed-peg-ins-merkle-tree.ak imports
        // the `CompletePegIn` type, whose redeemer fields changed, so its shared compiledCode (and
        // thus the policy hash) moved from 64b45a49…. The asset name = sha2_256(serialiseData(ref))
        // is independent of compiledCode and is unchanged.
        val cpi = CompletedPegInsContract(blueprint, configPolicy, configAssetName, cpiRef)
        assert(hex(cpi.policyId) == "7d297fa75863fb0e6f0f8ecc68eba83c3913a5e836943d230bb7aa9d")
        assert(
          CompletedPegInsContract.assetName(cpiRef).toHex
              == "bc7b1b2eec39061b7e2561b81163c6b037e59e27930db71a704122d952ea772d"
        )
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
        assert(hex(pegIn.policyId) == "fc61f1904c96f24e33cd637c724921a7c5de18c652ac787187f49167")
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
