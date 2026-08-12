package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Coin, Credential, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Tests for the shared bridge-bootstrap pieces.
  *
  * The singleton output shape is what `bootstrap-bridge-state` and `deploy-bridge` must agree on,
  * and it is what `bridge-state.ak::mint` checks ([BSS-5]): an output at the policy's own script
  * address holding the single "BSS" token. The BridgeState datum is operator-supplied, not pinned
  * on-chain (spec §Why the bootstrap datum is not pinned).
  */
class BridgeBootstrapTest extends AnyFunSuite {

    private val blueprint = BifrostBlueprint.packaged
    private val tmPolicy =
        ByteString.fromHex("11111111111111111111111111111111111111111111111111111111")
    private val oneShot =
        TxOutRef(
          TxId(
            ByteString.fromHex("231b92c928c2bac84280330881ad92084a2d616fab3c6a6321080fa0f29ad5a4")
          ),
          BigInt(0)
        )
    private val contract = BridgeStateContract(blueprint, tmPolicy, oneShot)

    private val bootstrapState = BridgeState(
      spiRoot = BridgeBootstrap.EmptyRoot,
      cpoRoot = BridgeBootstrap.EmptyRoot,
      treasuryUtxoId = ByteString.fromHex(("aa" * 32) + "00000000"),
      treasuryAmount = BigInt(100_000)
    )
    private val network = Network.Testnet

    private def utxo(txHashByte: Int, idx: Int, lovelace: Long, assets: Boolean = false): Utxo = {
        val hash = TransactionHash.fromHex(f"$txHashByte%02x" * 32)
        val addr = Address(network, Credential.ScriptHash(contract.policyId))
        val value =
            if assets then
                Value(Coin(lovelace)) + Value.asset(
                  contract.policyId,
                  AssetName(BridgeStateContract.assetName),
                  1L
                )
            else Value(Coin(lovelace))
        Utxo(TransactionInput(hash, idx), TransactionOutput.Shelley(addr, value))
    }

    // --- singleton bootstrap output ---

    test("the genesis root is 32 zero bytes") {
        assert(BridgeBootstrap.EmptyRoot == ByteString.fromArray(Array.fill[Byte](32)(0)))
    }

    test("the bootstrap output sits at the singleton's own script address, no stake credential") {
        val (addr, _, _) = BridgeBootstrap.bridgeStateOutput(contract, network, bootstrapState)
        assert(addr == Address(network, Credential.ScriptHash(contract.policyId)))
        assert(addr == contract.address(network))
    }

    test("the bootstrap output carries exactly one \"BSS\" token plus min-ADA ([BSS-5])") {
        val (_, value, _) = BridgeBootstrap.bridgeStateOutput(contract, network, bootstrapState)
        val asset = AssetName(BridgeStateContract.assetName)
        assert(value.asset(contract.policyId, asset) == 1L)
        assert(value.coin.value == BridgeBootstrap.BootstrapLovelace)
        assert(BridgeStateContract.assetName == ByteString.fromString("BSS"))
    }

    test("the bootstrap datum is the operator-supplied BridgeState, encoded as Constr 0") {
        val (_, _, datum) = BridgeBootstrap.bridgeStateOutput(contract, network, bootstrapState)
        assert(
          datum == Data.Constr(
            0,
            scalus.cardano.onchain.plutus.prelude.List(
              Data.B(bootstrapState.spiRoot),
              Data.B(bootstrapState.cpoRoot),
              Data.B(bootstrapState.treasuryUtxoId),
              Data.I(bootstrapState.treasuryAmount)
            )
          )
        )
    }

    // --- Treasury state genesis output ([TSY-5] – [TSY-8]) ---
    //
    // The mint handler filters the outputs by its OWN script credential and then requires exactly
    // one, so a stake credential on this address does not just drift a reader — it changes the
    // address the deployer pays to while [TSY-5] compares against the bare one, and genesis fails.

    private val configPolicy =
        ByteString.fromHex("22222222222222222222222222222222222222222222222222222222")
    private val treasuryContract =
        TreasuryInfoContract(blueprint, oneShot.id.hash, oneShot.idx, configPolicy)
    private val yFederation = ByteString.fromHex("bb" * 32)
    private val genesisTreasury =
        TreasuryInfoDatum(BridgeBootstrap.EmptyRoot, yFederation)

    test("the treasury state output sits at its own script address, no stake credential ([TSY-5])") {
        val (addr, _, _) =
            BridgeBootstrap.treasuryStateOutput(treasuryContract, network, genesisTreasury)
        assert(addr == Address(network, Credential.ScriptHash(treasuryContract.policyId)))
        assert(addr == treasuryContract.address(network))
    }

    test("the treasury state output carries exactly one \"BFRTRY\" token plus min-ADA ([TSY-6])") {
        val (_, value, _) =
            BridgeBootstrap.treasuryStateOutput(treasuryContract, network, genesisTreasury)
        val asset = AssetName(TreasuryInfoContract.StateAssetName)
        assert(value.asset(treasuryContract.policyId, asset) == 1L)
        // [TSY-6] flattens the whole non-ADA value and expects a single entry, so any second asset
        // — even one of this same policy — fails the mint.
        assert(value.assets.assets.values.map(_.size).sum == 1)
        assert(value.coin.value == BridgeBootstrap.BootstrapLovelace)
        assert(TreasuryInfoContract.StateAssetName == ByteString.fromString("BFRTRY"))
    }

    test("the genesis TreasuryInfoDatum is Constr 0 of [identity root, frost key] ([TSY-1])") {
        val (_, _, datum) =
            BridgeBootstrap.treasuryStateOutput(treasuryContract, network, genesisTreasury)
        // Pinned against the literal encoding, not against `genesisTreasury.toData`: this is the
        // shape `treasury.ak` decodes and heimdall's Rust builder writes, so a derivation change
        // that silently re-ordered or wrapped the fields must fail here.
        assert(
          datum == Data.Constr(
            0,
            scalus.cardano.onchain.plutus.prelude.List(
              Data.B(BridgeBootstrap.EmptyRoot),
              Data.B(yFederation)
            )
          )
        )
    }

    test("TreasuryInfoDatum round-trips through Data, fields by name ([LIB-1])") {
        val back = genesisTreasury.toData.to[TreasuryInfoDatum]
        assert(back.bifrostIdentityRoot == BridgeBootstrap.EmptyRoot)
        assert(back.currentSposFrostKey == yFederation)
        assert(back == genesisTreasury)
    }

    test("a non-32-byte datum field is rejected before it can be submitted ([TSY-8])") {
        // On-chain this aborts the mint and burns the deploy's fee; failing here instead keeps a
        // malformed genesis off the chain entirely.
        assertThrows[IllegalArgumentException] {
            BridgeBootstrap.treasuryStateOutput(
              treasuryContract,
              network,
              TreasuryInfoDatum(BridgeBootstrap.EmptyRoot, ByteString.fromHex("bb" * 31))
            )
        }
        assertThrows[IllegalArgumentException] {
            BridgeBootstrap.treasuryStateOutput(
              treasuryContract,
              network,
              TreasuryInfoDatum(ByteString.fromHex(""), yFederation)
            )
        }
    }

    test("the treasury mint redeemer is the field-less Constr 0 the handler ignores") {
        assert(
          TreasuryInfoContract.MintRedeemer ==
              Data.Constr(0, scalus.cardano.onchain.plutus.prelude.List())
        )
    }

    // --- one-shot selection ---

    test("pickOneShot takes the largest clean pure-ADA UTxO") {
        val small = utxo(0x01, 0, 6_000_000L)
        val big = utxo(0x02, 0, 40_000_000L)
        assert(BridgeBootstrap.pickOneShot(Seq(small, big), Set.empty).contains(big))
    }

    test("pickOneShot skips UTxOs holding native assets") {
        val withAsset = utxo(0x03, 0, 90_000_000L, assets = true)
        val clean = utxo(0x04, 0, 6_000_000L)
        assert(BridgeBootstrap.pickOneShot(Seq(withAsset, clean), Set.empty).contains(clean))
    }

    test("pickOneShot skips UTxOs below the minimum lovelace") {
        val tiny = utxo(0x05, 0, BridgeBootstrap.MinOneShotLovelace - 1)
        assert(BridgeBootstrap.pickOneShot(Seq(tiny), Set.empty).isEmpty)
    }

    // Spending a CIP-33 reference-script UTxO destroys a deployed reference script and makes the
    // builder under-estimate the Conway reference-script fee surcharge.
    test("pickOneShot never returns an excluded (reference-script) UTxO") {
        val refScript = utxo(0x06, 0, 50_000_000L)
        val clean = utxo(0x07, 0, 6_000_000L)
        val picked = BridgeBootstrap.pickOneShot(Seq(refScript, clean), Set(refScript.input))
        assert(picked.contains(clean))
    }

    test("pickOneShot returns None when nothing qualifies") {
        assert(BridgeBootstrap.pickOneShot(Seq.empty, Set.empty).isEmpty)
    }
}
