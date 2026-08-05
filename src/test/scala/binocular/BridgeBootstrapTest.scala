package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Coin, Credential, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}

/** Tests for the shared bridge-bootstrap pieces.
  *
  * The trie output shape is what `bootstrap-completed-peg-outs` and `deploy-bridge` must agree on,
  * and it is exactly what `completed-peg-outs-merkle-tree.ak::mint` checks: one output at the
  * policy's own script address, the single "CPO" token, and an inline datum of the all-zero root.
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
    private val contract = CompletedPegOutsContract(blueprint, tmPolicy, oneShot)
    private val network = Network.Testnet

    private def utxo(txHashByte: Int, idx: Int, lovelace: Long, assets: Boolean = false): Utxo = {
        val hash = TransactionHash.fromHex(f"$txHashByte%02x" * 32)
        val addr = Address(network, Credential.ScriptHash(contract.policyId))
        val value =
            if assets then
                Value(Coin(lovelace)) + Value.asset(
                  contract.policyId,
                  AssetName(CompletedPegOutsContract.assetName),
                  1L
                )
            else Value(Coin(lovelace))
        Utxo(TransactionInput(hash, idx), TransactionOutput.Shelley(addr, value))
    }

    // --- trie bootstrap output ---

    test("the genesis root is 32 zero bytes, the literal the Aiken mint handler pins") {
        assert(BridgeBootstrap.EmptyRoot == ByteString.fromArray(Array.fill[Byte](32)(0)))
    }

    test("the bootstrap output sits at the trie policy's own script address, no stake credential") {
        val (addr, _, _) = BridgeBootstrap.completedPegOutsOutput(contract, network)
        assert(addr == Address(network, Credential.ScriptHash(contract.policyId)))
        assert(addr == contract.address(network))
    }

    test("the bootstrap output carries exactly one \"CPO\" token plus min-ADA") {
        val (_, value, _) = BridgeBootstrap.completedPegOutsOutput(contract, network)
        val asset = AssetName(CompletedPegOutsContract.assetName)
        assert(value.asset(contract.policyId, asset) == 1L)
        assert(value.coin.value == BridgeBootstrap.BootstrapLovelace)
        assert(CompletedPegOutsContract.assetName == ByteString.fromString("CPO"))
    }

    test("the bootstrap datum is the empty-root CompletedPegOutsTrieDatum") {
        val (_, _, datum) = BridgeBootstrap.completedPegOutsOutput(contract, network)
        assert(
          datum == Data.Constr(
            0,
            scalus.cardano.onchain.plutus.prelude.List(
              Data.B(BridgeBootstrap.EmptyRoot)
            )
          )
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
