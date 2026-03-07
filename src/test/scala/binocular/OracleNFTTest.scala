package binocular

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.RedeemerPurpose.ForMint
import scalus.cardano.txbuilder.txBuilder
import scalus.testing.kit.Party.Alice
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.utils.await

/** Tests for one-shot Oracle NFT minting policy using TxBuilder and UPLC evaluation.
  *
  * Uses the same testing pattern as Scalus HTLC tests: build transactions with TxBuilder, extract
  * ScriptContext via getScriptContextV3, and evaluate the compiled UPLC program.
  *
  * The BitcoinValidator's mint method enforces:
  *   1. The parameterized TxOutRef must be consumed as input (one-shot guarantee)
  *   2. Exactly 1 token is minted with empty token name
  *   3. The oracle output (at redeemer index) contains the NFT
  *   4. The oracle output goes to the script address (policyId == script hash)
  *   5. Burning (negative mint qty) is always allowed
  */
class OracleNFTTest extends AnyFunSuite, ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val baseContract = BitcoinContract.contract.withErrorTraces

    private def createProvider: Emulator =
        Emulator.withAddresses(Seq(Alice.address))

    // Pre-compute the applied contract from the deterministic emulator seed UTxO.
    // Emulator.withAddresses creates UTxOs deterministically, so the seed is always the same.
    private val (contract, scriptHash, scriptAddr) = {
        val p = createProvider
        val (input, _) = p.findUtxos(Alice.address).await().toOption.get.head
        val txOutRef = TxOutRef(TxId(input.transactionId), BigInt(input.index))
        val c = baseContract(txOutRef.toData)
        (c, c.script.scriptHash, c.address(env.network))
    }

    test(s"Oracle NFT script size is ${contract.script.script.size} bytes") {
        assert(contract.script.script.size > 0)
    }

    test("mint succeeds when required UTxO is spent and exactly 1 token minted") {
        val provider = createProvider
        val seed = Utxo(provider.findUtxos(Alice.address).await().toOption.get.head)

        val scriptCtx = txBuilder
            .spend(seed)
            .mint(contract, Map(AssetName.empty -> 1L), BigInt(0))
            .payTo(scriptAddr, Value.asset(scriptHash, AssetName.empty, 1, Coin.ada(5)))
            .draft
            .getScriptContextV3(provider.utxos, ForMint(scriptHash))

        val result = contract(scriptCtx.toData).program.evaluateDebug
        assert(result.isSuccess, s"Evaluation failed: ${result.logs}")
    }

    test("mint fails when required UTxO is NOT in inputs") {
        val provider = createProvider
        val seed = Utxo(provider.findUtxos(Alice.address).await().toOption.get.head)

        // Parameterize with a TxOutRef that doesn't match any input
        val fakeTxOutRef = TxOutRef(
          TxId(hex"1111111111111111111111111111111111111111111111111111111111111111"),
          BigInt(0)
        )
        val fakeContract = baseContract(fakeTxOutRef.toData)
        val fakeScriptHash = fakeContract.script.scriptHash
        val fakeScriptAddr = fakeContract.address(env.network)

        val scriptCtx = txBuilder
            .spend(seed)
            .mint(fakeContract, Map(AssetName.empty -> 1L), BigInt(0))
            .payTo(fakeScriptAddr, Value.asset(fakeScriptHash, AssetName.empty, 1, Coin.ada(5)))
            .draft
            .getScriptContextV3(provider.utxos, ForMint(fakeScriptHash))

        val result = fakeContract(scriptCtx.toData).program.evaluateDebug
        assert(!result.isSuccess)
    }

    test("mint fails when minting quantity != 1") {
        val provider = createProvider
        val seed = Utxo(provider.findUtxos(Alice.address).await().toOption.get.head)

        val scriptCtx = txBuilder
            .spend(seed)
            .mint(contract, Map(AssetName.empty -> 2L), BigInt(0))
            .payTo(scriptAddr, Value.asset(scriptHash, AssetName.empty, 2, Coin.ada(5)))
            .draft
            .getScriptContextV3(provider.utxos, ForMint(scriptHash))

        val result = contract(scriptCtx.toData).program.evaluateDebug
        assert(!result.isSuccess)
    }

    test("mint fails when oracle output does not contain NFT") {
        val provider = createProvider
        val seed = Utxo(provider.findUtxos(Alice.address).await().toOption.get.head)

        val scriptCtx = txBuilder
            .spend(seed)
            .mint(contract, Map(AssetName.empty -> 1L), BigInt(0))
            .payTo(scriptAddr, Value.ada(5)) // No NFT in oracle output
            .payTo(Alice.address, Value.asset(scriptHash, AssetName.empty, 1)) // NFT elsewhere
            .draft
            .getScriptContextV3(provider.utxos, ForMint(scriptHash))

        val result = contract(scriptCtx.toData).program.evaluateDebug
        assert(!result.isSuccess)
    }

    test("mint fails when oracle output goes to wrong address") {
        val provider = createProvider
        val seed = Utxo(provider.findUtxos(Alice.address).await().toOption.get.head)

        val scriptCtx = txBuilder
            .spend(seed)
            .mint(contract, Map(AssetName.empty -> 1L), BigInt(0))
            .payTo(Alice.address, Value.asset(scriptHash, AssetName.empty, 1, Coin.ada(5)))
            .draft
            .getScriptContextV3(provider.utxos, ForMint(scriptHash))

        val result = contract(scriptCtx.toData).program.evaluateDebug
        assert(!result.isSuccess)
    }

    test("burn always succeeds (negative quantity)") {
        val provider = createProvider
        val seed = Utxo(provider.findUtxos(Alice.address).await().toOption.get.head)

        val scriptCtx = txBuilder
            .spend(seed)
            .mint(contract, Map(AssetName.empty -> -1L), BigInt(0))
            .draft
            .getScriptContextV3(provider.utxos, ForMint(scriptHash))

        val result = contract(scriptCtx.toData).program.evaluateDebug
        assert(result.isSuccess, s"Burn evaluation failed: ${result.logs}")
    }
}
