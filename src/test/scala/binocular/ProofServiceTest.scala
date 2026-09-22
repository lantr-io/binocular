package binocular

import binocular.bitcoin.*
import binocular.cli.DaemonExecution
import binocular.cli.commands.BridgeSweepSetup
import binocular.oracle.{reverse, BlockHeader as OracleBlockHeader, ChainState}
import binocular.server.{ProofApi, ProofService}
import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.rules.Context
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainProvider, Emulator, UtxoQuery}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*

class ProofServiceTest extends AnyFunSuite with BitcoinValidatorGenerators {
    private given ExecutionContext = DaemonExecution.ec
    private val network = Network.Mainnet
    private val oraclePolicy = ScriptHash.fromHex("01" * 28)
    private val configPolicy = ScriptHash.fromHex("02" * 28)
    private val bssPolicy = ScriptHash.fromHex("03" * 28)
    private val tmPolicy = ScriptHash.fromHex("04" * 28)
    private val configAsset = AssetName(ConfigDatum.ConfigNftAssetName)

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    private def input(seed: Int): TransactionInput =
        TransactionInput(TransactionHash.fromHex(f"$seed%02x" * 32), 0)

    private def output(
        policy: ScriptHash,
        asset: AssetName,
        datum: scalus.uplc.builtin.Data
    ): TransactionOutput =
        TransactionOutput.Babbage(
          Address(network, Credential.ScriptHash(policy)),
          Value.asset(policy, asset, 1, Coin.ada(2)),
          datumOption = Some(DatumOption.Inline(datum)),
          scriptRef = None
        )

    private val cfg = ConfigDatum(
      updateAuth = scalus.cardano.onchain.plutus.prelude.Option.None,
      params = ConfigParams(
        ScheduleParams(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        feeRateSatPerVb = 2,
        perPegoutFee = 100,
        minPegOutFbtc = 1_000,
        baseBanDurationMs = 1,
        maxFaultsBeforePermanent = 3,
        maxValidityWindowMs = 3_600_000,
        federationCsvBlocks = 6,
        peginRefundTimeoutBlocks = 12
      ),
      bridgedTokenPolicy = filled(5, 28),
      completedPegInsPolicy = filled(6, 28),
      bridgeStatePolicy = ByteString.fromArray(bssPolicy.bytes),
      tmScriptHash = ByteString.fromArray(tmPolicy.bytes),
      pegInScriptHash = filled(7, 28),
      pegOutScriptHash = filled(8, 28),
      spoBansPolicyId = filled(9, 28),
      sposRegistryPolicyId = filled(10, 28),
      treasuryInfoPolicyId = filled(11, 28),
      yFederation = filled(12, 32),
      federationOneShot = TxOutRef(TxId(filled(13, 32)), 0)
    )

    private final class SwitchingProvider(initial: Map[TransactionInput, TransactionOutput])
        extends BlockchainProvider {
        private var delegate = new Emulator(initial, Context.testMainnet())

        def switchTo(utxos: Map[TransactionInput, TransactionOutput]): Unit =
            delegate = new Emulator(utxos, Context.testMainnet())

        override def executionContext: ExecutionContext = summon[ExecutionContext]
        override def cardanoInfo: CardanoInfo = delegate.cardanoInfo
        override def fetchLatestParams = delegate.fetchLatestParams
        override def currentSlot = delegate.currentSlot
        override def getDatum(hash: DataHash) = delegate.getDatum(hash)
        override def findUtxos(query: UtxoQuery) = delegate.findUtxos(query)
        override def submit(tx: Transaction) = delegate.submit(tx)
    }

    private final class History extends CpoHistorySource {
        var calls = 0
        var outputs = Seq.empty[ChainOutput]
        override def backend = "test history"
        override def addressHistory(address: String) = {
            calls += 1
            Right(outputs)
        }
    }

    private val depositTxHex =
        "020000000100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff02a086010000000000225120bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000256a23424652cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000"

    private def voutsOf(rawHex: String): Seq[VoutInfo] =
        TreasuryMovementValidator
            .allOutputs(ByteString.fromHex(rawHex))
            .asScala
            .toSeq
            .zipWithIndex
            .map((out, index) => VoutInfo(index, out.scriptPubKey.toHex, 0.0))

    private final case class DepositFixture(rawHex: String) {
        val raw = ByteString.fromHex(rawHex)
        val txidLE = BitcoinHelpers.getTxHash(raw)
        val txid = txidLE.reverse.toHex
        val headerHex = "00000000" + ("00" * 32) + txidLE.toHex + ("00" * 12)
        val blockHashLE =
            BitcoinHelpers.blockHeaderHash(OracleBlockHeader(ByteString.fromHex(headerHex)))
        val blockHash = blockHashLE.reverse.toHex
        val rawInfo = RawTransactionInfo(txid, txid, rawHex, Some(blockHash), 10)
        val block = BlockInfo(
          blockHash,
          100,
          0,
          txid,
          0L,
          0L,
          "",
          0.0,
          None,
          Seq(TransactionInfo(txid, rawHex, voutsOf(rawHex)))
        )
        private val baseState = makeTestChainState(height = 100)
        val state = baseState
            .copy(
              confirmedBlocksRoot = OffChainMPF.empty.insert(blockHashLE, blockHashLE).rootHash,
              ctx = baseState.ctx.copy(lastBlockHash = blockHashLE)
            )
        val outpoint = txidLE ++ hex"00000000"
    }

    private final class Rpc(fixtures: DepositFixture*) extends BitcoinRpc {
        private val byTx: Map[String, DepositFixture] = fixtures.map(f => f.txid -> f).toMap
        private val byBlock: Map[String, DepositFixture] =
            fixtures.map(f => f.blockHash -> f).toMap
        var canonicalBlock = fixtures.head.blockHash
        var blockHashCalls = 0

        override def getBlockHash(height: Int) = {
            blockHashCalls += 1
            Future.successful(canonicalBlock)
        }
        override def getBlockHeaderRaw(hash: String) = Future.successful(byBlock(hash).headerHex)
        override def getBlockHeader(hash: String) = Future.failed(new UnsupportedOperationException)
        override def getBlock(hash: String) = Future.successful(byBlock(hash).block)
        override def getBlockchainInfo() = Future.failed(new UnsupportedOperationException)
        override def getRawTransaction(txid: String) = Future.successful(byTx(txid).rawInfo)
        override def sendRawTransaction(hexString: String) =
            Future.failed(new UnsupportedOperationException)
    }

    private def oracleUtxo(seed: Int, state: ChainState): Utxo =
        Utxo(input(seed), output(oraclePolicy, AssetName.empty, state.toData))

    private def bridgeUtxos(seed: Int, state: BridgeState): Seq[Utxo] =
        Seq(
          Utxo(input(seed), output(configPolicy, configAsset, cfg.toData)),
          Utxo(
            input(seed + 1),
            output(bssPolicy, AssetName(BridgeStateContract.assetName), state.toData)
          )
        )

    private def view(oracle: Utxo, bridge: Seq[Utxo]): Map[TransactionInput, TransactionOutput] =
        (oracle +: bridge).map(u => u.input -> u.output).toMap

    private def service(
        provider: BlockchainProvider,
        history: CpoHistorySource,
        rpc: BitcoinRpc
    ): ProofService =
        new ProofService(
          provider,
          history,
          rpc,
          network,
          oraclePolicy,
          configPolicy,
          configAsset,
          oracleStartHeight = None,
          timeout = 5.seconds
        )

    test("typed deposit proof and JSON share one matching oracle resolution and root-keyed cache") {
        val first = DepositFixture(depositTxHex)
        val second = DepositFixture(depositTxHex.replace("cc" * 32, "dd" * 32))
        val emptyBridge = bridgeUtxos(
          20,
          BridgeState(OffChainMPF.empty.rootHash, filled(0, 32), filled(21, 36), 1)
        )
        val anchor1 = oracleUtxo(30, first.state)
        val anchor2 = oracleUtxo(31, first.state)
        val anchor3 = oracleUtxo(32, second.state)
        val provider = new SwitchingProvider(view(anchor1, emptyBridge))
        val history = new History
        val rpc = new Rpc(first, second)
        val proofs = service(provider, history, rpc)

        val typed1 = proofs.depositProofFor(first.outpoint).toOption.get
        assert(typed1.oracle == binocular.cli.ValidOracleUtxo(anchor1, first.state))
        assert(typed1.oracle.chainState.confirmedBlocksRoot == first.state.confirmedBlocksRoot)
        assert(
          proofs.depositProof(first.outpoint.toHex) == Right(
            ProofApi.depositBundleJson(typed1.bundle, typed1.oracle.chainState.confirmedBlocksRoot)
          )
        )
        assert(rpc.blockHashCalls == 1)

        provider.switchTo(view(anchor2, emptyBridge))
        val typed2 = proofs.depositProofFor(first.outpoint).toOption.get
        assert(typed2.oracle.utxo == anchor2)
        assert(typed2.bundle == typed1.bundle)
        assert(rpc.blockHashCalls == 1, "same root must reuse only the MPF, not the anchor")

        provider.switchTo(view(anchor3, emptyBridge))
        rpc.canonicalBlock = second.blockHash
        val typed3 = proofs.depositProofFor(second.outpoint).toOption.get
        assert(typed3.oracle.utxo == anchor3)
        assert(typed3.oracle.chainState.confirmedBlocksRoot == second.state.confirmedBlocksRoot)
        assert(typed3.bundle.pegInUtxoId == second.outpoint)
        assert(rpc.blockHashCalls == 2, "a new oracle root must rebuild the MPF")
    }

    test("typed swept snapshot and JSON share matching singleton context and head-keyed cache") {
        val empty = OffChainMPF.empty
        val state1 = BridgeState(empty.rootHash, filled(0, 32), filled(40, 36), 1)
        val state2 = state1.copy(treasuryUtxoId = filled(41, 36))
        val oracle = oracleUtxo(42, DepositFixture(depositTxHex).state)
        val bridge1 = bridgeUtxos(50, state1)
        val bridge2 = bridgeUtxos(60, state1)
        val bridge3 = bridgeUtxos(70, state2)
        val provider = new SwitchingProvider(view(oracle, bridge1))
        val history = new History
        val proofs = service(provider, history, new Rpc(DepositFixture(depositTxHex)))

        val snapshot1 = proofs.sweptSnapshot().toOption.get
        assert(
          snapshot1.context == BridgeSweepSetup.SingletonContext(
            bridge1.head,
            cfg,
            bridge1(1),
            state1
          )
        )
        assert(snapshot1.trie.rootHash == snapshot1.context.state.spiRoot)
        assert(history.calls == 1)
        val missing = filled(80, 36)
        assert(
          proofs.spiProof(missing.toHex) ==
              SweptPegInsProofService
                  .proveFrom(snapshot1.trie, missing)
                  .left
                  .map(ProofApi.spiError)
                  .map(ProofApi.spiProofJson)
        )

        provider.switchTo(view(oracle, bridge2))
        val snapshot2 = proofs.sweptSnapshot().toOption.get
        assert(snapshot2.context.configUtxo == bridge2.head)
        assert(snapshot2.context.singletonUtxo == bridge2(1))
        assert(history.calls == 1, "same root/head must reuse only the trie, not either UTxO")

        provider.switchTo(view(oracle, bridge3))
        val snapshot3 = proofs.sweptSnapshot().toOption.get
        assert(snapshot3.context.singletonUtxo == bridge3(1))
        assert(snapshot3.context.state.treasuryUtxoId == state2.treasuryUtxoId)
        assert(snapshot3.trie.rootHash == state2.spiRoot)
        assert(history.calls == 2, "a new singleton head must rebuild even when the root is equal")
    }
}
