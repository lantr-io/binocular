package binocular

import binocular.cli.CommandHelpers
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.testing.integration.YaciTestContext
import scalus.testing.kit.Party
import scalus.testing.yaci.{YaciConfig, YaciDevKit}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.utils.await

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/** Integration tests for Binocular Oracle on Yaci DevKit
  *
  * Tests:
  *   1. Sequential single-header oracle updates
  *   2. Batch oracle update with MPF root preservation
  *   3. Full lifecycle with promotion and merkle proofs
  *   4. Standalone merkle proof verification
  *   5. Init failure with invalid fixture height
  */
class BinocularIntegrationTest extends AnyFunSuite with YaciDevKit {

    override protected def yaciConfig: YaciConfig = YaciConfig(
      containerName = "binocular-yaci-devkit",
      reuseContainer = true
    )

    // ===== Helpers =====

    /** Mock Bitcoin RPC that reads from test fixtures */
    class MockBitcoinRpc(fixtureDir: String = "src/test/resources/bitcoin_blocks")(using
        ec: ExecutionContext
    ) {
        private def loadBlockFixture(height: Int): BlockFixture =
            BlockFixture.load(height, fixtureDir)

        private def loadBlockByHash(hash: String): Option[BlockFixture] = {
            val dir = new java.io.File(fixtureDir)
            dir.listFiles()
                .filter(_.getName.startsWith("block_"))
                .filter(_.getName.endsWith(".json"))
                .filterNot(_.getName.contains("merkle_proofs"))
                .map { f => BlockFixture.load(f) }
                .find(_.hash == hash)
        }

        def getBlockHash(height: Int): Future[String] = Future {
            loadBlockFixture(height).hash
        }

        def getBlockHeader(hash: String): Future[BlockHeaderInfo] = Future {
            val fixture = loadBlockByHash(hash).getOrElse(
              throw new RuntimeException(s"Block not found: $hash")
            )
            if fixture.bits.isEmpty then
                throw new RuntimeException(
                  s"Missing required field 'bits' for block ${fixture.height} (${fixture.hash})"
                )
            BlockHeaderInfo(
              hash = fixture.hash,
              height = fixture.height,
              version = fixture.version.toInt,
              merkleroot = fixture.merkleroot,
              time = fixture.timestamp,
              nonce = fixture.nonce,
              bits = fixture.bits,
              difficulty = 0.0,
              previousblockhash = fixture.previousblockhash
            )
        }

        def getBlock(hash: String): Future[BlockInfo] = Future {
            val fixture = loadBlockByHash(hash).getOrElse(
              throw new RuntimeException(s"Block not found: $hash")
            )
            if fixture.bits.isEmpty then
                throw new RuntimeException(
                  s"Missing required field 'bits' for block ${fixture.height} (${fixture.hash})"
                )
            BlockInfo(
              hash = fixture.hash,
              height = fixture.height,
              version = fixture.version.toInt,
              merkleroot = fixture.merkleroot,
              time = fixture.timestamp,
              nonce = fixture.nonce,
              bits = fixture.bits,
              difficulty = 0.0,
              previousblockhash = fixture.previousblockhash,
              tx = fixture.transactions.map { txid =>
                  TransactionInfo(txid = txid, hex = "")
              }
            )
        }

        def getRawTransaction(txid: String): Future[RawTransactionInfo] = Future {
            val dir = new java.io.File(fixtureDir)
            val blockWithTx = dir
                .listFiles()
                .filter(_.getName.startsWith("block_"))
                .filter(_.getName.endsWith(".json"))
                .filterNot(_.getName.contains("merkle_proofs"))
                .map { f => BlockFixture.load(f) }
                .find(_.transactions.contains(txid))
            blockWithTx match {
                case Some(block) =>
                    RawTransactionInfo(
                      txid = txid,
                      hash = txid,
                      hex = "",
                      blockhash = Some(block.hash),
                      confirmations = 10
                    )
                case None =>
                    throw new RuntimeException(s"Transaction not found: $txid")
            }
        }

        def getBlockchainInfo(): Future[BlockchainInfo] = Future {
            val dir = new java.io.File(fixtureDir)
            val maxHeight = dir
                .listFiles()
                .filter(_.getName.startsWith("block_"))
                .filter(_.getName.endsWith(".json"))
                .filterNot(_.getName.contains("merkle_proofs"))
                .map { f => BlockFixture.load(f) }
                .map(_.height)
                .maxOption
                .getOrElse(0)
            val bestBlock = loadBlockFixture(maxHeight)
            BlockchainInfo(
              chain = "test",
              blocks = maxHeight,
              headers = maxHeight,
              bestblockhash = bestBlock.hash
            )
        }
    }

    private def fetchInitialState(mockRpc: MockBitcoinRpc, startHeight: Int)(using
        ec: ExecutionContext
    ): ChainState = {
        BitcoinChainState
            .getInitialChainState(
              new SimpleBitcoinRpc(
                BitcoinNodeConfig(
                  url = "mock://rpc",
                  username = "test",
                  password = "test",
                  network = "testnet"
                )
              ) {
                  override def getBlockHash(height: Int) = mockRpc.getBlockHash(height)
                  override def getBlockHeader(hash: String) = mockRpc.getBlockHeader(hash)
              },
              startHeight
            )
            .await(30.seconds)
    }

    private def fetchHeaders(mockRpc: MockBitcoinRpc, fromHeight: Int, toHeight: Int)(using
        ec: ExecutionContext
    ): Seq[BlockHeader] = {
        Future
            .sequence(
              (fromHeight to toHeight).map { height =>
                  for {
                      hashHex <- mockRpc.getBlockHash(height)
                      headerInfo <- mockRpc.getBlockHeader(hashHex)
                  } yield BitcoinChainState.convertHeader(headerInfo)
              }
            )
            .await(120.seconds)
    }

    /** Submit an oracle update, wait for confirmation, verify on-chain state matches. */
    private def submitOracleUpdate(
        ctx: YaciTestContext,
        scriptAddress: Address,
        script: Script.PlutusV3,
        oracleUtxo: Utxo,
        currentState: ChainState,
        newState: ChainState,
        headers: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityTime: BigInt,
        params: BitcoinValidatorParams,
        referenceScriptUtxo: Option[Utxo] = None,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    ): (Utxo, ChainState) = {
        val updateResult = OracleTransactions.buildAndSubmitUpdateTransaction(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          oracleUtxo,
          currentState,
          newState,
          headers,
          parentPath,
          validityTime,
          script,
          referenceScriptUtxo,
          mpfInsertProofs = mpfInsertProofs
        )

        updateResult match {
            case Right(txHash) =>
                given ExecutionContext = ctx.provider.executionContext
                val status = ctx.provider
                    .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 30)
                    .await(60.seconds)
                assert(
                  status == TransactionStatus.Confirmed,
                  s"Update transaction $txHash did not confirm"
                )
                Thread.sleep(2000)

                val newOracleUtxo = CommandHelpers.findOracleUtxo(ctx.provider, script.scriptHash).await(30.seconds)
                val onChainState = newOracleUtxo.output.inlineDatum.get.to[ChainState]

                assert(
                  onChainState.blockHeight == newState.blockHeight &&
                      onChainState.blockHash == newState.blockHash &&
                      onChainState.forkTree.blockCount == newState.forkTree.blockCount,
                  s"On-chain state does not match off-chain computed state!\n" +
                      s"  Off-chain: height=${newState.blockHeight}, hash=${newState.blockHash.toHex}, forkTree=${newState.forkTree.blockCount}\n" +
                      s"  On-chain:  height=${onChainState.blockHeight}, hash=${onChainState.blockHash.toHex}, forkTree=${onChainState.forkTree.blockCount}"
                )

                (newOracleUtxo, onChainState)

            case Left(error) =>
                fail(s"Failed to submit oracle update: $error")
        }
    }

    private def initOracleWithNft(
        ctx: YaciTestContext,
        genesisState: ChainState,
        lovelaceAmount: Long = 5_000_000L
    ): (Utxo, Script.PlutusV3, Address, BitcoinValidatorParams) = {
        given ec: ExecutionContext = ctx.provider.executionContext

        val aliceUtxos = ctx.provider.findUtxos(ctx.alice.address).await(30.seconds)
        val (seedInput, seedOutput) = aliceUtxos.toOption.get.head
        val seedUtxo = Utxo(seedInput, seedOutput)

        val txOutRef = TxOutRef(
          TxId(seedInput.transactionId),
          BigInt(seedInput.index)
        )
        val testOwner = PubKeyHash(Party.Alice.addrKeyHash)
        val params = BitcoinValidatorParams(
          maturationConfirmations = 100,
          challengeAging = 30,
          oneShotTxOutRef = txOutRef,
          closureTimeout = 30 * 24 * 60 * 60,
          owner = testOwner,
          powLimit = BitcoinHelpers.PowLimit
        )

        val script = BitcoinContract.makeContract(params).script
        val scriptHash = script.scriptHash
        val scriptAddress = Address(Network.Testnet, Credential.ScriptHash(scriptHash))

        val oracleValue = Value.asset(scriptHash, AssetName.empty, 1, Coin(lovelaceAmount))

        val tx = TxBuilder(ctx.provider.cardanoInfo)
            .spend(seedUtxo)
            .collaterals(seedUtxo)
            .mint(script, Map(AssetName.empty -> 1L), _ => BigInt(0).toData)
            .payTo(scriptAddress, oracleValue, genesisState)
            .complete(ctx.provider, ctx.alice.address)
            .await(120.seconds)
            .sign(ctx.alice.signer)
            .transaction

        val txHash = OracleTransactions.submitTx(ctx.provider, tx) match {
            case Right(h)  => h
            case Left(err) => throw new RuntimeException(s"Failed to init oracle with NFT: $err")
        }

        println(s"[initOracleWithNft] Oracle initialized: $txHash")
        ctx.provider
            .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 30)
            .await(60.seconds)
        Thread.sleep(2000)

        val oracleUtxo = CommandHelpers.findOracleUtxo(ctx.provider, scriptHash).await(30.seconds)
        (oracleUtxo, script, scriptAddress, params)
    }

    // ===== Tests =====

    test("sequential single-header oracle updates") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        val startHeight = 866970
        val mockRpc = new MockBitcoinRpc()

        val initialState = fetchInitialState(mockRpc, startHeight)
        val (oracleUtxo, itScript, scriptAddress, itParams) =
            initOracleWithNft(ctx, initialState)

        println(s"[Test] Oracle initialized at height $startHeight")

        val headersToSubmit = Seq(866971, 866972)
        var currentState = initialState
        var currentOracleUtxo = oracleUtxo

        headersToSubmit.zipWithIndex.foreach { case (height, idx) =>
            println(
              s"\n--- Submitting header ${idx + 1}/${headersToSubmit.length} (height $height) ---"
            )

            val header = fetchHeaders(mockRpc, height, height).head
            val headersList = ScalusList.single(header)

            val (_, validityTime) =
                OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)
            val parentPath = currentState.forkTree.findTipPath
            val newState = OracleTransactions.applyHeaders(
              currentState,
              headersList,
              parentPath,
              validityTime,
              itParams
            )

            val (newUtxo, onChainState) = submitOracleUpdate(
              ctx,
              scriptAddress,
              itScript,
              currentOracleUtxo,
              currentState,
              newState,
              headersList,
              parentPath,
              validityTime,
              itParams
            )

            currentOracleUtxo = newUtxo
            currentState = onChainState
        }

        assert(
          currentState.forkTree.blockCount == headersToSubmit.length,
          s"Expected ${headersToSubmit.length} blocks in forks tree"
        )

        println(s"Successfully submitted all ${headersToSubmit.length} headers sequentially!")
    }

    test("batch oracle update preserves MPF root") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        val startHeight = 866970
        val updateToHeight = 866973
        val mockRpc = new MockBitcoinRpc()

        val initialState = fetchInitialState(mockRpc, startHeight)
        val (oracleUtxo, itScript, scriptAddress, itParams) =
            initOracleWithNft(ctx, initialState)

        // Verify initial MPF root
        val initialOnChain = oracleUtxo.output.inlineDatum.get.to[ChainState]
        assert(
          initialOnChain.confirmedBlocksRoot.size == 32,
          "Initial MPF root should be 32 bytes"
        )
        assert(
          initialOnChain.confirmedBlocksRoot == initialState.confirmedBlocksRoot,
          "On-chain MPF root should match initial state"
        )

        // Fetch and submit batch of headers
        val headers = fetchHeaders(mockRpc, startHeight + 1, updateToHeight)
        val headersList = ScalusList.from(headers.toList)

        val (_, validityTime) =
            OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)
        val parentPath = initialState.forkTree.findTipPath
        val newState = OracleTransactions.applyHeaders(
          initialState,
          headersList,
          parentPath,
          validityTime,
          itParams
        )

        val (_, onChainState) = submitOracleUpdate(
          ctx,
          scriptAddress,
          itScript,
          oracleUtxo,
          initialState,
          newState,
          headersList,
          parentPath,
          validityTime,
          itParams
        )

        assert(
          onChainState.forkTree.blockCount == headers.size,
          s"Forks tree should contain ${headers.size} new blocks, but has ${onChainState.forkTree.blockCount}"
        )

        assert(
          onChainState.blockHeight == startHeight,
          s"Height should remain $startHeight, got ${onChainState.blockHeight}"
        )

        assert(
          onChainState.confirmedBlocksRoot == initialState.confirmedBlocksRoot,
          "MPF root should be unchanged (no promotion occurred)"
        )

        println(s"Batch update of ${headers.size} headers verified, MPF root unchanged")
    }

    test("full lifecycle with promotion and proofs") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        val startHeight = 866970
        val totalBlocks = 105
        val batchSize = 10
        val finalHeight = startHeight + totalBlocks
        val mockRpc = new MockBitcoinRpc()

        val initialState = fetchInitialState(mockRpc, startHeight)
        val (initOracleUtxo, itScript, scriptAddress, itParams) =
            initOracleWithNft(ctx, initialState)
        var currentOracleUtxo: Utxo = initOracleUtxo

        // Deploy reference script to reduce transaction size
        println(s"[Test] Deploying reference script")
        val refScriptResult = OracleTransactions.deployReferenceScript(
          ctx.alice.signer,
          ctx.provider,
          ctx.alice.address,
          scriptAddress,
          itScript
        )

        val referenceScriptUtxo: Option[Utxo] = refScriptResult match {
            case Right((txHash, outputIndex, savedOutput)) =>
                ctx.provider
                    .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 30)
                    .await(60.seconds)
                Thread.sleep(2000)
                val refInput = TransactionInput(TransactionHash.fromHex(txHash), outputIndex)
                Some(Utxo(refInput, savedOutput))
            case Left(err) =>
                fail(s"Failed to deploy reference script: $err")
        }

        println(s"[Test] Adding $totalBlocks blocks in batches of $batchSize")

        val allHeaders = fetchHeaders(mockRpc, startHeight + 1, finalHeight)
        val batches = allHeaders.grouped(batchSize).toSeq
        var currentState = initialState
        var currentMpf = OffChainMPF.empty
            .insert(initialState.blockHash, initialState.blockHash)

        batches.zipWithIndex.foreach { case (batch, batchIndex) =>
            println(
              s"[Test] Processing batch ${batchIndex + 1}/${batches.size} (${batch.size} headers)"
            )

            val headersList = ScalusList.from(batch.toList)
            val validityTime: BigInt =
                OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)._2
            val parentPath = currentState.forkTree.findTipPath

            val (newState, mpfProofs, updatedMpf) = OracleTransactions.computeUpdateWithProofs(
              currentState,
              headersList,
              parentPath,
              validityTime,
              currentMpf,
              itParams
            )

            val (newUtxo, onChainState) = submitOracleUpdate(
              ctx,
              scriptAddress,
              itScript,
              currentOracleUtxo,
              currentState,
              newState,
              headersList,
              parentPath,
              validityTime,
              itParams,
              referenceScriptUtxo,
              mpfProofs
            )

            currentState = onChainState
            currentOracleUtxo = newUtxo
            currentMpf = updatedMpf
        }

        // Verify that promotion occurred
        val heightIncrease = currentState.blockHeight - initialState.blockHeight
        assert(
          heightIncrease > 0,
          s"Expected promotion to increase confirmed height, but got: initial=${initialState.blockHeight}, final=${currentState.blockHeight}"
        )
        println(s"[Test] Promotion detected: height increased by $heightIncrease blocks")

        assert(
          currentState.confirmedBlocksRoot != initialState.confirmedBlocksRoot,
          "Confirmed blocks MPF root should change after promotion"
        )

        // Verify merkle proofs for the initial block (should be promoted/confirmed)
        val blockHash = mockRpc.getBlockHash(startHeight).await(10.seconds)
        val blockInfo = mockRpc.getBlock(blockHash).await(10.seconds)
        assert(blockInfo.tx.nonEmpty, "Block has no transactions")

        val txHashes = blockInfo.tx.map { tx =>
            ByteString.fromHex(tx.txid).reverse
        }

        val merkleTree = MerkleTree.fromHashes(txHashes)
        val merkleRoot = merkleTree.getMerkleRoot

        val testCount = Math.min(5, blockInfo.tx.length)
        for txIndex <- 0 until testCount do {
            val testTxHash = ByteString.fromHex(blockInfo.tx(txIndex).txid).reverse
            val testProof = merkleTree.makeMerkleProof(txIndex)
            val calculatedRoot =
                MerkleTree.calculateMerkleRootFromProof(txIndex, testTxHash, testProof)
            assert(calculatedRoot == merkleRoot, s"Proof failed for tx at index $txIndex")
        }
        println(s"[Test] All $testCount Merkle proofs verified")

        // Verify block is promoted (not in forks tree)
        val blockHashBytes = ByteString.fromHex(blockHash).reverse
        assert(
          !currentState.forkTree.existsHash(blockHashBytes),
          "Block should not be in forks tree after promotion"
        )

        println(s"[Test] Full lifecycle completed: $totalBlocks blocks, $heightIncrease promoted")
    }
}
