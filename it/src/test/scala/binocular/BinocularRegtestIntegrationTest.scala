package binocular

import binocular.cli.CommandHelpers
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.{BlockHeader as _, *}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.testing.kit.Party
import scalus.testing.yaci.{YaciConfig, YaciDevKit}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import cats.syntax.either.*

/** Integration test that exercises the full Binocular oracle lifecycle using a real bitcoind
  * regtest node alongside Yaci DevKit.
  *
  * Unlike fixture-based tests, this uses live Bitcoin RPC to fetch real block headers, performs
  * full PoW validation (testingMode=false), and tests the complete add-blocks → wait → promote
  * cycle.
  */
class BinocularRegtestIntegrationTest extends AnyFunSuite with YaciDevKit {

    override protected def yaciConfig: YaciConfig = YaciConfig(
      containerName = "binocular-regtest-yaci-devkit",
      reuseContainer = false
    )

    // ===== Bitcoind Regtest Manager =====

    /** Manages a bitcoind regtest subprocess for integration testing. */
    class RegtestBitcoindManager {
        private val rpcPort = 18543
        private val rpcUser = "test"
        private val rpcPassword = "test"
        private val dataDir = os.temp.dir(prefix = "binocular-regtest-")
        private var subProcess: Option[os.SubProcess] = None

        def start(): Unit = {
            println(s"[bitcoind] Starting regtest node, dataDir=$dataDir, rpcPort=$rpcPort")
            val proc = os
                .proc(
                  "bitcoind",
                  "-regtest",
                  s"-datadir=$dataDir",
                  s"-rpcport=$rpcPort",
                  s"-rpcuser=$rpcUser",
                  s"-rpcpassword=$rpcPassword",
                  "-listen=0",
                  "-txindex=0",
                  "-server=1",
                  "-fallbackfee=0.0001",
                  "-daemon=0"
                )
                .spawn(stdout = os.root / "dev" / "null", stderr = os.root / "dev" / "null")
            subProcess = Some(proc)
            waitForBitcoindReady()
            bitcoinCli("createwallet", "test")
        }

        private def waitForBitcoindReady(): Unit = {
            val maxAttempts = 60
            var attempts = 0
            while attempts < maxAttempts do {
                try {
                    val result = bitcoinCli("getblockchaininfo")
                    if result.contains("\"chain\"") then {
                        println(s"[bitcoind] Ready after $attempts attempts")
                        return
                    }
                } catch { case _: Exception => }
                Thread.sleep(500)
                attempts += 1
            }
            throw new RuntimeException(
              s"bitcoind did not become ready after ${maxAttempts * 500}ms"
            )
        }

        def bitcoinCli(args: String*): String = {
            val allArgs = Seq(
              "bitcoin-cli",
              "-regtest",
              s"-rpcport=$rpcPort",
              s"-rpcuser=$rpcUser",
              s"-rpcpassword=$rpcPassword"
            ) ++ args
            os.proc(allArgs)
                .call(timeout = 60000, stderr = os.root / "dev" / "null")
                .out
                .text()
        }

        def generateBlocks(count: Int): Unit = {
            val address = bitcoinCli("getnewaddress").trim
            println(s"[bitcoind] Generating $count blocks to $address...")
            bitcoinCli("generatetoaddress", count.toString, address)
            println(s"[bitcoind] Generated $count blocks")
        }

        def createRpc()(using ec: ExecutionContext): SimpleBitcoinRpc = {
            new SimpleBitcoinRpc(
              BitcoinNodeConfig(
                url = s"http://127.0.0.1:$rpcPort",
                username = rpcUser,
                password = rpcPassword,
                network = "regtest"
              )
            )
        }

        def stop(): Unit = {
            println("[bitcoind] Stopping...")
            try {
                bitcoinCli("stop")
                Thread.sleep(2000)
            } catch { case _: Exception => }
            subProcess.foreach(_.destroy(shutdownGracePeriod = 0))
            // Clean up temp directory
            try { os.remove.all(dataDir) }
            catch { case _: Exception => }
            println("[bitcoind] Stopped and cleaned up")
        }
    }

    /** Fetch raw block headers from regtest via RPC and convert to BlockHeader objects. */
    private def fetchHeadersFromRegtest(
        rpc: SimpleBitcoinRpc,
        fromHeight: Int,
        toHeight: Int
    )(using ec: ExecutionContext): Seq[BlockHeader] = {
        (fromHeight to toHeight).map { height =>
            val hashHex = rpc.getBlockHash(height).await(10.seconds)
            val rawHex = rpc.getBlockHeaderRaw(hashHex).await(10.seconds)
            BlockHeader(ByteString.fromHex(rawHex))
        }
    }

    // ===== Test =====

    test("full lifecycle with regtest bitcoind: add blocks, wait, promote") {
        // Check that bitcoind is available
        val bitcoindAvailable = Try {
            os.proc("which", "bitcoind").call(check = false).exitCode == 0
        }.getOrElse(false)
        assume(bitcoindAvailable, "bitcoind not found on PATH, skipping regtest test")

        val bitcoind = new RegtestBitcoindManager()
        try {
            // Phase 1: Start bitcoind regtest and generate blocks
            bitcoind.start()
            bitcoind.generateBlocks(200)

            // Phase 2: Start Yaci DevKit
            val yaciCtx = createYaciContext()
            given ec: ExecutionContext = yaciCtx.provider.executionContext

            val rpc = bitcoind.createRpc()

            // Phase 3: Initialize oracle
            val tipInfo = rpc.getBlockchainInfo().await(10.seconds)
            val bitcoinTip = tipInfo.blocks
            println(s"[Test] Bitcoin regtest tip: $bitcoinTip")

            val startHeight = bitcoinTip - 150
            println(s"[Test] Initializing oracle at height $startHeight")

            val initialState = BitcoinChainState
                .getInitialChainState(rpc, startHeight)
                .await(30.seconds)

            // Init oracle with NFT using regtest params
            val aliceUtxos =
                yaciCtx.provider.findUtxos(yaciCtx.alice.address).await(30.seconds)
            val (seedInput, seedOutput) = aliceUtxos.toOption.get.head
            val seedUtxo = Utxo(seedInput, seedOutput)
            val txOutRef = TxOutRef(TxId(seedInput.transactionId), BigInt(seedInput.index))
            val testOwner = PubKeyHash(Party.Alice.addrKeyHash)

            val params = BitcoinValidatorParams(
              maturationConfirmations = 100,
              challengeAging = 60, // 60 seconds for faster testing
              oneShotTxOutRef = txOutRef,
              closureTimeout = 30 * 24 * 60 * 60,
              owner = testOwner,
              powLimit = BitcoinHelpers.RegtestPowLimit,
              testingMode = false
            )

            val compiled = BitcoinContract.makeContract(params)
            val script = compiled.script
            val scriptHash = script.scriptHash
            val scriptAddress = compiled.address(Network.Testnet)

            val oracleValue = Value.asset(scriptHash, AssetName.empty, 1, Coin(5_000_000))
            val initTx = TxBuilder(yaciCtx.provider.cardanoInfo)
                .spend(seedUtxo)
                .collaterals(seedUtxo)
                .mint(script, Map(AssetName.empty -> 1L), _ => BigInt(0).toData)
                .payTo(scriptAddress, oracleValue, initialState)
                .complete(yaciCtx.provider, yaciCtx.alice.address)
                .await(120.seconds)
                .sign(yaciCtx.alice.signer)
                .transaction

            val initTxHash = OracleTransactions.submitTx(yaciCtx.provider, initTx).valueOr { err =>
                fail(s"Failed to init oracle: $err")
            }
            println(s"[Test] Oracle initialized: $initTxHash")
            val initStatus = yaciCtx.provider
                .pollForConfirmation(
                  TransactionHash.fromHex(initTxHash),
                  maxAttempts = 60,
                  delayMs = 2000
                )
                .await(120.seconds)
            assert(initStatus == TransactionStatus.Confirmed, "Init tx not confirmed")
            Thread.sleep(2000)
            var currentOracleUtxo =
                CommandHelpers.findOracleUtxo(yaciCtx.provider, scriptHash).await(30.seconds)

            // Phase 4: Deploy reference script
            println(s"[Test] Deploying reference script")
            val refScriptResult = OracleTransactions.deployReferenceScript(
              yaciCtx.provider,
              Party.Alice.account,
              compiled
            )
            val referenceScriptUtxo: Utxo = refScriptResult match {
                case Right((txHash, outputIndex, savedOutput)) =>
                    yaciCtx.provider
                        .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 30)
                        .await(60.seconds)
                    Thread.sleep(2000)
                    val refInput = TransactionInput(TransactionHash.fromHex(txHash), outputIndex)
                    Utxo(refInput, savedOutput)
                case Left(err) =>
                    fail(s"Failed to deploy reference script: $err")
            }

            // Phase 5: Add 150 blocks in batches
            val numBlocksToAdd = 150
            val batchSize = 25 // limited by on-chain execution unit budget (grows with tree size)
            var currentState = initialState
            var currentMpf =
                OffChainMPF.empty.insert(
                  initialState.ctx.lastBlockHash,
                  initialState.ctx.lastBlockHash
                )
            val firstBatchTime = System.currentTimeMillis()

            val allHeaders =
                fetchHeadersFromRegtest(rpc, startHeight + 1, startHeight + numBlocksToAdd)
            val batches = allHeaders.grouped(batchSize).toSeq

            batches.zipWithIndex.foreach { case (batch, batchIndex) =>
                println(
                  s"[Test] Processing batch ${batchIndex + 1}/${batches.size} (${batch.size} headers)"
                )
                val headersList = ScalusList.from(batch.toList)
                val (_, validityTime) =
                    OracleTransactions.computeValidityIntervalTime(yaciCtx.provider.cardanoInfo)
                val parentPath = currentState.forkTree.findTipPath

                val (newState, mpfProofs, updatedMpf) =
                    OracleTransactions.computeUpdateWithProofs(
                      currentState,
                      headersList,
                      parentPath,
                      validityTime,
                      currentMpf,
                      params
                    )

                val updateResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                  yaciCtx.provider,
                  Party.Alice.account,
                  compiled,
                  currentOracleUtxo,
                  currentState,
                  newState,
                  headersList,
                  parentPath,
                  validityTime,
                  referenceScriptUtxo,
                  mpfInsertProofs = mpfProofs
                )

                updateResult match {
                    case Right(result) =>
                        val status = yaciCtx.provider
                            .pollForConfirmation(
                              TransactionHash.fromHex(result.txHash),
                              maxAttempts = 30
                            )
                            .await(60.seconds)
                        assert(
                          status == TransactionStatus.Confirmed,
                          s"Update transaction ${result.txHash} did not confirm"
                        )
                        Thread.sleep(2000)
                        currentOracleUtxo = CommandHelpers
                            .findOracleUtxo(yaciCtx.provider, scriptHash)
                            .await(30.seconds)
                        val onChainState = currentOracleUtxo.output.inlineDatum.get.to[ChainState]
                        assert(
                          onChainState.ctx.height == newState.ctx.height &&
                              onChainState.ctx.lastBlockHash == newState.ctx.lastBlockHash,
                          s"On-chain state mismatch after batch ${batchIndex + 1}"
                        )
                        currentState = onChainState
                        currentMpf = updatedMpf
                        println(
                          s"[Test] Batch ${batchIndex + 1} confirmed: height=${currentState.ctx.height}, " +
                              s"forkTree=${currentState.forkTree.blockCount} blocks"
                        )
                    case Left(error) =>
                        fail(s"Failed to submit batch ${batchIndex + 1}: $error")
                }
            }

            // Phase 6: Wait until challenge aging has passed
            val elapsedMs = System.currentTimeMillis() - firstBatchTime
            val waitMs = Math.max(0, 65_000 - elapsedMs) // 65s to be safe (challengeAging=60s)
            if waitMs > 0 then {
                println(s"[Test] Waiting ${waitMs / 1000}s for challenge aging...")
                Thread.sleep(waitMs)
            }

            // Phase 7: Generate 1 new block, then promote in batches of 10
            bitcoind.generateBlocks(1)
            val newTip = rpc.getBlockchainInfo().await(10.seconds).blocks
            val newHeader =
                fetchHeadersFromRegtest(rpc, startHeight + numBlocksToAdd + 1, newTip).head
            val maxPromotionsPerTx = 10
            var headersToSubmit: ScalusList[BlockHeader] = ScalusList.single(newHeader)
            var totalPromoted = BigInt(0)
            var promotionRound = 0

            while {
                promotionRound += 1
                val (_, promoteValidityTime) =
                    OracleTransactions.computeValidityIntervalTime(yaciCtx.provider.cardanoInfo)
                val promotePath = currentState.forkTree.findTipPath

                // Find all promotable blocks, take at most 10
                val allPromoted = OracleTransactions.computePromotedBlocks(
                  currentState,
                  headersToSubmit,
                  promotePath,
                  promoteValidityTime,
                  params
                )
                val promotedThisRound = allPromoted.take(maxPromotionsPerTx)
                val numPromoted = promotedThisRound.length

                if numPromoted > 0 then {
                    // Generate MPF proofs for the limited set
                    var mpf = currentMpf
                    val proofsBuilder = scala.collection.mutable.ListBuffer[ScalusList[ProofStep]]()
                    promotedThisRound.foreach { block =>
                        val proof = mpf.proveNonMembership(block.hash)
                        proofsBuilder += proof
                        mpf = mpf.insert(block.hash, block.hash)
                    }
                    val mpfProofs = ScalusList.from(proofsBuilder.toList)

                    val newState = BitcoinValidator.computeUpdate(
                      currentState,
                      headersToSubmit,
                      promotePath,
                      mpfProofs,
                      promoteValidityTime,
                      params
                    )

                    val result = OracleTransactions.buildAndSubmitUpdateTransaction(
                      yaciCtx.provider,
                      Party.Alice.account,
                      compiled,
                      currentOracleUtxo,
                      currentState,
                      newState,
                      headersToSubmit,
                      promotePath,
                      promoteValidityTime,
                      referenceScriptUtxo,
                      mpfInsertProofs = mpfProofs
                    )

                    result match {
                        case Right(txResult) =>
                            val promoteStatus = yaciCtx.provider
                                .pollForConfirmation(
                                  TransactionHash.fromHex(txResult.txHash),
                                  maxAttempts = 30
                                )
                                .await(60.seconds)
                            assert(
                              promoteStatus == TransactionStatus.Confirmed,
                              s"Promotion tx ${txResult.txHash} did not confirm"
                            )
                            Thread.sleep(2000)
                            currentOracleUtxo = CommandHelpers
                                .findOracleUtxo(yaciCtx.provider, scriptHash)
                                .await(30.seconds)
                            currentState = currentOracleUtxo.output.inlineDatum.get.to[ChainState]
                            currentMpf = mpf
                            totalPromoted += numPromoted
                            println(
                              s"[Test] Promotion round $promotionRound: promoted $numPromoted blocks, " +
                                  s"height=${currentState.ctx.height}, " +
                                  s"forkTree=${currentState.forkTree.blockCount} blocks"
                            )
                        case Left(error) =>
                            fail(s"Failed to submit promotion round $promotionRound: $error")
                    }
                    // After first round, no new headers to add
                    headersToSubmit = ScalusList.Nil
                    numPromoted > 0
                } else false
            } do ()

            // Phase 8: Verify promotion occurred
            println(
              s"[Test] Final state: height=${currentState.ctx.height}, " +
                  s"forkTree=${currentState.forkTree.blockCount} blocks, " +
                  s"totalPromoted=$totalPromoted blocks in $promotionRound rounds"
            )

            assert(
              totalPromoted > 0,
              s"Expected promotion to occur, but no blocks were promoted"
            )
            assert(
              currentState.confirmedBlocksRoot != initialState.confirmedBlocksRoot,
              "Confirmed blocks MPF root should change after promotion"
            )
            assert(
              currentState.forkTree.blockCount < numBlocksToAdd + 1,
              "Fork tree should shrink after promotion"
            )

        } finally {
            bitcoind.stop()
        }
    }
}
