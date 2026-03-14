package binocular.cli

import binocular.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.Utxo
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.utils.await

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/** Integration test for UpdateOracle with MPF (MerklePatriciaForestry) verification
  *
  * Tests that the confirmed blocks MPF root is correctly maintained when:
  *   1. Oracle is initialized at block N
  *   2. Multiple blocks are submitted
  *   3. The confirmedBlocksRoot field is properly set
  */
class UpdateOracleWithMerkleTreeTest extends CliIntegrationTestBase {

    test("update-oracle: MPF root is maintained when blocks are added") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        // Use consecutive blocks from our fixtures
        // Start at 866970, update to 866971-866972 (2 blocks)
        val startHeight = 866970
        val updateToHeight = 866972
        val mockRpc = new MockBitcoinRpc()

        println(s"[Test] Step 1: Creating initial oracle at height $startHeight")

        // Create initial oracle
        val initialState = BitcoinChainState
            .getInitialChainState(
              new binocular.SimpleBitcoinRpc(
                binocular.BitcoinNodeConfig(
                  url = "mock://rpc",
                  username = "test",
                  password = "test",
                  network = binocular.BitcoinNetwork.Testnet
                )
              ) {
                  override def getBlockHash(height: Int) = mockRpc.getBlockHash(height)
                  override def getBlockHeader(hash: String) = mockRpc.getBlockHeader(hash)
              },
              startHeight
            )
            .await(30.seconds)

        val scriptAddress = Address.fromBech32(
          binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
        )

        // Submit init transaction
        val initTxResult = OracleTransactions.buildAndSubmitInitTransaction(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          initialState
        )

        val oracleUtxo: Utxo = initTxResult match {
            case Right(txHash) =>
                println(s"[Test] Oracle initialized: $txHash")
                waitForTransaction(ctx.provider, txHash, maxAttempts = 30)
                Thread.sleep(2000) // Wait for indexing
                findOracleUtxo(ctx.provider, scriptAddress, txHash)
            case Left(err) =>
                fail(s"Failed to initialize oracle: $err")
        }

        // Verify initial MPF root state
        val initialChainState = oracleUtxo.output.inlineDatum.get.to[ChainState]

        println(s"[Test] Initial MPF root: ${initialChainState.confirmedBlocksRoot.toHex}")
        println(s"    Initial block: ${initialState.blockHash.toHex}")

        assert(
          initialChainState.confirmedBlocksRoot.size == 32,
          "Initial MPF root should be 32 bytes"
        )
        assert(
          initialChainState.confirmedBlocksRoot == initialState.confirmedBlocksRoot,
          "On-chain MPF root should match initial state"
        )

        println(
          s"[Test] Step 2: Fetching headers for blocks ${startHeight + 1} to $updateToHeight"
        )

        // Fetch headers for update
        val headers = Future
            .sequence(
              (startHeight + 1 to updateToHeight).map { height =>
                  for {
                      hashHex <- mockRpc.getBlockHash(height)
                      headerInfo <- mockRpc.getBlockHeader(hashHex)
                  } yield BitcoinChainState.convertHeader(headerInfo)
              }
            )
            .await(30.seconds)

        val headersList = ScalusList.from(headers.toList)

        println(s"[Test] Fetched ${headers.length} headers")

        println(s"[Test] Step 3: Calculating new ChainState")

        // Calculate new state using shared validator logic
        val (_, validityTime) =
            OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)
        val parentPath = initialState.forkTree.findTipPath
        val params = BitcoinContract.testParams
        val newState =
            OracleTransactions.applyHeaders(initialState, headersList, parentPath, validityTime, params)

        println(s"[Test] New state calculated:")
        println(s"    Old height: ${initialState.blockHeight}")
        println(s"    New height: ${newState.blockHeight}")

        println(s"[Test] Step 4: Submitting update transaction")

        // Submit update transaction with pre-computed state and validity time
        val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          oracleUtxo,
          initialState,
          newState,
          headersList,
          parentPath,
          validityTime,
          BitcoinContract.testTxOutRef
        )

        updateTxResult match {
            case Right(txHash) =>
                println(s"[Test] Oracle updated: $txHash")

                // Wait for confirmation
                val confirmed = waitForTransaction(ctx.provider, txHash, maxAttempts = 30)
                assert(confirmed, s"Update transaction did not confirm")

                Thread.sleep(2000) // Wait for indexing

                println(s"[Test] Step 5: Verifying Merkle tree was updated")

                // Verify new oracle state
                val latestUtxo = findOracleUtxo(ctx.provider, scriptAddress, txHash)
                val chainState = latestUtxo.output.inlineDatum.get.to[ChainState]

                println(s"[Test] Updated ChainState verified:")
                println(s"    Height: ${chainState.blockHeight}")
                println(s"    Hash: ${chainState.blockHash.toHex}")

                // height should not changed, because blocks are not yet confirmed
                assert(
                  chainState.blockHeight == startHeight,
                  s"Updated height mismatch: ${chainState.blockHeight} != $startHeight"
                )

                println(s"[Test] Step 6: Verifying forks tree structure")
                val totalBlocks = chainState.forkTree.blockCount
                println(s"    Total blocks in forks tree: $totalBlocks")
                assert(
                  totalBlocks == headers.size,
                  s"Forks tree should contain ${headers.size} new blocks, but has $totalBlocks"
                )

                println(s"[Test] Forks tree has grown correctly")

                println(s"[Test] Step 7: Verifying MPF root")

                // The MPF root should be unchanged since no blocks have been promoted
                // (only 2 new blocks added, far fewer than 100 required for promotion)
                val mpfRoot = chainState.confirmedBlocksRoot
                println(s"    MPF root: ${mpfRoot.toHex}")
                println(s"    Root length: ${mpfRoot.bytes.length} bytes")

                assert(mpfRoot.bytes.length == 32, "MPF root should be 32 bytes")
                assert(
                  mpfRoot == initialState.confirmedBlocksRoot,
                  "MPF root should be unchanged (no promotion occurred)"
                )

                println(s"[Test] MPF root correctly unchanged (no promotion)")
            case Left(err) =>
                fail(s"Failed to update oracle: $err")
        }
    }

}
