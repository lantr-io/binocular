package binocular.utils

import binocular.{BitcoinNodeConfig, SimpleBitcoinRpc, MerkleTree}
import org.apache.pekko.actor.ActorSystem
import scalus.builtin.ByteString
import upickle.default._

import java.nio.file.{Files, Paths}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/** Utility to fetch real Bitcoin block data from RPC for integration testing.
  *
  * Uses the existing HeaderSyncWithRpc to connect to Bitcoin RPC.
  * Can work with QuickNode, GetBlock, or local Bitcoin Core node.
  *
  * Usage:
  * {{{
  * # With QuickNode (fastest - no sync needed)
  * export bitcoind_rpc_url="https://your-node.quicknode.pro/YOUR-API-KEY/"
  * export bitcoind_rpc_user=""
  * export bitcoind_rpc_password=""
  * sbt "Test/runMain binocular.utils.BitcoinDataFetcher 100000"
  *
  * # With local node
  * export bitcoind_rpc_url="http://localhost:8332"
  * export bitcoind_rpc_user="bitcoin"
  * export bitcoind_rpc_password="your_password"
  * sbt "Test/runMain binocular.utils.BitcoinDataFetcher 170"
  *
  * # Fetch multiple blocks
  * sbt "Test/runMain binocular.utils.BitcoinDataFetcher 170 286 100000"
  * }}}
  */
object BitcoinDataFetcher {

  case class BlockData(
      height: Int,
      hash: String,
      merkleroot: String,
      transactions: Seq[String],
      previousblockhash: Option[String],
      timestamp: Long,
      bits: String,
      difficulty: Double,
      description: String
  ) derives ReadWriter

  case class MerkleProofTestCase(
      blockHeight: Int,
      blockHash: String,
      merkleRoot: String,
      totalTransactions: Int,
      testCases: Seq[TxProofCase]
  ) derives ReadWriter

  case class TxProofCase(
      txIndex: Int,
      txHash: String,
      merkleProof: Seq[String],
      description: String
  ) derives ReadWriter

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("""
        |Usage: BitcoinDataFetcher <options> <block_heights...>
        |
        |Fetch specific blocks:
        |  BitcoinDataFetcher 170 286 100000           # Fetch blocks 170, 286, 100000
        |
        |Fetch a range of blocks:
        |  BitcoinDataFetcher --range 800000 800100   # Fetch blocks 800000-800100 (101 blocks)
        |  BitcoinDataFetcher --range 866870 866970   # Fetch 100 consecutive blocks
        |
        |Configuration (via environment variables):
        |  Option 1 - QuickNode (fastest, no local sync):
        |    export bitcoind_rpc_url="https://your-node.quicknode.pro/YOUR-KEY/"
        |    export bitcoind_rpc_user=""
        |    export bitcoind_rpc_password=""
        |
        |  Option 2 - GetBlock:
        |    export bitcoind_rpc_url="https://btc.getblock.io/YOUR-KEY/mainnet/"
        |    export bitcoind_rpc_user=""
        |    export bitcoind_rpc_password=""
        |
        |  Option 3 - Local Bitcoin Core:
        |    export bitcoind_rpc_url="http://localhost:8332"
        |    export bitcoind_rpc_user="bitcoin"
        |    export bitcoind_rpc_password="your_password"
        |
        |Recommended blocks to fetch:
        |  170      - First Bitcoin transaction (Satoshi to Hal Finney)
        |  286      - Early block with simple merkle tree
        |  100000   - Milestone block with ~4 transactions
        |  125552   - Block from March 2011
        |
        |For integration tests (fetch 100+ consecutive blocks):
        |  BitcoinDataFetcher --range 866870 866970   # Recent blocks for validation testing
        |""".stripMargin)
      sys.exit(1)
    }

    // Parse arguments
    val (blockHeights, outputDir) = if (args(0) == "--range") {
      if (args.length < 3) {
        System.err.println("Error: --range requires start and end heights")
        System.err.println("Example: BitcoinDataFetcher --range 800000 800100")
        sys.exit(1)
      }
      val start = args(1).toInt
      val end = args(2).toInt
      if (start >= end) {
        System.err.println(s"Error: start height ($start) must be less than end height ($end)")
        sys.exit(1)
      }
      val range = start to end
      println(s"Fetching ${range.length} blocks from $start to $end")
      (range.toSeq, "src/test/resources/bitcoin_blocks")
    } else {
      (args.map(_.toInt).toSeq, "src/test/resources/bitcoin_blocks")
    }

    println(s"Fetching ${blockHeights.length} Bitcoin block(s)...")
    println(s"Output directory: $outputDir")
    println()

    // Load configuration
    val config = BitcoinNodeConfig.load() match {
      case Right(cfg) =>
        println(s"✓ Loaded Bitcoin RPC config: $cfg")
        println()
        cfg
      case Left(error) =>
        System.err.println(s"✗ Failed to load Bitcoin RPC configuration: $error")
        System.err.println("\nSee usage above for configuration options.")
        sys.exit(1)
    }

    given system: ActorSystem = ActorSystem("bitcoin-data-fetcher")
    given ec: ExecutionContext = system.dispatcher

    try {
      val timeout = if (blockHeights.length > 50) 15.minutes else 10.minutes
      println(s"Timeout set to: $timeout")
      
      val results = Await.result(
        fetchMultipleBlocks(config, blockHeights, outputDir),
        timeout
      )
      
      println(s"\n${"=" * 60}")
      println(s"✅ Successfully fetched ${results.length} block(s)!")
      println(s"${"=" * 60}\n")
      
      results.foreach { result =>
        println(s"Block ${result.data.height}:")
        println(s"  Hash: ${result.data.hash}")
        println(s"  Transactions: ${result.data.transactions.length}")
        println(s"  Files saved to: ${outputDir}/")
        println(s"    - ${result.blockFile}")
        println(s"    - ${result.proofFile}")
        println()
      }
      
      println("You can now use these JSON files in integration tests!")
      println()
      println(s"Total blocks fetched: ${results.length}")
      println(s"Total transactions: ${results.map(_.data.transactions.length).sum}")
      
    } catch {
      case e: Exception =>
        System.err.println(s"\n✗ Failed: ${e.getMessage}")
        e.printStackTrace()
        sys.exit(1)
    } finally {
      println("\nTerminating...")
      system.terminate()
      Await.result(system.whenTerminated, 10.seconds)
    }
  }

  case class FetchResult(
      data: BlockData,
      blockFile: String,
      proofFile: String
  )

  def fetchBlockData(
      config: BitcoinNodeConfig,
      blockHeight: Int,
      outputDir: String
  )(using ExecutionContext): Future[FetchResult] = {
    
    val startTime = System.currentTimeMillis()
    println(s"Fetching block $blockHeight...")
    
    // Use simple RPC client (no bitcoin-s cookie issues!)
    val rpc = new SimpleBitcoinRpc(config)
    
    for {
      // Fetch block hash
      _ <- Future.successful(println(s"  → Requesting block hash..."))
      blockHashHex <- rpc.getBlockHash(blockHeight)
      _ <- Future.successful(println(s"  ✓ Block hash: $blockHashHex"))
      
      // Fetch full block with transactions
      _ <- Future.successful(println(s"  → Requesting full block data..."))
      block <- rpc.getBlock(blockHashHex)
      _ <- Future.successful(println(s"  ✓ Transactions: ${block.tx.length}"))
      _ <- Future.successful(println(s"  ✓ Merkle root: ${block.merkleroot}"))
      
      // Extract transaction hashes
      txHashes = block.tx.map(_.txid)
      
      // Create block data
      blockData = BlockData(
        height = blockHeight,
        hash = block.hash,
        merkleroot = block.merkleroot,
        transactions = txHashes,
        previousblockhash = block.previousblockhash,
        timestamp = block.time,
        bits = block.bits,
        difficulty = block.difficulty,
        description = getBlockDescription(blockHeight)
      )
      
      // Generate merkle proofs
      _ <- Future.successful(println(s"  → Generating merkle proofs..."))
      merkleTestCase = generateMerkleProofTestCase(blockData)
      _ <- Future.successful(println(s"  ✓ Generated ${merkleTestCase.testCases.length} merkle proof test cases"))
      
      // Save files
      _ <- Future.successful(println(s"  → Saving files..."))
      result <- saveBlockData(blockData, merkleTestCase, outputDir)
      elapsed = System.currentTimeMillis() - startTime
      _ <- Future.successful(println(s"  ✓ Saved to $outputDir/ (${elapsed}ms)"))
      
    } yield result
  }

  def fetchMultipleBlocks(
      config: BitcoinNodeConfig,
      blockHeights: Seq[Int],
      outputDir: String
  )(using ActorSystem, ExecutionContext): Future[Seq[FetchResult]] = {
    
    val total = blockHeights.length
    val startTime = System.currentTimeMillis()
    
    // Fetch blocks sequentially to avoid overwhelming the RPC
    blockHeights.zipWithIndex.foldLeft(Future.successful(Seq.empty[FetchResult])) { 
      case (futureResults, (height, idx)) =>
        for {
          results <- futureResults
          _ = if (idx > 0 && idx % 10 == 0) {
            val elapsed = (System.currentTimeMillis() - startTime) / 1000.0
            val avgTime = elapsed / idx
            val remaining = (total - idx) * avgTime
            println(s"\nProgress: $idx/$total blocks fetched (${(idx * 100.0 / total).round}%)")
            println(s"  Time elapsed: ${elapsed.round}s, avg: ${avgTime.round}s/block, est. remaining: ${remaining.round}s")
          }
          result <- fetchBlockData(config, height, outputDir).recover {
            case e: Exception =>
              System.err.println(s"\n✗ Failed to fetch block $height: ${e.getMessage}")
              throw e
          }
        } yield results :+ result
    }
  }

  private def getBlockDescription(height: Int): String = height match {
    case 170 => "First Bitcoin transaction from Satoshi to Hal Finney (50 BTC)"
    case 286 => "Early Bitcoin block (January 2009)"
    case 100000 => "Bitcoin block 100,000 milestone (December 2010)"
    case 125552 => "Block from March 2011"
    case h if h < 1000 => s"Early Bitcoin block from 2009"
    case h if h < 100000 => s"Historical Bitcoin block from 2009-2010"
    case h if h >= 800000 => s"Recent Bitcoin block"
    case _ => s"Bitcoin mainnet block"
  }

  private def generateMerkleProofTestCase(blockData: BlockData): MerkleProofTestCase = {
    import binocular.reverse
    // Convert transaction hashes to ByteString (reversed for merkle tree)
    val txHashes = blockData.transactions.map { txHex =>
      ByteString.fromHex(txHex).reverse
    }
    
    // Build Merkle tree
    val tree = MerkleTree.fromHashes(txHashes)
    
    // Generate test cases for interesting transactions
    val testCases = scala.collection.mutable.ArrayBuffer[TxProofCase]()
    
    // Always test coinbase (first transaction)
    if (txHashes.nonEmpty) {
      val proof = tree.makeMerkleProof(0)
      testCases += TxProofCase(
        txIndex = 0,
        txHash = blockData.transactions(0),
        merkleProof = proof.map(_.toHex).toSeq,
        description = "Coinbase transaction (first)"
      )
    }
    
    // Test last transaction if more than 1
    if (txHashes.length > 1) {
      val lastIdx = txHashes.length - 1
      val proof = tree.makeMerkleProof(lastIdx)
      testCases += TxProofCase(
        txIndex = lastIdx,
        txHash = blockData.transactions(lastIdx),
        merkleProof = proof.map(_.toHex).toSeq,
        description = "Last transaction"
      )
    }
    
    // Test middle transaction if more than 2
    if (txHashes.length > 2) {
      val midIdx = txHashes.length / 2
      val proof = tree.makeMerkleProof(midIdx)
      testCases += TxProofCase(
        txIndex = midIdx,
        txHash = blockData.transactions(midIdx),
        merkleProof = proof.map(_.toHex).toSeq,
        description = s"Middle transaction (${midIdx + 1} of ${txHashes.length})"
      )
    }
    
    MerkleProofTestCase(
      blockHeight = blockData.height,
      blockHash = blockData.hash,
      merkleRoot = blockData.merkleroot,
      totalTransactions = blockData.transactions.length,
      testCases = testCases.toSeq
    )
  }

  private def saveBlockData(
      blockData: BlockData,
      merkleTestCase: MerkleProofTestCase,
      outputDir: String
  )(using ExecutionContext): Future[FetchResult] = Future {
    // Create output directory
    val dir = Paths.get(outputDir)
    Files.createDirectories(dir)
    
    // Save block data JSON
    val blockFile = dir.resolve(s"block_${blockData.height}.json")
    val blockJson = write(blockData, indent = 2)
    Files.writeString(blockFile, blockJson)
    
    // Save merkle proof test case JSON
    val proofFile = dir.resolve(s"block_${blockData.height}_merkle_proofs.json")
    val proofJson = write(merkleTestCase, indent = 2)
    Files.writeString(proofFile, proofJson)
    
    FetchResult(
      data = blockData,
      blockFile = blockFile.getFileName.toString,
      proofFile = proofFile.getFileName.toString
    )
  }

}
