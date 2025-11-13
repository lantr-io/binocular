package binocular

import binocular.BitcoinValidator.{BlockHeader, ChainState}
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.core.protocol.blockchain.{BlockHeader => BtcHeader}
import org.bitcoins.rpc.client.v27.BitcoindV27RpcClient
import scalus.builtin.ByteString
import scalus.prelude
import upickle.default.*

import java.io.File
import java.net.URI
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Using}

/** Test fixtures for Bitcoin block headers
  *
  * Provides utilities for:
  * - Fetching real Bitcoin headers from RPC
  * - Saving headers as JSON test fixtures
  * - Loading headers from fixtures for tests
  * - Creating ChainState from header sequences
  */
object BitcoinHeaderFixtures {

    /** Serializable representation of a Bitcoin block header */
    case class HeaderFixture(
        height: Int,
        hash: String,            // Hex-encoded block hash
        prevHash: String,        // Hex-encoded previous block hash
        merkleRoot: String,      // Hex-encoded Merkle root
        timestamp: Long,         // Unix timestamp
        bits: String,            // Hex-encoded difficulty target (compact bits)
        nonce: Long,             // Block nonce
        version: Int,            // Block version
        headerBytes: String      // Hex-encoded 80-byte raw header
    ) derives ReadWriter

    /** A fixture containing multiple consecutive headers */
    case class HeaderSequenceFixture(
        name: String,
        startHeight: Int,
        endHeight: Int,
        headers: List[HeaderFixture]
    ) derives ReadWriter

    /** Convert bitcoin-s BlockHeader to our fixture format */
    private def toFixture(btcHeader: BtcHeader, height: Int): HeaderFixture = {
        HeaderFixture(
          height = height,
          hash = btcHeader.hashBE.hex,
          prevHash = btcHeader.previousBlockHashBE.hex,
          merkleRoot = btcHeader.merkleRootHashBE.hex,
          timestamp = btcHeader.time.toLong,
          bits = btcHeader.nBits.hex,
          nonce = btcHeader.nonce.toLong,
          version = btcHeader.version.toInt,
          headerBytes = btcHeader.hex
        )
    }

    /** Convert HeaderFixture to Scalus BlockHeader */
    def toBlockHeader(fixture: HeaderFixture): BlockHeader = {
        BlockHeader(ByteString.fromHex(fixture.headerBytes))
    }

    /** Generate fixture by fetching headers from Bitcoin RPC
      *
      * Requires Bitcoin Core RPC credentials in environment:
      * - bitcoind_rpc_url
      * - bitcoind_rpc_user
      * - bitcoind_rpc_password
      *
      * @param name fixture name (used for filename)
      * @param startHeight starting block height
      * @param count number of blocks to fetch
      * @param outputDir directory to save fixture (default: src/test/resources/fixtures)
      * @return Future that completes when fixture is saved
      */
    def generateFixture(
        name: String,
        startHeight: Int,
        count: Int,
        outputDir: String = "src/test/resources/fixtures"
    )(using system: ActorSystem, client: BitcoindV27RpcClient): Future[File] = {
        given ec: ExecutionContext = system.dispatcher

        def fetchHeaders(height: Int, remaining: Int, acc: List[HeaderFixture]): Future[List[HeaderFixture]] = {
            if (remaining <= 0) Future.successful(acc.reverse)
            else
                for
                    blockHash <- client.getBlockHash(height)
                    header <- client.getBlockHeader(blockHash)
                    fixture = toFixture(header.blockHeader, height)
                    _ = print(s"\rFetching header at height $height... (${count - remaining + 1}/$count)")
                    rest <- fetchHeaders(height + 1, remaining - 1, fixture :: acc)
                yield rest
        }

        for
            headers <- fetchHeaders(startHeight, count, Nil)
            _ = println(s"\n✓ Fetched $count headers from height $startHeight")
            fixture = HeaderSequenceFixture(
              name = name,
              startHeight = startHeight,
              endHeight = startHeight + count - 1,
              headers = headers
            )
            outputFile = new File(s"$outputDir/$name.json")
            _ = outputFile.getParentFile.mkdirs()
            _ = os.write.over(os.Path(outputFile.getAbsolutePath), write(fixture, indent = 2))
            _ = println(s"✓ Saved fixture to ${outputFile.getAbsolutePath}")
        yield outputFile
    }

    /** Load fixture from JSON file
      *
      * @param name fixture name (without .json extension)
      * @param fixturesDir directory containing fixtures
      * @return HeaderSequenceFixture loaded from file
      */
    def loadFixture(
        name: String,
        fixturesDir: String = "src/test/resources/fixtures"
    ): HeaderSequenceFixture = {
        val file = new File(s"$fixturesDir/$name.json")
        require(file.exists(), s"Fixture file not found: ${file.getAbsolutePath}")

        val json = os.read(os.Path(file.getAbsolutePath))
        read[HeaderSequenceFixture](json)
    }

    /** Load headers as Scalus BlockHeader list */
    def loadHeaders(name: String): List[BlockHeader] = {
        val fixture = loadFixture(name)
        fixture.headers.map(toBlockHeader)
    }

    /** Create initial ChainState from first header in sequence
      *
      * Creates a genesis state suitable for starting Oracle tests.
      *
      * @param fixture the header sequence fixture
      * @return ChainState initialized from first header
      */
    def createGenesisState(fixture: HeaderSequenceFixture): ChainState = {
        val firstHeader = fixture.headers.head
        val blockHash = ByteString.fromHex(firstHeader.hash).reverse
        val target = ByteString.fromHex(firstHeader.bits).reverse
        val timestamp = BigInt(firstHeader.timestamp)

        // Calculate previous difficulty adjustment timestamp
        val interval = BitcoinValidator.DifficultyAdjustmentInterval.toInt
        val previousAdjustmentHeight = firstHeader.height - (firstHeader.height % interval)
        val blocksSinceAdjustment = firstHeader.height - previousAdjustmentHeight
        val estimatedPrevAdjustmentTime = timestamp - (blocksSinceAdjustment * BitcoinValidator.TargetBlockTime)

        ChainState(
          blockHeight = BigInt(firstHeader.height),
          blockHash = blockHash,
          currentTarget = target,
          blockTimestamp = timestamp,
          recentTimestamps = prelude.List.single(timestamp),
          previousDifficultyAdjustmentTimestamp = estimatedPrevAdjustmentTime,
          confirmedBlocksTree = scalus.prelude.List(blockHash),
          forksTree = prelude.SortedMap.empty
        )
    }

    /** Pre-defined fixture names for common test scenarios */
    object FixtureNames {
        val Small20 = "bitcoin-headers-small-20"              // 20 blocks for quick tests
        val Medium100 = "bitcoin-headers-medium-100"          // 100 blocks for normal tests
        val DifficultyAdjustment = "bitcoin-headers-diff-adj" // Blocks around difficulty adjustment
    }

    /** Fixture generation utility
      *
      * Run with Bitcoin RPC credentials:
      * {{{
      * export bitcoind_rpc_url="http://localhost:8332"
      * export bitcoind_rpc_user="bitcoin"
      * export bitcoind_rpc_password="password"
      * sbt "test:runMain binocular.BitcoinHeaderFixtures"
      * }}}
      */
    def main(args: Array[String]): Unit = {
        import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceRemote, BitcoindRpcAppConfig}
        import org.bitcoins.core.config.MainNet
        import java.nio.file.Path

        val bitcoindUri = new URI(
          sys.env.getOrElse("bitcoind_rpc_url", sys.error("bitcoind_rpc_url not set"))
        )
        val bitcoindUser = sys.env.getOrElse("bitcoind_rpc_user", sys.error("bitcoind_rpc_user not set"))
        val bitcoindPassword = sys.env.getOrElse("bitcoind_rpc_password", sys.error("bitcoind_rpc_password not set"))

        given system: ActorSystem = ActorSystem("bitcoin-fixtures")
        given ec: ExecutionContext = system.dispatcher

        given BitcoindRpcAppConfig = BitcoindRpcAppConfig(
          baseDatadir = Path.of("."),
          configOverrides = Vector.empty,
          authCredentinalsOpt = Some(BitcoindAuthCredentials.PasswordBased(bitcoindUser, bitcoindPassword))
        )

        val instance = BitcoindInstanceRemote(
          network = MainNet,
          uri = bitcoindUri,
          rpcUri = bitcoindUri
        )

        given client: BitcoindV27RpcClient = new BitcoindV27RpcClient(instance)

        println("=== Bitcoin Header Fixture Generator ===")
        println(s"Connected to: $bitcoindUri")
        println()

        // Generate small fixture for quick tests (20 blocks)
        val program = for
            info <- client.getBlockChainInfo
            currentHeight = info.blocks
            startHeight = currentHeight - 50 // Start 50 blocks back for stable data
            _ = println(s"Current blockchain height: $currentHeight")
            _ = println(s"Generating fixtures starting from height: $startHeight")
            _ = println()

            // Generate small fixture
            _ <- generateFixture(FixtureNames.Small20, startHeight, 20)
            _ = println()

            // Optional: Generate larger fixtures if requested
            _ <- if (args.contains("--all")) {
                for
                    _ <- generateFixture(FixtureNames.Medium100, startHeight, 100)
                    _ = println()
                yield ()
            } else {
                println("Hint: Run with --all to generate all fixtures")
                Future.successful(())
            }
        yield ()

        program.onComplete {
            case Success(_) =>
                println()
                println("✓ Fixture generation complete!")
                system.terminate()
            case Failure(e) =>
                println(s"✗ Fixture generation failed: ${e.getMessage}")
                e.printStackTrace()
                system.terminate()
        }
    }
}
