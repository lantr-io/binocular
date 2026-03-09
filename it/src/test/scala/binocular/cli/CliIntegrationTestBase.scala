package binocular.cli

import binocular.{BitcoinNodeConfig, BlockFixture, BlockHeaderInfo, BlockInfo, BlockchainInfo, CardanoConfig, OracleConfig, RawTransactionInfo, TransactionInfo, WalletConfig}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.BlockchainProvider
import scalus.testing.integration.YaciTestContext
import scalus.testing.yaci.{YaciConfig, YaciDevKit}
import scalus.utils.await

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import upickle.default.*

/** Base trait for CLI integration tests
  *
  * Provides infrastructure for testing CLI commands with:
  *   - Mock Bitcoin RPC using saved fixture data
  *   - Yaci DevKit for Cardano interactions
  *   - Helper methods for common test scenarios
  */
trait CliIntegrationTestBase extends AnyFunSuite with YaciDevKit {

    override protected def yaciConfig: YaciConfig = YaciConfig(
      containerName = "binocular-yaci-devkit",
      reuseContainer = true
    )

    /** Get UTXOs for an address */
    protected def getUtxos(provider: BlockchainProvider, address: Address): List[Utxo] = {
        given ExecutionContext = provider.executionContext
        val result = provider.findUtxos(address).await(30.seconds)
        result match {
            case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
            case Left(_)  => List.empty
        }
    }

    /** Find the oracle UTxO created by a specific transaction */
    protected def findOracleUtxo(
        provider: BlockchainProvider,
        scriptAddress: Address,
        txHash: String
    ): Utxo = {
        val utxos = getUtxos(provider, scriptAddress)
        utxos
            .find(u =>
                u.input.transactionId.toHex == txHash && u.output.inlineDatum.isDefined
            )
            .getOrElse {
                throw new RuntimeException(
                  s"No oracle UTxO with inline datum found for tx $txHash"
                )
            }
    }

    /** Wait for transaction to be confirmed */
    protected def waitForTransaction(
        provider: BlockchainProvider,
        txHash: String,
        maxAttempts: Int = 30,
        delayMs: Long = 1000
    ): Boolean = {
        given ExecutionContext = provider.executionContext
        val input = TransactionInput(TransactionHash.fromHex(txHash), 0)
        var attempts = 0
        while attempts < maxAttempts do {
            try {
                val result = provider.findUtxo(input).await(10.seconds)
                result match {
                    case Right(_) => return true
                    case Left(_) =>
                        Thread.sleep(delayMs)
                        attempts += 1
                }
            } catch {
                case _: Exception =>
                    Thread.sleep(delayMs)
                    attempts += 1
            }
        }
        false
    }

    /** Mock Bitcoin RPC that reads from test fixtures */
    class MockBitcoinRpc(fixtureDir: String = "src/test/resources/bitcoin_blocks")(using
        ec: ExecutionContext
    ) {

        private def loadBlockFixture(height: Int): BlockFixture =
            BlockFixture.load(height, fixtureDir)

        private def loadBlockByHash(hash: String): Option[BlockFixture] = {
            // Linear search through fixtures - good enough for tests
            val dir = new java.io.File(fixtureDir)
            dir.listFiles()
                .filter(_.getName.startsWith("block_"))
                .filter(_.getName.endsWith(".json"))
                .filterNot(_.getName.contains("merkle_proofs")) // Skip merkle proof files
                .map { f => BlockFixture.load(f) }
                .find(_.hash == hash)
        }

        def getBlockHash(height: Int): Future[String] = {
            Future {
                val fixture = loadBlockFixture(height)
                fixture.hash
            }
        }

        def getBlockHeader(hash: String): Future[BlockHeaderInfo] = {
            Future {
                val fixture = loadBlockByHash(hash).getOrElse(
                  throw new RuntimeException(s"Block not found: $hash")
                )

                // Validate required fields are present
                if fixture.bits.isEmpty then {
                    throw new RuntimeException(
                      s"Missing required field 'bits' for block ${fixture.height} (${fixture.hash})"
                    )
                }

                BlockHeaderInfo(
                  hash = fixture.hash,
                  height = fixture.height,
                  version = fixture.version.toInt,
                  merkleroot = fixture.merkleroot,
                  time = fixture.timestamp,
                  nonce = fixture.nonce,
                  bits = fixture.bits,
                  difficulty = 0.0, // Not used in our tests
                  previousblockhash = fixture.previousblockhash
                )
            }
        }

        def getBlock(hash: String): Future[BlockInfo] = {
            Future {
                val fixture = loadBlockByHash(hash).getOrElse(
                  throw new RuntimeException(s"Block not found: $hash")
                )

                // Validate required fields are present
                if fixture.bits.isEmpty then {
                    throw new RuntimeException(
                      s"Missing required field 'bits' for block ${fixture.height} (${fixture.hash})"
                    )
                }

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
        }

        def getRawTransaction(txid: String): Future[RawTransactionInfo] = {
            Future {
                // Find which block contains this transaction
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
                          confirmations = 10 // Assume confirmed
                        )
                    case None =>
                        throw new RuntimeException(s"Transaction not found: $txid")
                }
            }
        }

        def getBlockchainInfo(): Future[BlockchainInfo] = {
            Future {
                // Find highest block in fixtures
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
    }

    /** Merkle proof test case from JSON */
    case class MerkleProofTestCase(
        txIndex: Int,
        txHash: String,
        merkleProof: Seq[String],
        description: String
    ) derives ReadWriter

    /** Merkle proof fixture from JSON */
    case class MerkleProofFixture(
        blockHeight: Int,
        blockHash: String,
        merkleRoot: String,
        totalTransactions: Int,
        testCases: Seq[MerkleProofTestCase]
    ) derives ReadWriter

    /** Load merkle proof fixture for a block */
    def loadMerkleProofFixture(
        height: Int,
        fixtureDir: String = "src/test/resources/bitcoin_blocks"
    ): MerkleProofFixture = {
        val file = s"$fixtureDir/block_${height}_merkle_proofs.json"
        val json = Source.fromFile(file).mkString
        read[MerkleProofFixture](json)
    }

    /** Helper to create test configurations for CLI commands */
    def createTestConfigs(
        ctx: YaciTestContext,
        mockRpc: MockBitcoinRpc,
        startHeight: Option[Long] = None
    ): (BitcoinNodeConfig, CardanoConfig, OracleConfig, WalletConfig) = {

        val bitcoinConfig = BitcoinNodeConfig(
          url = "mock://bitcoin-rpc",
          username = "test",
          password = "test",
          network = binocular.BitcoinNetwork.Testnet
        )

        val cardanoConfig = CardanoConfig(
          backend = binocular.CardanoBackend.Blockfrost,
          network = binocular.CardanoNetwork.Testnet,
          blockfrost = binocular.BlockfrostConfig(projectId = "")
        )

        val oracleConfig = binocular.OracleConfig(
          network = binocular.CardanoNetwork.Testnet,
          startHeight = startHeight,
          maxHeadersPerTx = 10,
          pollInterval = 60
        )

        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val walletConfig = binocular.WalletConfig(
          mnemonic = mnemonic,
          network = binocular.CardanoNetwork.Testnet
        )

        (bitcoinConfig, cardanoConfig, oracleConfig, walletConfig)
    }
}
