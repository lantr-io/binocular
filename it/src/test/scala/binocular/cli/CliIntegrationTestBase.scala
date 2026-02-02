package binocular.cli

import binocular.{BitcoinNodeConfig, BlockHeaderInfo, BlockInfo, BlockchainInfo, CardanoConfig, OracleConfig, RawTransactionInfo, SimpleBitcoinRpc, TransactionInfo, WalletConfig, YaciDevKitSpec}
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
trait CliIntegrationTestBase extends YaciDevKitSpec {

    /** Mock Bitcoin RPC that reads from test fixtures */
    class MockBitcoinRpc(fixtureDir: String = "src/test/resources/bitcoin_blocks")(using
        ec: ExecutionContext
    ) {

        private def loadBlockFixture(height: Int): BlockFixture = {
            val file = s"$fixtureDir/block_$height.json"
            val json = Source.fromFile(file).mkString
            read[BlockFixture](json)
        }

        private def loadBlockByHash(hash: String): Option[BlockFixture] = {
            // Linear search through fixtures - good enough for tests
            val dir = new java.io.File(fixtureDir)
            dir.listFiles()
                .filter(_.getName.startsWith("block_"))
                .filter(_.getName.endsWith(".json"))
                .filterNot(_.getName.contains("merkle_proofs")) // Skip merkle proof files
                .map { f =>
                    val json = Source.fromFile(f).mkString
                    read[BlockFixture](json)
                }
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
                    .map { f =>
                        val json = Source.fromFile(f).mkString
                        read[BlockFixture](json)
                    }
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
                    .map { f =>
                        val json = Source.fromFile(f).mkString
                        read[BlockFixture](json)
                    }
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

    /** Block fixture from JSON */
    case class BlockFixture(
        height: Int,
        hash: String,
        merkleroot: String,
        transactions: Seq[String],
        previousblockhash: Option[String], // Required for all blocks except genesis
        timestamp: Long, // Required for validation
        bits: String, // Required for difficulty validation
        nonce: Long, // Required for proof-of-work validation
        version: Long, // Required for header hash calculation
        difficulty: Option[Double] = None, // Optional - not used in validation
        description: Option[String] = None // Optional - for documentation
    ) derives ReadWriter

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
        devKit: YaciDevKit,
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
