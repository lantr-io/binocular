package binocular.cli.commands

import binocular.{reverse, BitcoinNodeConfig, BitcoinValidator, CardanoConfig, ChainState, MerkleTree, OracleConfig, OracleTransactions, SimpleBitcoinRpc}
import binocular.cli.{Command, CommandHelpers}
import com.bloxbean.cardano.client.address.Address
import scalus.bloxbean.Interop.toScalusData
import scalus.builtin.Data.fromData
import scalus.builtin.ByteString

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Prove Bitcoin transaction inclusion via oracle */
case class ProveTransactionCommand(
    utxo: String,
    btcTxId: String
) extends Command {

    override def execute(): Int = {
        println(s"Proving Bitcoin transaction inclusion...")
        println(s"  Oracle UTxO: $utxo")
        println(s"  Bitcoin TX: $btcTxId")
        println()

        // Parse UTxO string
        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((oracleTxHash, oracleOutputIndex)) =>
                proveTransaction(oracleTxHash, oracleOutputIndex)
        }
    }

    private def proveTransaction(oracleTxHash: String, oracleOutputIndex: Int): Int = {
        // Load configurations
        val bitcoinConfig = BitcoinNodeConfig.load()
        val cardanoConfig = CardanoConfig.load()
        val oracleConfig = OracleConfig.load()

        (bitcoinConfig, cardanoConfig, oracleConfig) match {
            case (Right(btcConf), Right(cardanoConf), Right(oracleConf)) =>
                println("Step 1: Loading configurations...")
                println(s"✓ Bitcoin Node: ${btcConf.url}")
                println(s"✓ Cardano Network: ${cardanoConf.network}")
                println(s"✓ Oracle Address: ${oracleConf.scriptAddress}")
                println()

                // Create Cardano backend service
                val backendService = cardanoConf.createBackendService() match {
                    case Right(service) =>
                        println(s"✓ Connected to Cardano backend")
                        service
                    case Left(err) =>
                        System.err.println(s"Error creating backend service: $err")
                        return 1
                }

                println()
                println("Step 2: Fetching oracle UTxO from Cardano...")

                // Fetch the oracle UTxO
                val scriptAddress = new Address(oracleConf.scriptAddress)
                val utxoService = backendService.getUtxoService
                val utxos =
                    try {
                        utxoService.getUtxos(scriptAddress.getAddress, 100, 1)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"✗ Error fetching UTxOs: ${e.getMessage}")
                            return 1
                    }

                if !utxos.isSuccessful then {
                    System.err.println(s"✗ Error fetching UTxOs: ${utxos.getResponse}")
                    return 1
                }

                val allUtxos = utxos.getValue.asScala.toList
                val targetUtxo = allUtxos.find(u =>
                    u.getTxHash == oracleTxHash && u.getOutputIndex == oracleOutputIndex
                )

                targetUtxo match {
                    case None =>
                        System.err.println(
                          s"✗ Oracle UTxO not found: $oracleTxHash:$oracleOutputIndex"
                        )
                        return 1

                    case Some(utxo) =>
                        println(s"✓ Found oracle UTxO")

                        // Parse ChainState datum
                        println()
                        println("Step 3: Parsing ChainState datum...")

                        val inlineDatumHex = utxo.getInlineDatum
                        if inlineDatumHex == null || inlineDatumHex.isEmpty then {
                            System.err.println("✗ UTxO has no inline datum")
                            return 1
                        }

                        val chainState =
                            try {
                                val plutusData =
                                    com.bloxbean.cardano.client.plutus.spec.PlutusData.deserialize(
                                      com.bloxbean.cardano.client.util.HexUtil
                                          .decodeHexString(inlineDatumHex)
                                    )
                                val data = plutusData.toScalusData
                                data.to[ChainState]
                            } catch {
                                case e: Exception =>
                                    System.err.println(
                                      s"✗ Error parsing ChainState datum: ${e.getMessage}"
                                    )
                                    return 1
                            }

                        println(s"✓ Oracle state:")
                        println(s"  Block Height: ${chainState.blockHeight}")
                        println(s"  Block Hash: ${chainState.blockHash.toHex}")

                        // Fetch Bitcoin transaction
                        println()
                        println(s"Step 4: Fetching Bitcoin transaction $btcTxId...")

                        given ec: ExecutionContext = ExecutionContext.global
                        val rpc = new SimpleBitcoinRpc(btcConf)

                        val txInfo =
                            try {
                                Await.result(rpc.getRawTransaction(btcTxId), 30.seconds)
                            } catch {
                                case e: Exception =>
                                    System.err.println(
                                      s"✗ Error fetching transaction: ${e.getMessage}"
                                    )
                                    return 1
                            }

                        val blockHash = txInfo.blockhash match {
                            case Some(hash) => hash
                            case None =>
                                System.err.println(s"✗ Transaction not confirmed (no blockhash)")
                                return 1
                        }

                        println(s"✓ Transaction found in block: $blockHash")
                        println(s"  Confirmations: ${txInfo.confirmations}")

                        // Verify block is in oracle's confirmed state
                        println()
                        println("Step 5: Verifying block is in oracle's confirmed state...")

                        val blockHashBytes = ByteString.fromHex(blockHash).reverse

                        // Check if block is in confirmed state (not in forks tree)
                        // With ForkBranch, need to check if block is in any branch
                        val blockInForksTree = chainState.forksTree.exists { branch =>
                            branch.tipHash == blockHashBytes ||
                            BitcoinValidator.existsHash(branch.recentBlocks, blockHashBytes)
                        }

                        if blockInForksTree then {
                            System.err.println(s"✗ Block is still in fork tree (not yet confirmed)")
                            println(s"  The oracle needs to be updated to confirm this block")
                            return 1
                        }

                        // For a simple check, verify the block height is <= oracle height
                        val blockHeader =
                            try {
                                Await.result(rpc.getBlockHeader(blockHash), 30.seconds)
                            } catch {
                                case e: Exception =>
                                    System.err.println(
                                      s"✗ Error fetching block header: ${e.getMessage}"
                                    )
                                    return 1
                            }

                        if blockHeader.height > chainState.blockHeight.toInt then {
                            System.err.println(
                              s"✗ Block height ${blockHeader.height} > oracle height ${chainState.blockHeight}"
                            )
                            println(s"  Oracle needs to be updated to include this block")
                            return 1
                        }

                        println(s"✓ Block is confirmed by oracle")
                        println(s"  Block Height: ${blockHeader.height}")

                        // Fetch full block with transactions
                        println()
                        println("Step 6: Fetching block transactions to build Merkle proof...")

                        val blockInfo =
                            try {
                                Await.result(rpc.getBlock(blockHash), 60.seconds)
                            } catch {
                                case e: Exception =>
                                    System.err.println(s"✗ Error fetching block: ${e.getMessage}")
                                    return 1
                            }

                        println(s"✓ Block has ${blockInfo.tx.length} transactions")

                        // Find transaction index in block
                        val txIndex = blockInfo.tx.indexWhere(_.txid == btcTxId)
                        if txIndex < 0 then {
                            System.err.println(s"✗ Transaction not found in block")
                            return 1
                        }

                        println(s"✓ Transaction found at index $txIndex")

                        // Build Merkle proof
                        println()
                        println("Step 7: Building Merkle proof...")

                        val txHashes = blockInfo.tx.map { tx =>
                            ByteString.fromHex(tx.txid).reverse
                        }

                        val merkleTree = MerkleTree.fromHashes(txHashes)
                        val merkleRoot = merkleTree.getMerkleRoot
                        val merkleProof = merkleTree.makeMerkleProof(txIndex)

                        println(s"✓ Merkle proof generated")
                        println(s"  Merkle Root: ${merkleRoot.toHex}")
                        println(s"  Proof Size: ${merkleProof.length} hashes")

                        // Verify the proof locally
                        val txHash = ByteString.fromHex(btcTxId).reverse
                        val calculatedRoot =
                            MerkleTree.calculateMerkleRootFromProof(txIndex, txHash, merkleProof)

                        if calculatedRoot != merkleRoot then {
                            System.err.println(s"✗ Merkle proof verification failed!")
                            return 1
                        }

                        println(s"✓ Merkle proof verified locally")

                        // Output proof
                        println()
                        println("=" * 70)
                        println("TRANSACTION INCLUSION PROOF")
                        println("=" * 70)
                        println()
                        println(s"Transaction ID: $btcTxId")
                        println(s"Block Hash: $blockHash")
                        println(s"Block Height: ${blockHeader.height}")
                        println(s"Transaction Index: $txIndex")
                        println(s"Confirmations: ${txInfo.confirmations}")
                        println()
                        println(s"Oracle UTxO: $oracleTxHash:$oracleOutputIndex")
                        println(s"Oracle Height: ${chainState.blockHeight}")
                        println(s"Oracle Block: ${chainState.blockHash.toHex}")
                        println()
                        println(s"Merkle Root: ${merkleRoot.toHex}")
                        println(
                          s"Expected Root: ${ByteString.fromHex(blockHeader.merkleroot).reverse.toHex}"
                        )
                        println()
                        println("Merkle Proof:")
                        merkleProof.zipWithIndex.foreach { case (hash, i) =>
                            println(s"  [$i] ${hash.toHex}")
                        }
                        println()
                        println("=" * 70)

                        0
                }

            case (Left(err), _, _) =>
                System.err.println(s"Error loading Bitcoin config: $err")
                1
            case (_, Left(err), _) =>
                System.err.println(s"Error loading Cardano config: $err")
                1
            case (_, _, Left(err)) =>
                System.err.println(s"Error loading Oracle config: $err")
                1
        }
    }
}
