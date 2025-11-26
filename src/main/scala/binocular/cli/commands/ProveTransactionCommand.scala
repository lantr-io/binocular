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

/** Prove Bitcoin transaction inclusion via oracle
  *
  * Supports two modes:
  *   1. Online mode (default): Fetches transaction and block data from Bitcoin RPC
  *   2. Offline mode: When --block, --tx-index, and --proof are all provided, verifies locally
  *      without any Bitcoin RPC calls
  */
case class ProveTransactionCommand(
    utxo: String,
    btcTxId: String,
    blockHash: Option[String] = None,
    txIndex: Option[Int] = None,
    proof: Option[String] = None,
    merkleRoot: Option[String] = None
) extends Command {

    // Check if we have all required params for offline verification
    private def isOfflineMode: Boolean =
        blockHash.isDefined && txIndex.isDefined && proof.isDefined && merkleRoot.isDefined

    override def execute(): Int = {
        println(s"Proving Bitcoin transaction inclusion...")
        println(s"  Oracle UTxO: $utxo")
        println(s"  Bitcoin TX: $btcTxId")
        blockHash.foreach(h => println(s"  Block Hash: $h"))
        txIndex.foreach(i => println(s"  TX Index: $i"))
        proof.foreach(_ => println(s"  Proof: (provided)"))
        merkleRoot.foreach(r => println(s"  Merkle Root: $r"))
        if isOfflineMode then println(s"  Mode: OFFLINE (no Bitcoin RPC)")
        println()

        // Validate block hash if provided
        blockHash match {
            case Some(hash) if hash.length != 64 =>
                System.err.println(
                  s"Error: --block must be a 64-character block hash, not a block height"
                )
                System.err.println(s"  Received: $hash (${hash.length} characters)")
                System.err.println()
                System.err.println(
                  s"  Use a block hash like: 0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf"
                )
                return 1
            case Some(hash) if !hash.forall(c => c.isDigit || ('a' to 'f').contains(c.toLower)) =>
                System.err.println(s"Error: --block must be a valid hexadecimal block hash")
                System.err.println(s"  Received: $hash")
                return 1
            case _ => // valid or not provided
        }

        // Validate that if any offline param is provided, all must be provided
        val offlineParams = List(blockHash, txIndex, proof, merkleRoot)
        val providedCount = offlineParams.count(_.isDefined)
        if providedCount > 0 && providedCount < 4 then {
            System.err.println(
              s"Error: For offline verification, all four options are required:"
            )
            System.err.println(
              s"  --block <hash>       : ${if blockHash.isDefined then "✓" else "missing"}"
            )
            System.err.println(
              s"  --tx-index <n>       : ${if txIndex.isDefined then "✓" else "missing"}"
            )
            System.err.println(
              s"  --proof <hashes>     : ${if proof.isDefined then "✓" else "missing"}"
            )
            System.err.println(
              s"  --merkle-root <hash> : ${if merkleRoot.isDefined then "✓" else "missing"}"
            )
            return 1
        }

        // Parse UTxO string
        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((oracleTxHash, oracleOutputIndex)) =>
                if isOfflineMode then proveTransactionOffline(oracleTxHash, oracleOutputIndex)
                else proveTransactionOnline(oracleTxHash, oracleOutputIndex)
        }
    }

    /** Offline verification - no Bitcoin RPC calls */
    private def proveTransactionOffline(oracleTxHash: String, oracleOutputIndex: Int): Int = {
        val targetBlockHash = blockHash.get
        val targetTxIndex = txIndex.get
        val proofHashes = proof.get
        val expectedMerkleRoot = merkleRoot.get

        // Parse proof hashes
        val merkleProof =
            try {
                proofHashes
                    .split(",")
                    .map(_.trim)
                    .filter(_.nonEmpty)
                    .map { hash =>
                        if hash.length != 64 then
                            throw new IllegalArgumentException(
                              s"Invalid proof hash length: $hash (${hash.length} chars, expected 64)"
                            )
                        ByteString.fromHex(hash)
                    }
                    .toList
            } catch {
                case e: Exception =>
                    System.err.println(s"Error parsing proof hashes: ${e.getMessage}")
                    return 1
            }

        // Load Cardano config only
        val cardanoConfig = CardanoConfig.load()
        val oracleConfig = OracleConfig.load()

        (cardanoConfig, oracleConfig) match {
            case (Right(cardanoConf), Right(oracleConf)) =>
                println("Step 1: Loading Cardano configuration...")
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

                        // Verify merkle proof locally
                        println()
                        println("Step 4: Verifying Merkle proof...")

                        val txHash = ByteString.fromHex(btcTxId).reverse
                        val calculatedRoot =
                            MerkleTree.calculateMerkleRootFromProof(
                              targetTxIndex,
                              txHash,
                              merkleProof
                            )

                        val expectedRootBytes = ByteString.fromHex(expectedMerkleRoot)

                        println(s"  Calculated Root: ${calculatedRoot.toHex}")
                        println(s"  Expected Root:   ${expectedRootBytes.toHex}")

                        // Verify the calculated root matches expected
                        if calculatedRoot != expectedRootBytes then {
                            println()
                            println("=" * 70)
                            println("VERIFICATION FAILED")
                            println("=" * 70)
                            println()
                            println(s"Transaction ID: $btcTxId")
                            println(s"Block Hash: $targetBlockHash")
                            println(s"Transaction Index: $targetTxIndex")
                            println()
                            println(s"Calculated Merkle Root: ${calculatedRoot.toHex}")
                            println(s"Expected Merkle Root:   ${expectedRootBytes.toHex}")
                            println()
                            println("The merkle proof does NOT verify the transaction inclusion.")
                            println("Possible causes:")
                            println("  - Wrong transaction ID")
                            println("  - Wrong transaction index")
                            println("  - Invalid proof hashes")
                            println("  - Wrong merkle root")
                            println("=" * 70)
                            return 1
                        }

                        println(s"✓ Merkle proof verified!")

                        // Output result
                        println()
                        println("=" * 70)
                        println("TRANSACTION INCLUSION VERIFIED")
                        println("=" * 70)
                        println()
                        println(s"Transaction ID: $btcTxId")
                        println(s"Block Hash: $targetBlockHash")
                        println(s"Transaction Index: $targetTxIndex")
                        println()
                        println(s"Oracle UTxO: $oracleTxHash:$oracleOutputIndex")
                        println(s"Oracle Height: ${chainState.blockHeight}")
                        println()
                        println(s"Merkle Root: ${calculatedRoot.toHex}")
                        println()
                        println("Merkle Proof:")
                        merkleProof.zipWithIndex.foreach { case (hash, i) =>
                            println(s"  [$i] ${hash.toHex}")
                        }
                        println()
                        println("The transaction is cryptographically proven to be in the block.")
                        println("=" * 70)

                        0
                }

            case (Left(err), _) =>
                System.err.println(s"Error loading Cardano config: $err")
                1
            case (_, Left(err)) =>
                System.err.println(s"Error loading Oracle config: $err")
                1
        }
    }

    /** Online verification - uses Bitcoin RPC */
    private def proveTransactionOnline(oracleTxHash: String, oracleOutputIndex: Int): Int = {
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

                        given ec: ExecutionContext = ExecutionContext.global
                        val rpc = new SimpleBitcoinRpc(btcConf)

                        // Get block hash - either from parameter or by fetching transaction
                        println()
                        val (targetBlockHash, confirmations): (String, Option[Int]) =
                            blockHash match {
                                case Some(hash) =>
                                    println(s"Step 4: Using provided block hash...")
                                    println(s"✓ Block hash: $hash")
                                    (hash, None)

                                case None =>
                                    println(s"Step 4: Fetching Bitcoin transaction $btcTxId...")
                                    val txInfo =
                                        try {
                                            Await.result(rpc.getRawTransaction(btcTxId), 30.seconds)
                                        } catch {
                                            case e: Exception =>
                                                val msg = e.getMessage
                                                if msg.contains(
                                                      "No such mempool or blockchain transaction"
                                                    )
                                                then {
                                                    System.err.println(
                                                      s"✗ Transaction not found on Bitcoin blockchain"
                                                    )
                                                    System.err.println(s"  TX ID: $btcTxId")
                                                    System.err.println()
                                                    System.err.println(
                                                      s"  For offline verification, provide:"
                                                    )
                                                    System.err.println(
                                                      s"    --block <hash> --tx-index <n> --proof <hash1,hash2,...>"
                                                    )
                                                } else {
                                                    System.err.println(
                                                      s"✗ Error fetching transaction: $msg"
                                                    )
                                                }
                                                return 1
                                        }

                                    txInfo.blockhash match {
                                        case Some(hash) =>
                                            println(s"✓ Transaction found in block: $hash")
                                            println(s"  Confirmations: ${txInfo.confirmations}")
                                            (hash, Some(txInfo.confirmations))
                                        case None =>
                                            System.err.println(
                                              s"✗ Transaction not confirmed (no blockhash)"
                                            )
                                            return 1
                                    }
                            }

                        // Verify block is in oracle's confirmed state
                        println()
                        println("Step 5: Verifying block is in oracle's confirmed state...")

                        val blockHashBytes = ByteString.fromHex(targetBlockHash).reverse

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
                                Await.result(rpc.getBlockHeader(targetBlockHash), 30.seconds)
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
                                Await.result(rpc.getBlock(targetBlockHash), 60.seconds)
                            } catch {
                                case e: Exception =>
                                    System.err.println(s"✗ Error fetching block: ${e.getMessage}")
                                    return 1
                            }

                        println(s"✓ Block has ${blockInfo.tx.length} transactions")

                        // Find transaction index in block
                        val txIdx = blockInfo.tx.indexWhere(_.txid == btcTxId)
                        if txIdx < 0 then {
                            println()
                            println("=" * 70)
                            println("TRANSACTION NOT FOUND IN BLOCK")
                            println("=" * 70)
                            println()
                            println(s"Transaction ID: $btcTxId")
                            println(s"Block Hash: $targetBlockHash")
                            println(s"Block Height: ${blockHeader.height}")
                            println(s"Block TX Count: ${blockInfo.tx.length}")
                            println()
                            println("The transaction is NOT included in this block.")
                            if blockHash.isDefined then {
                                println(
                                  "This confirms the transaction does not exist in the specified block."
                                )
                            }
                            println("=" * 70)
                            return 1
                        }

                        println(s"✓ Transaction found at index $txIdx")

                        // Build Merkle proof
                        println()
                        println("Step 7: Building Merkle proof...")

                        val txHashes = blockInfo.tx.map { tx =>
                            ByteString.fromHex(tx.txid).reverse
                        }

                        val merkleTree = MerkleTree.fromHashes(txHashes)
                        val merkleRoot = merkleTree.getMerkleRoot
                        val merkleProof = merkleTree.makeMerkleProof(txIdx)

                        println(s"✓ Merkle proof generated")
                        println(s"  Merkle Root: ${merkleRoot.toHex}")
                        println(s"  Proof Size: ${merkleProof.length} hashes")

                        // Verify the proof locally
                        val txHash = ByteString.fromHex(btcTxId).reverse
                        val calculatedRoot =
                            MerkleTree.calculateMerkleRootFromProof(txIdx, txHash, merkleProof)

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
                        println(s"Block Hash: $targetBlockHash")
                        println(s"Block Height: ${blockHeader.height}")
                        println(s"Transaction Index: $txIdx")
                        confirmations.foreach(c => println(s"Confirmations: $c"))
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
                        println("Merkle Proof (use with --proof for offline verification):")
                        merkleProof.zipWithIndex.foreach { case (hash, i) =>
                            println(s"  [$i] ${hash.toHex}")
                        }
                        println()
                        println("Offline verification command:")
                        println(
                          s"  binocular prove-transaction $oracleTxHash:$oracleOutputIndex $btcTxId \\"
                        )
                        println(s"    --block $targetBlockHash \\")
                        println(s"    --tx-index $txIdx \\")
                        println(s"    --merkle-root ${merkleRoot.toHex} \\")
                        println(s"    --proof ${merkleProof.map(_.toHex).mkString(",")}")
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
