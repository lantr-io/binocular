package binocular.cli.commands

import binocular.{reverse, BitcoinNodeConfig, BitcoinValidator, CardanoConfig, ChainState, MerkleTree, OracleConfig, OracleTransactions, SimpleBitcoinRpc}
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo, Utxos}
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** Prove Bitcoin transaction inclusion via oracle */
case class ProveTransactionCommand(
    utxo: String,
    btcTxId: String,
    blockHash: Option[String] = None,
    txIndex: Option[Int] = None,
    proof: Option[String] = None,
    merkleRoot: Option[String] = None
) extends Command {

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
                return 1
            case Some(hash) if !hash.forall(c => c.isDigit || ('a' to 'f').contains(c.toLower)) =>
                System.err.println(s"Error: --block must be a valid hexadecimal block hash")
                return 1
            case _ =>
        }

        val offlineParams = List(blockHash, txIndex, proof, merkleRoot)
        val providedCount = offlineParams.count(_.isDefined)
        if providedCount > 0 && providedCount < 4 then {
            System.err.println(s"Error: For offline verification, all four options are required:")
            System.err.println(
              s"  --block <hash>       : ${if blockHash.isDefined then "provided" else "missing"}"
            )
            System.err.println(
              s"  --tx-index <n>       : ${if txIndex.isDefined then "provided" else "missing"}"
            )
            System.err.println(
              s"  --proof <hashes>     : ${if proof.isDefined then "provided" else "missing"}"
            )
            System.err.println(
              s"  --merkle-root <hash> : ${if merkleRoot.isDefined then "provided" else "missing"}"
            )
            return 1
        }

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((oracleTxHash, oracleOutputIndex)) =>
                if isOfflineMode then proveTransactionOffline(oracleTxHash, oracleOutputIndex)
                else proveTransactionOnline(oracleTxHash, oracleOutputIndex)
        }
    }

    private def fetchOracleUtxo(
        oracleTxHash: String,
        oracleOutputIndex: Int
    ): Either[Int, (scalus.cardano.node.BlockchainProvider, Utxo, ChainState, OracleConfig)] = {
        val cardanoConfig = CardanoConfig.load()
        val oracleConfig = OracleConfig.load()

        (cardanoConfig, oracleConfig) match {
            case (Right(cardanoConf), Right(oracleConf)) =>
                given ec: ExecutionContext = ExecutionContext.global

                val provider = cardanoConf.createBlockchainProvider() match {
                    case Right(p) => p
                    case Left(err) =>
                        System.err.println(s"Error creating blockchain provider: $err")
                        return Left(1)
                }

                val input =
                    TransactionInput(TransactionHash.fromHex(oracleTxHash), oracleOutputIndex)
                val utxoResult = Await.result(provider.findUtxo(input), 30.seconds)

                val utxo = utxoResult match {
                    case Right(u) => u
                    case Left(_) =>
                        System.err.println(
                          s"Oracle UTxO not found: $oracleTxHash:$oracleOutputIndex"
                        )
                        return Left(1)
                }

                val chainState =
                    try {
                        val data = utxo.output.requireInlineDatum
                        data.to[ChainState]
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error parsing ChainState datum: ${e.getMessage}")
                            return Left(1)
                    }

                Right((provider, utxo, chainState, oracleConf))

            case (Left(err), _) =>
                System.err.println(s"Error loading Cardano config: $err")
                Left(1)
            case (_, Left(err)) =>
                System.err.println(s"Error loading Oracle config: $err")
                Left(1)
        }
    }

    private def proveTransactionOffline(oracleTxHash: String, oracleOutputIndex: Int): Int = {
        val targetBlockHash = blockHash.get
        val targetTxIndex = txIndex.get
        val proofHashes = proof.get
        val expectedMerkleRoot = merkleRoot.get

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

        println("Step 1: Loading Cardano configuration...")

        fetchOracleUtxo(oracleTxHash, oracleOutputIndex) match {
            case Left(exitCode) => exitCode
            case Right((_, _, chainState, _)) =>
                println(s"Oracle state:")
                println(s"  Block Height: ${chainState.blockHeight}")
                println(s"  Block Hash: ${chainState.blockHash.toHex}")

                println()
                println("Step 4: Verifying Merkle proof...")

                val txHash = ByteString.fromHex(btcTxId).reverse
                val calculatedRoot =
                    MerkleTree.calculateMerkleRootFromProof(targetTxIndex, txHash, merkleProof)
                val expectedRootBytes = ByteString.fromHex(expectedMerkleRoot)

                println(s"  Calculated Root: ${calculatedRoot.toHex}")
                println(s"  Expected Root:   ${expectedRootBytes.toHex}")

                if calculatedRoot != expectedRootBytes then {
                    println()
                    println("=" * 70)
                    println("VERIFICATION FAILED")
                    println("=" * 70)
                    println()
                    println(s"The merkle proof does NOT verify the transaction inclusion.")
                    return 1
                }

                println(s"Merkle proof verified!")
                println()
                println("=" * 70)
                println("TRANSACTION INCLUSION VERIFIED")
                println("=" * 70)
                println()
                println(s"Transaction ID: $btcTxId")
                println(s"Block Hash: $targetBlockHash")
                println(s"Transaction Index: $targetTxIndex")
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
    }

    private def proveTransactionOnline(oracleTxHash: String, oracleOutputIndex: Int): Int = {
        val bitcoinConfig = BitcoinNodeConfig.load()

        bitcoinConfig match {
            case Left(err) =>
                System.err.println(s"Error loading Bitcoin config: $err")
                return 1
            case _ =>
        }
        val btcConf = bitcoinConfig.toOption.get

        println("Step 1: Loading configurations...")
        println(s"  Bitcoin Node: ${btcConf.url}")

        fetchOracleUtxo(oracleTxHash, oracleOutputIndex) match {
            case Left(exitCode) => exitCode
            case Right((_, _, chainState, oracleConf)) =>
                println(s"  Oracle Address: ${oracleConf.scriptAddress}")
                println()
                println(s"Oracle state:")
                println(s"  Block Height: ${chainState.blockHeight}")
                println(s"  Block Hash: ${chainState.blockHash.toHex}")

                given ec: ExecutionContext = ExecutionContext.global
                val rpc = new SimpleBitcoinRpc(btcConf)

                println()
                val (targetBlockHash, confirmations): (String, Option[Int]) =
                    blockHash match {
                        case Some(hash) =>
                            println(s"Step 4: Using provided block hash...")
                            println(s"  Block hash: $hash")
                            (hash, None)

                        case None =>
                            println(s"Step 4: Fetching Bitcoin transaction $btcTxId...")
                            val txInfo =
                                try {
                                    Await.result(rpc.getRawTransaction(btcTxId), 30.seconds)
                                } catch {
                                    case e: Exception =>
                                        val msg = e.getMessage
                                        if msg.contains("No such mempool or blockchain transaction")
                                        then {
                                            System.err.println(
                                              s"Transaction not found on Bitcoin blockchain"
                                            )
                                            System.err.println(s"  TX ID: $btcTxId")
                                        } else {
                                            System.err.println(s"Error fetching transaction: $msg")
                                        }
                                        return 1
                                }

                            txInfo.blockhash match {
                                case Some(hash) =>
                                    println(s"  Transaction found in block: $hash")
                                    println(s"  Confirmations: ${txInfo.confirmations}")
                                    (hash, Some(txInfo.confirmations))
                                case None =>
                                    System.err.println(s"Transaction not confirmed (no blockhash)")
                                    return 1
                            }
                    }

                println()
                println("Step 5: Verifying block is in oracle's confirmed state...")

                val blockHashBytes = ByteString.fromHex(targetBlockHash).reverse

                val blockInForksTree = chainState.forksTree.exists { branch =>
                    branch.tipHash == blockHashBytes ||
                    BitcoinValidator.existsHash(branch.recentBlocks, blockHashBytes)
                }

                if blockInForksTree then {
                    System.err.println(s"Block is still in fork tree (not yet confirmed)")
                    return 1
                }

                val blockHeader =
                    try {
                        Await.result(rpc.getBlockHeader(targetBlockHash), 30.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error fetching block header: ${e.getMessage}")
                            return 1
                    }

                if blockHeader.height > chainState.blockHeight.toInt then {
                    System.err.println(
                      s"Block height ${blockHeader.height} > oracle height ${chainState.blockHeight}"
                    )
                    return 1
                }

                println(s"  Block is confirmed by oracle")
                println(s"  Block Height: ${blockHeader.height}")

                println()
                println("Step 6: Fetching block transactions to build Merkle proof...")

                val blockInfo =
                    try {
                        Await.result(rpc.getBlock(targetBlockHash), 60.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error fetching block: ${e.getMessage}")
                            return 1
                    }

                println(s"  Block has ${blockInfo.tx.length} transactions")

                val txIdx = blockInfo.tx.indexWhere(_.txid == btcTxId)
                if txIdx < 0 then {
                    println()
                    println("TRANSACTION NOT FOUND IN BLOCK")
                    return 1
                }

                println(s"  Transaction found at index $txIdx")

                println()
                println("Step 7: Building Merkle proof...")

                val txHashes = blockInfo.tx.map { tx =>
                    ByteString.fromHex(tx.txid).reverse
                }

                val merkleTree = MerkleTree.fromHashes(txHashes)
                val merkleRootVal = merkleTree.getMerkleRoot
                val merkleProof = merkleTree.makeMerkleProof(txIdx)

                println(s"  Merkle proof generated")
                println(s"  Merkle Root: ${merkleRootVal.toHex}")
                println(s"  Proof Size: ${merkleProof.length} hashes")

                val txHash = ByteString.fromHex(btcTxId).reverse
                val calculatedRoot =
                    MerkleTree.calculateMerkleRootFromProof(txIdx, txHash, merkleProof)

                if calculatedRoot != merkleRootVal then {
                    System.err.println(s"Merkle proof verification failed!")
                    return 1
                }

                println(s"  Merkle proof verified locally")

                // Step 8: Build block proof
                println()
                println("Step 8: Building block proof for Oracle's confirmed tree...")

                val emptyHash = ByteString.unsafeFromArray(new Array[Byte](32))
                def countBlocksInTree(tree: ScalusList[ByteString]): Int = {
                    var count = 0
                    var power = 1
                    var current = tree
                    while current != ScalusList.Nil do {
                        current match {
                            case ScalusList.Cons(hash, tail) =>
                                if hash != emptyHash then count += power
                                power *= 2
                                current = tail
                            case _ => current = ScalusList.Nil
                        }
                    }
                    count
                }

                val confirmedHeight = chainState.blockHeight.toInt
                val numConfirmedBlocks = countBlocksInTree(chainState.confirmedBlocksTree)
                val distanceFromTip = confirmedHeight - blockHeader.height
                val blockIndex = numConfirmedBlocks - 1 - distanceFromTip

                println(s"  Oracle confirmed height: $confirmedHeight")
                println(s"  Blocks in tree: $numConfirmedBlocks")
                println(s"  Block ${blockHeader.height} is $distanceFromTip blocks from tip")
                println(s"  Target block index: $blockIndex")

                val firstBlockHeight = confirmedHeight - numConfirmedBlocks + 1
                val confirmedBlockHashes =
                    try {
                        (firstBlockHeight to confirmedHeight).map { height =>
                            val hash = Await.result(rpc.getBlockHash(height), 30.seconds)
                            ByteString.fromHex(hash).reverse
                        }.toList
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"Error fetching confirmed block hashes: ${e.getMessage}"
                            )
                            return 1
                    }

                val blockTree = MerkleTree.fromHashes(confirmedBlockHashes)
                val blockMerkleProof = blockTree.makeMerkleProof(blockIndex)

                val targetBlockHashBytes = ByteString.fromHex(targetBlockHash).reverse
                val calculatedBlockRoot =
                    MerkleTree.calculateMerkleRootFromProof(
                      blockIndex,
                      targetBlockHashBytes,
                      blockMerkleProof
                    )

                val oracleTreeRoot = BitcoinValidator.getMerkleRoot(chainState.confirmedBlocksTree)

                println(s"  Calculated block tree root: ${calculatedBlockRoot.toHex}")
                println(s"  Oracle tree root: ${oracleTreeRoot.toHex}")

                if calculatedBlockRoot != oracleTreeRoot then {
                    println()
                    println(
                      "Block tree root mismatch - this may be due to rolling tree differences"
                    )
                } else {
                    println(s"  Block proof verified!")
                }

                val rawBlockHeader =
                    try {
                        Await.result(rpc.getBlockHeaderRaw(targetBlockHash), 30.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error fetching raw block header: ${e.getMessage}")
                            return 1
                    }

                // Output both proofs
                println()
                println("=" * 70)
                println("TWO-PROOF TRANSACTION VERIFICATION")
                println("=" * 70)
                println()
                println("--- Transaction Details ---")
                println(s"Transaction ID: $btcTxId")
                println(s"Block Hash: $targetBlockHash")
                println(s"Block Height: ${blockHeader.height}")
                confirmations.foreach(c => println(s"Confirmations: $c"))
                println()
                println("--- Oracle State ---")
                println(s"Oracle UTxO: $oracleTxHash:$oracleOutputIndex")
                println(s"Oracle Height: ${chainState.blockHeight}")
                println()
                println("--- PROOF 1: Transaction in Block ---")
                println(s"TX Index: $txIdx")
                println(s"TX Merkle Root: ${merkleRootVal.toHex}")
                println(s"TX Proof (${merkleProof.length} hashes):")
                merkleProof.zipWithIndex.foreach { case (hash, i) =>
                    println(s"  [$i] ${hash.toHex}")
                }
                println()
                println("--- PROOF 2: Block in Oracle ---")
                println(s"Block Index: $blockIndex")
                println(s"Block Tree Root: ${calculatedBlockRoot.toHex}")
                println(s"Block Proof (${blockMerkleProof.length} hashes):")
                blockMerkleProof.zipWithIndex.foreach { case (hash, i) =>
                    println(s"  [$i] ${hash.toHex}")
                }
                println()
                println("--- Block Header (80 bytes) ---")
                println(s"$rawBlockHeader")
                println()
                println("--- For BitcoinDependentLock unlock ---")
                println(s"bitcoin-dependent-lock unlock <LOCKED_UTXO> \\")
                println(s"  --tx-index $txIdx \\")
                println(s"  --tx-proof ${merkleProof.map(_.toHex).mkString(",")} \\")
                println(s"  --block-index $blockIndex \\")
                println(s"  --block-proof ${blockMerkleProof.map(_.toHex).mkString(",")} \\")
                println(s"  --block-header $rawBlockHeader \\")
                println(s"  --oracle-utxo $oracleTxHash:$oracleOutputIndex")
                println()
                println("=" * 70)
                0
        }
    }
}
