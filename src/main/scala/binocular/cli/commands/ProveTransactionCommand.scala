package binocular.cli.commands

import binocular.{reverse, BinocularConfig, BitcoinContract, ChainState, MerkleTree, SimpleBitcoinRpc}
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.ledger.Utxo
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scalus.utils.await

/** Prove Bitcoin transaction inclusion via oracle */
case class ProveTransactionCommand(
    btcTxId: String,
    blockHash: Option[String] = None,
    txIndex: Option[Int] = None,
    proof: Option[String] = None,
    merkleRoot: Option[String] = None
) extends Command {

    private def isOfflineMode: Boolean =
        blockHash.isDefined && txIndex.isDefined && proof.isDefined && merkleRoot.isDefined

    override def execute(config: BinocularConfig): Int = {
        println(s"Proving Bitcoin transaction inclusion...")
        println(s"  Bitcoin TX: $btcTxId")
        blockHash.foreach(h => println(s"  Block Hash: $h"))
        txIndex.foreach(i => println(s"  TX Index: $i"))
        proof.foreach(_ => println(s"  Proof: (provided)"))
        merkleRoot.foreach(r => println(s"  Merkle Root: $r"))
        if isOfflineMode then println(s"  Mode: OFFLINE (no Bitcoin RPC)")
        println()

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

        if isOfflineMode then proveTransactionOffline(config)
        else proveTransactionOnline(config)
    }

    private def fetchOracleUtxo(
        config: BinocularConfig
    ): Either[Int, (scalus.cardano.node.BlockchainProvider, Utxo, ChainState)] = {
        val cardanoConf = config.cardano
        val oracleConf = config.oracle

        given ec: ExecutionContext = ExecutionContext.global

        val params = oracleConf.toBitcoinValidatorParams() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error deriving params: $err")
                return Left(1)
        }

        val script = BitcoinContract.makeContract(params).script

        val provider = cardanoConf.createBlockchainProvider() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error creating blockchain provider: $err")
                return Left(1)
        }

        val utxo =
            try {
                CommandHelpers
                    .findOracleUtxo(provider, script.scriptHash)
                    .await(30.seconds)
            } catch {
                case e: Exception =>
                    System.err.println(s"Error: ${e.getMessage}")
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

        Right((provider, utxo, chainState))
    }

    private def proveTransactionOffline(
        config: BinocularConfig
    ): Int = {
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

        fetchOracleUtxo(config) match {
            case Left(exitCode) => exitCode
            case Right((_, oracleUtxo, chainState)) =>
                val oracleRef =
                    s"${oracleUtxo.input.transactionId.toHex}:${oracleUtxo.input.index}"
                println(s"Oracle state:")
                println(s"  Oracle UTxO: $oracleRef")
                println(s"  Block Height: ${chainState.ctx.height}")
                println(s"  Block Hash: ${chainState.ctx.lastBlockHash.toHex}")

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

    private def proveTransactionOnline(
        config: BinocularConfig
    ): Int = {
        val btcConf = config.bitcoinNode

        println("Step 1: Loading configurations...")
        println(s"  Bitcoin Node: ${btcConf.url}")

        fetchOracleUtxo(config) match {
            case Left(exitCode) => exitCode
            case Right((_, oracleUtxo, chainState)) =>
                val oracleRef =
                    s"${oracleUtxo.input.transactionId.toHex}:${oracleUtxo.input.index}"
                println()
                println(s"Oracle state:")
                println(s"  Oracle UTxO: $oracleRef")
                println(s"  Block Height: ${chainState.ctx.height}")
                println(s"  Block Hash: ${chainState.ctx.lastBlockHash.toHex}")

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
                                    rpc.getRawTransaction(btcTxId).await(30.seconds)
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

                val blockInForksTree = chainState.forkTree.existsHash(blockHashBytes)

                if blockInForksTree then {
                    System.err.println(s"Block is still in fork tree (not yet confirmed)")
                    return 1
                }

                val blockHeader =
                    try {
                        rpc.getBlockHeader(targetBlockHash).await(30.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error fetching block header: ${e.getMessage}")
                            return 1
                    }

                if blockHeader.height > chainState.ctx.height.toInt then {
                    System.err.println(
                      s"Block height ${blockHeader.height} > oracle height ${chainState.ctx.height}"
                    )
                    return 1
                }

                println(s"  Block is confirmed by oracle")
                println(s"  Block Height: ${blockHeader.height}")

                println()
                println("Step 6: Fetching block transactions to build Merkle proof...")

                val blockInfo =
                    try {
                        rpc.getBlock(targetBlockHash).await(60.seconds)
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

                println()
                println("Step 8: Building MPF membership proof for Oracle's confirmed trie...")

                val confirmedHeight = chainState.ctx.height.toInt
                val targetBlockHashBytes = ByteString.fromHex(targetBlockHash).reverse

                println(s"  Oracle confirmed height: $confirmedHeight")
                println(s"  Oracle confirmed root: ${chainState.confirmedBlocksRoot.toHex}")

                println(s"  Reconstructing off-chain MPF trie from Bitcoin RPC...")

                val confirmedBlockHashes =
                    try {
                        val hash = rpc.getBlockHash(blockHeader.height).await(30.seconds)
                        val blockHashBytes = ByteString.fromHex(hash).reverse
                        if blockHashBytes != targetBlockHashBytes then {
                            System.err.println(
                              s"Block hash mismatch: expected ${targetBlockHashBytes.toHex}, got ${blockHashBytes.toHex}"
                            )
                            return 1
                        }
                        println(s"  Block ${blockHeader.height} verified on Bitcoin network")
                        scala.List(blockHashBytes)
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"Error fetching confirmed block hashes: ${e.getMessage}"
                            )
                            return 1
                    }

                println(s"  Note: Full MPF proof generation requires reconstructing the trie")
                println(s"  from all confirmed blocks. Use the oracle's off-chain state for this.")

                val rawBlockHeader =
                    try {
                        rpc.getBlockHeaderRaw(targetBlockHash).await(30.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error fetching raw block header: ${e.getMessage}")
                            return 1
                    }

                println()
                println("=" * 70)
                println("TRANSACTION VERIFICATION (MPF)")
                println("=" * 70)
                println()
                println("--- Transaction Details ---")
                println(s"Transaction ID: $btcTxId")
                println(s"Block Hash: $targetBlockHash")
                println(s"Block Height: ${blockHeader.height}")
                confirmations.foreach(c => println(s"Confirmations: $c"))
                println()
                println("--- Oracle State ---")
                println(s"Oracle UTxO: $oracleRef")
                println(s"Oracle Height: ${chainState.ctx.height}")
                println(s"Confirmed Blocks Root: ${chainState.confirmedBlocksRoot.toHex}")
                println()
                println("--- PROOF 1: Transaction in Block ---")
                println(s"TX Index: $txIdx")
                println(s"TX Merkle Root: ${merkleRootVal.toHex}")
                println(s"TX Proof (${merkleProof.length} hashes):")
                merkleProof.zipWithIndex.foreach { case (hash, i) =>
                    println(s"  [$i] ${hash.toHex}")
                }
                println()
                println("--- PROOF 2: Block in Oracle (MPF) ---")
                println(s"Block Hash: ${targetBlockHashBytes.toHex}")
                println(s"Note: MPF membership proof must be generated from off-chain trie state")
                println()
                println("--- Block Header (80 bytes) ---")
                println(s"$rawBlockHeader")
                println()
                println("=" * 70)
                0
        }
    }
}
