package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.TransactionInput

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary, boundary.break
import scalus.utils.await

/** Initialize new oracle from Bitcoin block */
case class InitOracleCommand(startBlock: Option[Long], dryRun: Boolean = false)
    extends Command
    with LazyLogging {

    override def execute(config: BinocularConfig): Int = boundary {
        println("Initializing new oracle...")
        if dryRun then println("  (dry-run mode - will not submit)")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config) match {
            case Right(s) => s
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }

        val blockHeight = startBlock.orElse(config.oracle.startHeight).getOrElse {
            System.err.println("Error: No start block height specified")
            System.err.println("  Use --start-block <HEIGHT> or configure ORACLE_START_HEIGHT")
            break(1)
        }

        val walletAddr =
            setup.hdAccount.baseAddress(config.cardano.scalusNetwork).toBech32.getOrElse("?")
        println(s"Start Block Height: $blockHeight")
        println(s"Bitcoin Network: ${config.bitcoinNode.network}")
        println(s"Cardano Network: ${config.cardano.network}")
        println(s"Oracle Address: ${setup.scriptAddressBech32}")
        println(s"Wallet loaded: $walletAddr")
        println()

        println("Step 1: Connecting to Bitcoin RPC...")
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        println(s"Connected to Bitcoin node at ${config.bitcoinNode.url}")
        println()
        println(s"Step 2: Fetching initial chain state from block $blockHeight...")

        val initialState =
            try {
                BitcoinChainState.getInitialChainState(rpc, blockHeight.toInt).await(30.seconds)
            } catch {
                case e: Exception =>
                    System.err.println(s"Error fetching Bitcoin block: ${e.getMessage}")
                    println()
                    println("Make sure:")
                    println("  1. Bitcoin node is running and accessible")
                    println("  2. RPC credentials are correct")
                    println(s"  3. Block height $blockHeight exists")
                    break(1)
            }

        println(s"Fetched initial state:")
        println(s"  Block Height: ${initialState.ctx.height}")
        println(s"  Block Hash: ${initialState.ctx.lastBlockHash.toHex}")
        println(s"  Block Timestamp: ${initialState.ctx.timestamps.head}")
        println(s"  Current Target: ${initialState.ctx.currentBits.toHex}")
        println(s"  Recent Timestamps: ${initialState.ctx.timestamps.size} entries")

        val requiredTimestamps = 11
        if initialState.ctx.timestamps.size < requiredTimestamps then {
            System.err.println(s"Error: Insufficient timestamps for median-time-past validation")
            System.err.println(
              s"  Got ${initialState.ctx.timestamps.size}, need $requiredTimestamps"
            )
            break(1)
        }
        println(s"Validated: All $requiredTimestamps timestamps present for median-time-past")

        if dryRun then {
            println()
            println("Dry-run complete. Transaction would initialize oracle with:")
            println(s"  Block Height: ${initialState.ctx.height}")
            println(s"  Block Hash: ${initialState.ctx.lastBlockHash.toHex}")
            println(s"  Oracle Address: ${setup.scriptAddressBech32}")
            break(0)
        }

        println()
        println("Step 3: Fetching one-shot UTxO for NFT minting...")
        val oneShotRef = setup.params.oneShotTxOutRef
        val oneShotInput = TransactionInput(
          scalus.cardano.ledger.TransactionHash.fromByteString(oneShotRef.id.hash),
          oneShotRef.idx.toInt
        )
        val oneShotUtxo =
            try {
                setup.provider.findUtxo(oneShotInput).await(30.seconds) match {
                    case Right(u) => u
                    case Left(e) =>
                        System.err.println(s"Error: One-shot UTxO not found: $e")
                        System.err.println(
                          s"  Configured tx-out-ref: ${config.oracle.txOutRef}"
                        )
                        System.err.println(
                          "  Make sure the UTxO exists and hasn't been spent"
                        )
                        break(1)
                }
            } catch {
                case e: Exception =>
                    System.err.println(
                      s"Error fetching one-shot UTxO: ${e.getMessage}"
                    )
                    break(1)
            }
        println(s"  One-shot UTxO: ${config.oracle.txOutRef}")

        println()
        println("Step 4: Building and submitting Cardano transaction...")

        OracleTransactions.buildAndSubmitInitTransaction(
          setup.signer,
          setup.provider,
          setup.scriptAddress,
          setup.sponsorAddress,
          initialState,
          setup.script,
          oneShotUtxo,
          timeout = timeout
        ) match {
            case Right(txHash) =>
                println()
                println("Oracle initialized successfully!")
                println(s"  Transaction Hash: $txHash")
                println(s"  Oracle Address: ${setup.scriptAddressBech32}")
                println()
                println("Next steps:")
                println(s"  1. Wait for transaction confirmation")
                println(s"  2. Verify oracle: binocular verify-oracle $txHash:0")
                println(s"  3. List oracles: binocular list-oracles")
                0
            case Left(err) =>
                println()
                System.err.println(s"Error submitting transaction: $err")
                1
        }
    }
}
