package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, OracleSetup}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary, boundary.break
import scalus.utils.await

/** Initialize new oracle from Bitcoin block */
case class InitOracleCommand(startBlock: Option[Long], dryRun: Boolean = false)
    extends Command
    with LazyLogging {

    /** Check if the configured tx-out-ref is a placeholder (all zeros) */
    private def isPlaceholderTxOutRef(txOutRef: String): Boolean =
        txOutRef.split("#") match {
            case Array(hash, _) => hash.forall(_ == '0')
            case _              => false
        }

    override def execute(config: BinocularConfig): Int = boundary {
        println("Initializing new oracle...")
        if dryRun then println("  (dry-run mode - will not submit)")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        var setup = CommandHelpers.setupOracle(config) match {
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

        val walletAddr = setup.sponsorAddress.toBech32.getOrElse("?")
        println(s"Start Block Height: $blockHeight")
        println(s"Bitcoin Network: ${config.bitcoinNode.network}")
        println(s"Cardano Network: ${config.cardano.network}")
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

        println()
        println("Step 3: Resolving one-shot UTxO for NFT minting...")

        val oneShotUtxo = if isPlaceholderTxOutRef(config.oracle.txOutRef) then {
            println("  Configured tx-out-ref is placeholder, auto-detecting from sponsor wallet...")
            val utxos = setup.provider.findUtxos(setup.sponsorAddress).await(30.seconds) match {
                case Right(u) => u
                case Left(e) =>
                    System.err.println(s"Error fetching sponsor UTxOs: $e")
                    break(1)
            }
            val (input, output) = utxos.headOption.getOrElse {
                System.err.println("Error: No UTxOs found at sponsor address")
                System.err.println(s"  Address: $walletAddr")
                System.err.println("  Fund this address first")
                break(1)
            }
            val utxo = Utxo(input, output)
            println(s"  Auto-detected one-shot UTxO: ${input.transactionId.toHex}#${input.index}")

            // Rebuild setup with actual one-shot UTxO ref
            val txOutRef = TxOutRef(TxId(input.transactionId), input.index)
            val newParams = BitcoinContract.validatorParams(txOutRef, setup.params.owner)
            val newCompiled = BitcoinContract.makeContract(newParams)
            setup =
                OracleSetup(newParams, newCompiled, setup.hdAccount, setup.provider, setup.network)
            println(s"  Oracle Address: ${setup.scriptAddressBech32}")
            utxo
        } else {
            val oneShotRef = setup.params.oneShotTxOutRef
            val oneShotInput = TransactionInput(
              TransactionHash.fromByteString(oneShotRef.id.hash),
              oneShotRef.idx.toInt
            )
            println(s"  One-shot UTxO: ${config.oracle.txOutRef}")
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
        }

        if dryRun then {
            println()
            println("Dry-run complete. Transaction would initialize oracle with:")
            println(s"  Block Height: ${initialState.ctx.height}")
            println(s"  Block Hash: ${initialState.ctx.lastBlockHash.toHex}")
            println(s"  Oracle Address: ${setup.scriptAddressBech32}")
            break(0)
        }

        println()
        println("Step 4: Deploying reference script...")

        val referenceScriptUtxo = OracleTransactions.deployReferenceScript(
          setup.signer,
          setup.provider,
          setup.sponsorAddress,
          setup.scriptAddress,
          setup.script,
          timeout
        ) match {
            case Right((deployTxHash, deployIdx, deployOutput)) =>
                println(s"  Reference script tx submitted: $deployTxHash:$deployIdx")
                val refInput = TransactionInput(TransactionHash.fromHex(deployTxHash), deployIdx)
                println("  Waiting for reference script UTxO to be confirmed...")
                OracleTransactions.waitForUtxo(setup.provider, refInput, timeout) match {
                    case Right(utxo) =>
                        println("  Reference script UTxO confirmed.")
                        utxo
                    case Left(err) =>
                        System.err.println(s"Error: $err")
                        break(1)
                }
            case Left(err) =>
                System.err.println(s"Error deploying reference script: $err")
                break(1)
        }

        println()
        println("Step 5: Building and submitting init transaction...")

        val initTxHash = OracleTransactions.buildAndSubmitInitTransaction(
          setup.signer,
          setup.provider,
          setup.scriptAddress,
          setup.sponsorAddress,
          initialState,
          setup.script,
          oneShotUtxo,
          referenceScriptUtxo,
          timeout = timeout
        ) match {
            case Right(txHash) =>
                println(s"  Transaction Hash: $txHash")
                println(s"  Oracle Address: ${setup.scriptAddressBech32}")
                txHash
            case Left(err) =>
                println()
                System.err.println(s"Error submitting init transaction: $err")
                break(1)
        }

        println()
        println("Oracle initialized successfully!")
        println(s"  Oracle TX: $initTxHash")
        println(s"  Oracle Address: ${setup.scriptAddressBech32}")
        0
    }
}
