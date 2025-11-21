package binocular.cli.commands

import binocular.{BitcoinChainState, BitcoinNodeConfig, CardanoConfig, OracleConfig, OracleTransactions, SimpleBitcoinRpc, WalletConfig}
import binocular.cli.Command

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** Initialize new oracle from Bitcoin block */
case class InitOracleCommand(startBlock: Option[Long]) extends Command {

    override def execute(): Int = {
        println("Initializing new oracle...")
        println()

        // Load configurations
        val bitcoinConfig = BitcoinNodeConfig.load()
        val cardanoConfig = CardanoConfig.load()
        val oracleConfig = OracleConfig.load()
        val walletConfig = WalletConfig.load()

        (bitcoinConfig, cardanoConfig, oracleConfig, walletConfig) match {
            case (Right(btcConf), Right(cardanoConf), Right(oracleConf), Right(walletConf)) =>
                initializeOracle(btcConf, cardanoConf, oracleConf, walletConf)

            case (Left(err), _, _, _) =>
                System.err.println(s"Error loading Bitcoin config: $err")
                1
            case (_, Left(err), _, _) =>
                System.err.println(s"Error loading Cardano config: $err")
                1
            case (_, _, Left(err), _) =>
                System.err.println(s"Error loading Oracle config: $err")
                1
            case (_, _, _, Left(err)) =>
                System.err.println(s"Error loading Wallet config: $err")
                1
        }
    }

    private def initializeOracle(
        btcConf: BitcoinNodeConfig,
        cardanoConf: CardanoConfig,
        oracleConf: OracleConfig,
        walletConf: WalletConfig
    ): Int = {
        // Determine start block height
        val blockHeight = startBlock.orElse(oracleConf.startHeight).getOrElse {
            System.err.println("Error: No start block height specified")
            System.err.println("  Use --start-block <HEIGHT> or configure ORACLE_START_HEIGHT")
            return 1
        }

        println(s"Start Block Height: $blockHeight")
        println(s"Bitcoin Network: ${btcConf.network}")
        println(s"Cardano Network: ${cardanoConf.network}")
        println(s"Oracle Address: ${oracleConf.scriptAddress}")
        println()

        // Create account for signing
        val account = walletConf.createAccount() match {
            case Right(acc) =>
                println(s"✓ Wallet loaded: ${acc.baseAddress()}")
                acc
            case Left(err) =>
                System.err.println(s"Error creating wallet account: $err")
                return 1
        }

        // Create Cardano backend service
        val backendService = cardanoConf.createBackendService() match {
            case Right(service) =>
                println(s"✓ Connected to Cardano backend (${cardanoConf.backend})")
                service
            case Left(err) =>
                System.err.println(s"Error creating backend service: $err")
                return 1
        }

        println()
        println("Step 1: Connecting to Bitcoin RPC...")

        // Set up execution context for async operations
        given ec: ExecutionContext = ExecutionContext.global

        // Create Bitcoin RPC client
        val rpc = new SimpleBitcoinRpc(btcConf)

        println(s"✓ Connected to Bitcoin node at ${btcConf.url}")
        println()
        println(s"Step 2: Fetching initial chain state from block $blockHeight...")

        // Fetch initial chain state
        val initialStateFuture = BitcoinChainState.getInitialChainState(rpc, blockHeight.toInt)
        val initialState =
            try {
                Await.result(initialStateFuture, 30.seconds)
            } catch {
                case e: Exception =>
                    System.err.println(s"✗ Error fetching Bitcoin block: ${e.getMessage}")
                    println()
                    println("Make sure:")
                    println("  1. Bitcoin node is running and accessible")
                    println("  2. RPC credentials are correct")
                    println("  3. Block height $blockHeight exists")
                    return 1
            }

        println(s"✓ Fetched initial state:")
        println(s"  Block Height: ${initialState.blockHeight}")
        println(s"  Block Hash: ${initialState.blockHash.toHex}")
        println(s"  Block Timestamp: ${initialState.blockTimestamp}")
        println(s"  Current Target: ${initialState.currentTarget.toHex}")
        println(s"  Recent Timestamps: ${initialState.recentTimestamps.size} entries")

        // Validate that we have enough timestamps for median-time-past validation
        val requiredTimestamps = 11 // MedianTimeSpan
        if initialState.recentTimestamps.size < requiredTimestamps then {
            System.err.println(s"✗ Error: Insufficient timestamps for median-time-past validation")
            System.err.println(
              s"  Got ${initialState.recentTimestamps.size}, need $requiredTimestamps"
            )
            System.err.println(s"  This is a bug - please report it")
            return 1
        }
        println(s"✓ Validated: All $requiredTimestamps timestamps present for median-time-past")

        println()
        println("Step 3: Building and submitting Cardano transaction...")

        // Build and submit transaction
        val scriptAddress =
            new com.bloxbean.cardano.client.address.Address(oracleConf.scriptAddress)
        val txResult = OracleTransactions.buildAndSubmitInitTransaction(
          account,
          backendService,
          scriptAddress,
          initialState
        )

        txResult match {
            case Right(txHash) =>
                println()
                println("✓ Oracle initialized successfully!")
                println(s"  Transaction Hash: $txHash")
                println(s"  Oracle Address: ${oracleConf.scriptAddress}")
                println()
                println("Next steps:")
                println(s"  1. Wait for transaction confirmation")
                println(s"  2. Verify oracle: binocular verify-oracle $txHash:0")
                println(s"  3. List oracles: binocular list-oracles")
                0
            case Left(err) =>
                println()
                System.err.println(s"✗ Error submitting transaction: $err")
                1
        }
    }
}
