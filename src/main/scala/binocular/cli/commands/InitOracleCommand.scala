package binocular.cli.commands

import binocular.*
import binocular.cli.Command
import scalus.cardano.address.Address
import scalus.cardano.txbuilder.TransactionSigner

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary, boundary.break
import scalus.utils.await

/** Initialize new oracle from Bitcoin block */
case class InitOracleCommand(startBlock: Option[Long], dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        println("Initializing new oracle...")
        if dryRun then println("  (dry-run mode - will not submit)")
        println()

        val btcConf = config.bitcoinNode
        val cardanoConf = config.cardano
        val oracleConf = config.oracle
        val walletConf = config.wallet

        val params = oracleConf.toBitcoinValidatorParams() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
        }

        val oracleScriptAddress = oracleConf.scriptAddress(cardanoConf.cardanoNetwork) match {
            case Right(addr) => addr
            case Left(err) =>
                System.err.println(s"Error deriving script address: $err")
                return 1
        }

        initializeOracle(btcConf, cardanoConf, oracleConf, walletConf, params, oracleScriptAddress)
    }

    private def initializeOracle(
        btcConf: BitcoinNodeConfig,
        cardanoConf: CardanoConfig,
        oracleConf: OracleConfig,
        walletConf: WalletConfig,
        params: BitcoinValidatorParams,
        oracleScriptAddress: String
    ): Int = boundary {
        val blockHeight = startBlock.orElse(oracleConf.startHeight).getOrElse {
            System.err.println("Error: No start block height specified")
            System.err.println("  Use --start-block <HEIGHT> or configure ORACLE_START_HEIGHT")
            break(1)
        }

        println(s"Start Block Height: $blockHeight")
        println(s"Bitcoin Network: ${btcConf.network}")
        println(s"Cardano Network: ${cardanoConf.network}")
        println(s"Oracle Address: $oracleScriptAddress")
        println()

        given ec: ExecutionContext = ExecutionContext.global

        val hdAccount = walletConf.createHdAccount() match {
            case Right(acc) =>
                val addr = acc.baseAddress(cardanoConf.scalusNetwork).toBech32.getOrElse("?")
                println(s"Wallet loaded: $addr")
                acc
            case Left(err) =>
                System.err.println(s"Error creating wallet account: $err")
                break(1)
        }
        val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
        val sponsorAddress = hdAccount.baseAddress(cardanoConf.scalusNetwork)

        val provider = cardanoConf.createBlockchainProvider() match {
            case Right(p) =>
                println(s"Connected to Cardano backend (${cardanoConf.backend})")
                p
            case Left(err) =>
                System.err.println(s"Error creating blockchain provider: $err")
                break(1)
        }

        println()
        println("Step 1: Connecting to Bitcoin RPC...")

        val rpc = new SimpleBitcoinRpc(btcConf)

        println(s"Connected to Bitcoin node at ${btcConf.url}")
        println()
        println(s"Step 2: Fetching initial chain state from block $blockHeight...")

        val initialStateFuture = BitcoinChainState.getInitialChainState(rpc, blockHeight.toInt)
        val initialState =
            try {
                initialStateFuture.await(30.seconds)
            } catch {
                case e: Exception =>
                    System.err.println(s"Error fetching Bitcoin block: ${e.getMessage}")
                    println()
                    println("Make sure:")
                    println("  1. Bitcoin node is running and accessible")
                    println("  2. RPC credentials are correct")
                    println("  3. Block height $blockHeight exists")
                    break(1)
            }

        println(s"Fetched initial state:")
        println(s"  Block Height: ${initialState.blockHeight}")
        println(s"  Block Hash: ${initialState.blockHash.toHex}")
        println(s"  Block Timestamp: ${initialState.recentTimestamps.head}")
        println(s"  Current Target: ${initialState.currentTarget.toHex}")
        println(s"  Recent Timestamps: ${initialState.recentTimestamps.size} entries")

        val requiredTimestamps = 11
        if initialState.recentTimestamps.size < requiredTimestamps then {
            System.err.println(s"Error: Insufficient timestamps for median-time-past validation")
            System.err.println(
              s"  Got ${initialState.recentTimestamps.size}, need $requiredTimestamps"
            )
            break(1)
        }
        println(s"Validated: All $requiredTimestamps timestamps present for median-time-past")

        if dryRun then {
            println()
            println("Dry-run complete. Transaction would initialize oracle with:")
            println(s"  Block Height: ${initialState.blockHeight}")
            println(s"  Block Hash: ${initialState.blockHash.toHex}")
            println(s"  Oracle Address: $oracleScriptAddress")
            return 0
        }

        println()
        println("Step 3: Building and submitting Cardano transaction...")

        val scriptAddress = Address.fromBech32(oracleScriptAddress)
        val txResult = OracleTransactions.buildAndSubmitInitTransaction(
          signer,
          provider,
          scriptAddress,
          sponsorAddress,
          initialState,
          timeout = oracleConf.transactionTimeout.seconds
        )

        txResult match {
            case Right(txHash) =>
                println()
                println("Oracle initialized successfully!")
                println(s"  Transaction Hash: $txHash")
                println(s"  Oracle Address: $oracleScriptAddress")
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
