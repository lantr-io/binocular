package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, OracleSetup}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
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
        val oracleConf = config.oracle
        val timeout = oracleConf.transactionTimeout.seconds

        val hdAccount = config.wallet.createHdAccount() match {
            case Right(a) => a
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }
        val provider = config.cardano.createBlockchainProvider() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }
        val network = config.cardano.scalusNetwork
        val signer = hdAccount.signerForUtxos
        val sponsorAddress = hdAccount.baseAddress(network)

        val blockHeight = startBlock.orElse(oracleConf.startHeight).getOrElse {
            System.err.println("Error: No start block height specified")
            System.err.println("  Use --start-block <HEIGHT> or configure ORACLE_START_HEIGHT")
            break(1)
        }

        val walletAddr = sponsorAddress.toBech32.getOrElse("?")
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

        if dryRun then {
            println()
            println("Dry-run complete. Transaction would initialize oracle with:")
            println(s"  Block Height: ${initialState.ctx.height}")
            println(s"  Block Hash: ${initialState.ctx.lastBlockHash.toHex}")
            break(0)
        }

        // Step 3: Create dedicated one-shot UTxO
        println()
        println("Step 3: Creating dedicated one-shot UTxO (10 ADA)...")
        val (oneShotTxHash, oneShotIdx, oneShotOutput) =
            OracleTransactions.createOneShotUtxo(
              signer,
              provider,
              sponsorAddress,
              timeout = timeout
            ) match {
                case Right(result) =>
                    println(s"  One-shot tx submitted: ${result._1}#${result._2}")
                    result
                case Left(err) =>
                    System.err.println(s"Error creating one-shot UTxO: $err")
                    break(1)
            }

        val oneShotInput =
            TransactionInput(TransactionHash.fromHex(oneShotTxHash), oneShotIdx)
        println("  Waiting for one-shot UTxO to be confirmed...")
        OracleTransactions.waitForUtxo(provider, oneShotInput, timeout) match {
            case Right(_) => println("  One-shot UTxO confirmed.")
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }
        val oneShotUtxo = Utxo(oneShotInput, oneShotOutput)

        // Step 4: Parameterize script with the one-shot UTxO
        println()
        println("Step 4: Parameterizing script with one-shot UTxO...")
        val txOutRef = TxOutRef(TxId(oneShotInput.transactionId), oneShotInput.index)
        val owner = PubKeyHash(hdAccount.paymentKeyHash)
        val params = BitcoinContract.validatorParams(
          txOutRef,
          owner,
          maturationConfirmations = oracleConf.maturationConfirmations,
          challengeAging = oracleConf.challengeAging,
          closureTimeout = oracleConf.closureTimeout,
          testingMode = oracleConf.testingMode
        )
        val compiled = BitcoinContract.makeContract(params)
        val setup = OracleSetup(params, compiled, hdAccount, provider, network)
        println(s"  Oracle Params: $params")
        println(s"  Oracle Address: ${setup.scriptAddressBech32}")

        // Step 5: Deploy reference script
        println()
        println(s"Step 5: Deploying reference script ${setup.script.scriptHash}...")
        val (deployTxHash, deployIdx, deployOutput) =
            OracleTransactions.deployReferenceScript(
              signer,
              provider,
              sponsorAddress,
              sponsorAddress,
              setup.script,
              timeout
            ) match {
                case Right(result) =>
                    println(s"  Reference script tx submitted: ${result._1}#${result._2}")
                    result
                case Left(err) =>
                    System.err.println(s"Error deploying reference script: $err")
                    break(1)
            }

        val refInput =
            TransactionInput(TransactionHash.fromHex(deployTxHash), deployIdx)
        println("  Waiting for reference script UTxO to be confirmed...")
        OracleTransactions.waitForUtxo(provider, refInput, timeout) match {
            case Right(_) => println("  Reference script UTxO confirmed.")
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }
        val referenceScriptUtxo = Utxo(refInput, deployOutput)

        // Final step: Build and submit init transaction
        println()
        println("Building and submitting init transaction...")

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
        println(s"  One-shot input: ${oneShotUtxo.input}")
        0
    }
}
