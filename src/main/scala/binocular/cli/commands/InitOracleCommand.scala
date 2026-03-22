package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, OracleSetup}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
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

        if dryRun then {
            println()
            println("Dry-run complete. Transaction would initialize oracle with:")
            println(s"  Block Height: ${initialState.ctx.height}")
            println(s"  Block Hash: ${initialState.ctx.lastBlockHash.toHex}")
            println(s"  Oracle Address: ${setup.scriptAddressBech32}")
            break(0)
        }

        val (oneShotUtxo, referenceScriptUtxo) =
            if isPlaceholderTxOutRef(config.oracle.txOutRef) then {
                // Placeholder flow: create one-shot UTxO first, then parameterize, then deploy

                // Step 3: Create dedicated one-shot UTxO
                println()
                println("Step 3: Creating dedicated one-shot UTxO (10 ADA)...")
                val (oneShotTxHash, oneShotIdx, oneShotOutput) =
                    OracleTransactions.createOneShotUtxo(
                      setup.signer,
                      setup.provider,
                      setup.sponsorAddress,
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
                OracleTransactions.waitForUtxo(setup.provider, oneShotInput, timeout) match {
                    case Right(_) => println("  One-shot UTxO confirmed.")
                    case Left(err) =>
                        System.err.println(s"Error: $err")
                        break(1)
                }
                val oneShot = Utxo(oneShotInput, oneShotOutput)

                // Step 4: Parameterize script with the one-shot UTxO
                println()
                println("Step 4: Parameterizing script with one-shot UTxO...")
                val txOutRef = TxOutRef(TxId(oneShotInput.transactionId), oneShotInput.index)
                val oracleConf = config.oracle
                val newParams = BitcoinContract.validatorParams(
                  txOutRef,
                  setup.params.owner,
                  maturationConfirmations = oracleConf.maturationConfirmations,
                  challengeAging = oracleConf.challengeAging,
                  closureTimeout = oracleConf.closureTimeout,
                  testingMode = oracleConf.testingMode
                )
                val newCompiled = BitcoinContract.makeContract(newParams)
                setup = OracleSetup(
                  newParams,
                  newCompiled,
                  setup.hdAccount,
                  setup.provider,
                  setup.network
                )
                println(s"  Oracle Params: $newParams")
                println(s"  Oracle Address: ${setup.scriptAddressBech32}")

                // Step 5: Deploy reference script (now with correct params)
                println()
                println(s"Step 5: Deploying reference script ${setup.script.scriptHash}...")
                val (deployTxHash, deployIdx, deployOutput) =
                    OracleTransactions.deployReferenceScript(
                      setup.signer,
                      setup.provider,
                      setup.sponsorAddress,
                      setup.sponsorAddress,
                      setup.script,
                      timeout
                    ) match {
                        case Right(result) =>
                            println(
                              s"  Reference script tx submitted: ${result._1}#${result._2}"
                            )
                            result
                        case Left(err) =>
                            System.err.println(s"Error deploying reference script: $err")
                            break(1)
                    }

                val refInput =
                    TransactionInput(TransactionHash.fromHex(deployTxHash), deployIdx)
                println("  Waiting for reference script UTxO to be confirmed...")
                OracleTransactions.waitForUtxo(setup.provider, refInput, timeout) match {
                    case Right(_) => println("  Reference script UTxO confirmed.")
                    case Left(err) =>
                        System.err.println(s"Error: $err")
                        break(1)
                }
                val refUtxo = Utxo(refInput, deployOutput)

                (oneShot, refUtxo)
            } else {
                // Configured flow: deploy reference script, then resolve configured one-shot UTxO

                // Step 3: Deploy reference script
                println()
                println(s"Step 3: Deploying reference script ${setup.script.scriptHash}...")
                val (deployTxHash, deployIdx, deployOutput) =
                    OracleTransactions.deployReferenceScript(
                      setup.signer,
                      setup.provider,
                      setup.sponsorAddress,
                      setup.sponsorAddress,
                      setup.script,
                      timeout
                    ) match {
                        case Right(result) =>
                            println(
                              s"  Reference script tx submitted: ${result._1}#${result._2}"
                            )
                            result
                        case Left(err) =>
                            System.err.println(s"Error deploying reference script: $err")
                            break(1)
                    }

                val refInput =
                    TransactionInput(TransactionHash.fromHex(deployTxHash), deployIdx)
                println("  Waiting for reference script UTxO to be confirmed...")
                OracleTransactions.waitForUtxo(setup.provider, refInput, timeout) match {
                    case Right(_) => println("  Reference script UTxO confirmed.")
                    case Left(err) =>
                        System.err.println(s"Error: $err")
                        break(1)
                }
                val refUtxo = Utxo(refInput, deployOutput)

                // Step 4: Resolve configured one-shot UTxO
                println()
                println("Step 4: Resolving one-shot UTxO for NFT minting...")
                val oneShotRef = setup.params.oneShotTxOutRef
                val oneShotInput = TransactionInput(
                  TransactionHash.fromByteString(oneShotRef.id.hash),
                  oneShotRef.idx.toInt
                )
                println(s"  One-shot UTxO: ${config.oracle.txOutRef}")
                val oneShot =
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

                (oneShot, refUtxo)
            }

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
