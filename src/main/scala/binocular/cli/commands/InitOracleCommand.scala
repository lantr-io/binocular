package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console, OracleSetup}
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Initialize new oracle from Bitcoin block */
case class InitOracleCommand(startBlock: Option[Long], dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Oracle Init")
        if dryRun then Console.warn("Dry-run mode — will not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val oracleConf = config.oracle
        val timeout = oracleConf.transactionTimeout.seconds

        val hdAccount = config.wallet.createHdAccount().valueOr { err =>
            Console.error(err)
            break(1)
        }
        val provider = config.cardano.createBlockchainProvider().valueOr { err =>
            Console.error(err)
            break(1)
        }
        val network = config.cardano.scalusNetwork
        val sponsorAddress = hdAccount.baseAddress(network)

        val blockHeight = startBlock.orElse(oracleConf.startHeight).getOrElse {
            Console.error("No start block height specified")
            Console.error("Use --start-block <HEIGHT> or configure ORACLE_START_HEIGHT")
            break(1)
        }

        val walletAddr = sponsorAddress.toBech32.getOrElse("?")
        Console.info("Network", s"${config.bitcoinNode.network} → ${config.cardano.network}")
        Console.info("Start Height", f"$blockHeight%,d")
        Console.info("Wallet", walletAddr)
        println()

        // Step 1
        Console.step(1, "Connecting to Bitcoin RPC")
        val t1 = System.nanoTime()
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        Console.success(s"Connected to ${config.bitcoinNode.url}")
        println()

        // Step 2
        Console.step(2, "Fetching initial chain state")
        val t2 = System.nanoTime()
        val initialState =
            try {
                BitcoinChainState.getInitialChainState(rpc, blockHeight.toInt).await(30.seconds)
            } catch {
                case e: Exception =>
                    Console.error(s"Fetching Bitcoin block: ${e.getMessage}")
                    Console.error("Make sure:")
                    Console.error("  1. Bitcoin node is running and accessible")
                    Console.error("  2. RPC credentials are correct")
                    Console.error(s"  3. Block height $blockHeight exists")
                    break(1)
            }

        Console.info("Height", initialState.ctx.height)
        Console.info("Hash", initialState.ctx.lastBlockHash.toHex)
        Console.info("Timestamp", initialState.ctx.timestamps.head)
        Console.info("Target", initialState.ctx.currentBits.toHex)

        val requiredTimestamps = 11
        if initialState.ctx.timestamps.size < requiredTimestamps then {
            Console.error("Insufficient timestamps for median-time-past validation")
            Console.error(
              s"Got ${initialState.ctx.timestamps.size}, need $requiredTimestamps"
            )
            break(1)
        }
        val step2Time = (System.nanoTime() - t2) / 1e9
        Console.success(
          f"${initialState.ctx.timestamps.size} timestamps validated ($step2Time%.1fs)"
        )

        if dryRun then {
            println()
            Console.success("Dry-run complete. Transaction would initialize oracle with:")
            Console.info("Height", initialState.ctx.height)
            Console.info("Hash", initialState.ctx.lastBlockHash.toHex)
            break(0)
        }

        // Step 3
        println()
        Console.step(3, "Creating one-shot UTxO")
        val t3 = System.nanoTime()
        val (oneShotTxHash, oneShotIdx, oneShotOutput) =
            OracleTransactions.createOneShotUtxo(
              provider,
              hdAccount,
              timeout = timeout
            ) match {
                case Right(result) =>
                    Console.tx("Tx", s"${result._1}#${result._2}")
                    result
                case Left(err) =>
                    Console.error(s"Creating one-shot UTxO: $err")
                    break(1)
            }

        val oneShotInput =
            TransactionInput(TransactionHash.fromHex(oneShotTxHash), oneShotIdx)
        OracleTransactions.waitForUtxo(provider, oneShotInput, timeout) match {
            case Right(_) =>
                val step3Time = (System.nanoTime() - t3) / 1e9
                Console.success(f"Confirmed ($step3Time%.1fs)")
            case Left(err) =>
                Console.error(err)
                break(1)
        }
        val oneShotUtxo = Utxo(oneShotInput, oneShotOutput)

        // Step 4
        println()
        Console.step(4, "Parameterizing script")
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
        Console.info("Address", setup.scriptAddressBech32)
        Console.info("Owner", owner.hash.toHex)

        // Step 5
        println()
        Console.step(5, "Deploying reference script")
        val t5 = System.nanoTime()
        val (deployTxHash, deployIdx, deployOutput) =
            OracleTransactions.deployReferenceScript(
              provider,
              hdAccount,
              compiled,
              timeout
            ) match {
                case Right(result) =>
                    Console.tx("Tx", s"${result._1}#${result._2}")
                    result
                case Left(err) =>
                    Console.error(s"Deploying reference script: $err")
                    break(1)
            }

        val refInput =
            TransactionInput(TransactionHash.fromHex(deployTxHash), deployIdx)
        OracleTransactions.waitForUtxo(provider, refInput, timeout) match {
            case Right(_) =>
                val step5Time = (System.nanoTime() - t5) / 1e9
                Console.success(f"Confirmed ($step5Time%.1fs)")
            case Left(err) =>
                Console.error(err)
                break(1)
        }
        val referenceScriptUtxo = Utxo(refInput, deployOutput)

        // Step 6
        println()
        Console.step(6, "Submitting init transaction")
        val t6 = System.nanoTime()

        val txResult = OracleTransactions
            .buildAndSubmitInitTransaction(
              provider,
              hdAccount,
              compiled,
              initialState,
              oneShotUtxo,
              referenceScriptUtxo,
              timeout = timeout
            )
            .valueOr { err =>
                Console.error(s"Submitting init transaction: $err")
                break(1)
            }

        val step6Time = (System.nanoTime() - t6) / 1e9
        Console.tx("Tx", txResult.txHash)
        Console.metric("Tx size", s"${txResult.txSize} bytes")
        Console.metric("Datum", s"${txResult.datumSize} bytes")
        Console.success(f"Oracle initialized! ($step6Time%.1fs)")

        println()
        Console.separator()
        Console.tx("Oracle TX", txResult.txHash)
        Console.info("Oracle Address", setup.scriptAddressBech32)
        Console.info("One-shot", s"${oneShotUtxo.input}")
        Console.info("Owner PKH", owner.hash.toHex)
        Console.separator()
        0
    }
}
