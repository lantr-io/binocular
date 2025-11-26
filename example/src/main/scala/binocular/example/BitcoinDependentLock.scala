package binocular.example

import binocular.{reverse, BitcoinNodeConfig, CardanoConfig, CardanoNetwork, MerkleTree, OracleConfig, SimpleBitcoinRpc, TransactionVerifierContract, TxVerifierDatum, TxVerifierRedeemer, WalletConfig}
import com.bloxbean.cardano.client.address.{Address, AddressProvider}
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.monovore.decline.*
import cats.implicits.*
import scalus.bloxbean.Interop
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.fromData
import scalus.builtin.ToData.toData
import scalus.utils.Hex.hexToBytes

import java.math.BigInteger
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** BitcoinDependentLock - Lock and unlock funds based on Bitcoin transaction proofs
  *
  * This application demonstrates using the Binocular oracle to verify Bitcoin transactions on
  * Cardano. Funds can be locked requiring a specific Bitcoin transaction to exist, and unlocked by
  * providing a valid Merkle proof.
  */
@main def bitcoinDependentLock(args: String*): Unit = {
    val exitCode = BitcoinDependentLockApp.run(args)
    System.exit(exitCode)
}

object BitcoinDependentLockApp {

    /** CLI command enum */
    enum Cmd:
        case Lock(
            btcTxId: String,
            merkleRoot: String,
            amountLovelace: Long
        )
        case Unlock(
            utxo: String,
            txIndex: Int,
            proof: String
        )
        case Info

    /** CLI argument parsers */
    object CliParsers {

        val btcTxIdArg: Opts[String] = Opts.argument[String](
          metavar = "BTC_TX_ID"
        )

        val merkleRootOpt: Opts[String] = Opts.option[String](
          "merkle-root",
          help = "Block Merkle root (hex)",
          short = "m"
        )

        val amountOpt: Opts[Long] = Opts
            .option[Long](
              "amount",
              help = "Amount in lovelace to lock",
              short = "a"
            )
            .withDefault(2000000L) // 2 ADA default

        val utxoArg: Opts[String] = Opts.argument[String](
          metavar = "UTXO"
        )

        val txIndexOpt: Opts[Int] = Opts.option[Int](
          "tx-index",
          help = "Transaction index in block",
          short = "i"
        )

        val proofOpt: Opts[String] = Opts.option[String](
          "proof",
          help = "Merkle proof (comma-separated hex hashes)",
          short = "p"
        )
    }

    /** Main CLI command parser */
    val command: Command[Cmd] = {
        import CliParsers.*

        val lockCommand = Opts.subcommand(
          "lock",
          "Lock funds requiring Bitcoin transaction proof to unlock"
        ) {
            (btcTxIdArg, merkleRootOpt, amountOpt).mapN(Cmd.Lock.apply)
        }

        val unlockCommand = Opts.subcommand(
          "unlock",
          "Unlock funds by providing Merkle proof"
        ) {
            (utxoArg, txIndexOpt, proofOpt).mapN(Cmd.Unlock.apply)
        }

        val infoCommand = Opts.subcommand(
          "info",
          "Show verifier contract information"
        ) {
            Opts(Cmd.Info)
        }

        Command(
          name = "bitcoin-dependent-lock",
          header = "Lock and unlock Cardano funds based on Bitcoin transaction proofs"
        )(lockCommand orElse unlockCommand orElse infoCommand)
    }

    /** Run the CLI application */
    def run(args: Seq[String]): Int = {
        command.parse(args) match {
            case Left(help) =>
                System.err.println(help)
                1
            case Right(cmd) =>
                executeCommand(cmd)
        }
    }

    /** Execute a CLI command */
    def executeCommand(cmd: Cmd): Int = {
        cmd match {
            case Cmd.Lock(btcTxId, merkleRoot, amount) =>
                lockFunds(btcTxId, merkleRoot, amount)
            case Cmd.Unlock(utxo, txIndex, proof) =>
                unlockFunds(utxo, txIndex, proof)
            case Cmd.Info =>
                showInfo()
        }
    }

    /** Get the TransactionVerifier script and address */
    private def getVerifierScript(): com.bloxbean.cardano.client.plutus.spec.PlutusV3Script = {
        val program = TransactionVerifierContract.validator
        val scriptCborHex = program.doubleCborHex
        com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(scriptCborHex)
            .build()
            .asInstanceOf[com.bloxbean.cardano.client.plutus.spec.PlutusV3Script]
    }

    /** Show verifier contract information */
    def showInfo(): Int = {
        println("BitcoinDependentLock - TransactionVerifier Contract Info")
        println()

        val script = getVerifierScript()
        val scriptHash =
            com.bloxbean.cardano.client.util.HexUtil.encodeHexString(script.getScriptHash)

        // Get addresses for different networks
        val mainnetAddr = AddressProvider
            .getEntAddress(script, Networks.mainnet())
            .getAddress
        val testnetAddr = AddressProvider
            .getEntAddress(script, Networks.testnet())
            .getAddress

        println(s"Script Hash: $scriptHash")
        println()
        println("Contract Addresses:")
        println(s"  Mainnet: $mainnetAddr")
        println(s"  Testnet: $testnetAddr")
        println()
        println("Usage:")
        println("  lock <BTC_TX_ID> --merkle-root <ROOT> --amount <LOVELACE>")
        println("  unlock <UTXO> --tx-index <INDEX> --proof <HASH1,HASH2,...>")

        0
    }

    /** Lock funds at the verifier contract */
    def lockFunds(btcTxId: String, merkleRoot: String, amountLovelace: Long): Int = {
        println("Locking funds with Bitcoin transaction requirement...")
        println()
        println(s"  Bitcoin TX: $btcTxId")
        println(s"  Merkle Root: $merkleRoot")
        println(s"  Amount: $amountLovelace lovelace (${amountLovelace / 1000000.0} ADA)")
        println()

        // Load configurations
        val cardanoConfig = CardanoConfig.load()
        val walletConfig = WalletConfig.load()

        (cardanoConfig, walletConfig) match {
            case (Right(cardanoConf), Right(walletConf)) =>
                // Create account
                val account = walletConf.createAccount() match {
                    case Right(acc) =>
                        println(s"✓ Wallet loaded: ${acc.baseAddress()}")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet: $err")
                        return 1
                }

                // Create backend service
                val backendService = cardanoConf.createBackendService() match {
                    case Right(service) =>
                        println(s"✓ Connected to Cardano backend")
                        service
                    case Left(err) =>
                        System.err.println(s"Error creating backend: $err")
                        return 1
                }

                // Get script and address
                val script = getVerifierScript()
                val network =
                    if cardanoConf.network == CardanoNetwork.Mainnet then Networks.mainnet()
                    else Networks.testnet()
                val verifierAddress = AddressProvider
                    .getEntAddress(script, network)
                    .getAddress

                println(s"✓ Verifier address: $verifierAddress")
                println()

                // Create datum
                // Note: txHash must be reversed (Bitcoin display to internal byte order)
                // merkleRoot is already in internal byte order from prove-transaction output
                val txHash = ByteString.fromHex(btcTxId).reverse
                val rootHash = ByteString.fromHex(merkleRoot)

                val datum = TxVerifierDatum(
                  expectedTxHash = txHash,
                  blockMerkleRoot = rootHash
                )
                val datumData = Interop.toPlutusData(datum.toData)

                println("Step 1: Building lock transaction...")

                // Build transaction
                val lockTx = new Tx()
                    .payToContract(
                      verifierAddress,
                      Amount.lovelace(BigInteger.valueOf(amountLovelace)),
                      datumData
                    )
                    .from(account.baseAddress())

                val txBuilder = new QuickTxBuilder(backendService)
                val result = txBuilder
                    .compose(lockTx)
                    .withSigner(SignerProviders.signerFrom(account))
                    .completeAndWait()

                if result.isSuccessful then {
                    val txHash = result.getValue
                    println()
                    println("✓ Funds locked successfully!")
                    println(s"  Transaction: $txHash")
                    println(s"  Locked UTxO: $txHash:0")
                    println()
                    println("To unlock, use:")
                    println(
                      s"  bitcoin-dependent-lock unlock $txHash:0 --tx-index <INDEX> --proof <PROOF>"
                    )
                    0
                } else {
                    System.err.println(s"✗ Lock transaction failed: ${result.getResponse}")
                    1
                }

            case (Left(err), _) =>
                System.err.println(s"Error loading Cardano config: $err")
                1
            case (_, Left(err)) =>
                System.err.println(s"Error loading Wallet config: $err")
                1
        }
    }

    /** Unlock funds by providing Merkle proof */
    def unlockFunds(utxo: String, txIndex: Int, proofStr: String): Int = {
        println("Unlocking funds with Merkle proof...")
        println()
        println(s"  UTxO: $utxo")
        println(s"  TX Index: $txIndex")
        println()

        // Parse UTxO
        val (txHash, outputIndex) = utxo.split(":") match {
            case Array(hash, idx) => (hash, idx.toInt)
            case _ =>
                System.err.println("Invalid UTxO format. Use: <txHash>:<outputIndex>")
                return 1
        }

        // Parse proof
        val proofHashes = proofStr.split(",").map(_.trim).filter(_.nonEmpty)
        if proofHashes.isEmpty then {
            System.err.println("Proof cannot be empty")
            return 1
        }

        println(s"  Proof size: ${proofHashes.length} hashes")
        println()

        // Load configurations
        val cardanoConfig = CardanoConfig.load()
        val walletConfig = WalletConfig.load()

        (cardanoConfig, walletConfig) match {
            case (Right(cardanoConf), Right(walletConf)) =>
                // Create account
                val account = walletConf.createAccount() match {
                    case Right(acc) =>
                        println(s"✓ Wallet loaded: ${acc.baseAddress()}")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet: $err")
                        return 1
                }

                // Create backend service
                val backendService = cardanoConf.createBackendService() match {
                    case Right(service) =>
                        println(s"✓ Connected to Cardano backend")
                        service
                    case Left(err) =>
                        System.err.println(s"Error creating backend: $err")
                        return 1
                }

                // Get script
                val script = getVerifierScript()
                val network =
                    if cardanoConf.network == CardanoNetwork.Mainnet then Networks.mainnet()
                    else Networks.testnet()
                val verifierAddress = AddressProvider
                    .getEntAddress(script, network)
                    .getAddress

                println()
                println("Step 1: Fetching locked UTxO...")

                // Fetch UTxO
                val utxoService = backendService.getUtxoService
                val utxos =
                    try {
                        utxoService.getUtxos(verifierAddress, 100, 1)
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
                val lockedUtxo =
                    allUtxos.find(u => u.getTxHash == txHash && u.getOutputIndex == outputIndex)

                lockedUtxo match {
                    case None =>
                        System.err.println(s"✗ UTxO not found: $utxo")
                        System.err.println("Available UTxOs at verifier address:")
                        allUtxos.foreach(u =>
                            System.err.println(s"  ${u.getTxHash}:${u.getOutputIndex}")
                        )
                        return 1

                    case Some(utxoToSpend) =>
                        println(s"✓ Found locked UTxO")

                        // Create redeemer with proof
                        // Note: proof hashes are already in internal byte order from prove-transaction output
                        val proofList = proofHashes.map(h => ByteString.fromHex(h)).toList
                        val redeemerValue = TxVerifierRedeemer(
                          txIndex = BigInt(txIndex),
                          merkleProof = scalus.prelude.List.from(proofList)
                        )
                        val redeemerData = Interop.toPlutusData(redeemerValue.toData)

                        println()
                        println("Step 2: Building unlock transaction...")

                        // Setup ScalusTransactionEvaluator for proper script cost estimation
                        val protocolParamsResult =
                            backendService.getEpochService.getProtocolParameters
                        if !protocolParamsResult.isSuccessful then {
                            System.err.println(
                              s"✗ Error fetching protocol params: ${protocolParamsResult.getResponse}"
                            )
                            return 1
                        }
                        val protocolParams = protocolParamsResult.getValue

                        val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
                        val scriptHashHex = scalus.utils.Hex.bytesToHex(script.getScriptHash)
                        val scriptSupplier = new scalus.bloxbean.ScriptSupplier {
                            override def getScript(
                                hash: String
                            ): com.bloxbean.cardano.client.plutus.spec.PlutusScript =
                                if hash == scriptHashHex then script
                                else throw new RuntimeException(s"Unknown script hash: $hash")
                        }

                        // Retrieve slot config from the network
                        val slotConfig =
                            binocular.util.SlotConfigHelper.retrieveSlotConfig(backendService)

                        val scalusEvaluator = new scalus.bloxbean.ScalusTransactionEvaluator(
                          slotConfig,
                          protocolParams,
                          utxoSupplier,
                          scriptSupplier
                        )

                        // Build unlock transaction
                        val unlockTx = new ScriptTx()
                            .collectFrom(utxoToSpend, redeemerData)
                            .payToAddress(account.baseAddress(), utxoToSpend.getAmount)
                            .attachSpendingValidator(script)
                            .withChangeAddress(account.baseAddress())

                        val txBuilder = new QuickTxBuilder(backendService)
                        val result = txBuilder
                            .compose(unlockTx)
                            .feePayer(account.baseAddress())
                            .withSigner(SignerProviders.signerFrom(account))
                            .withTxEvaluator(scalusEvaluator)
                            .completeAndWait()

                        if result.isSuccessful then {
                            val resultTxHash = result.getValue
                            println()
                            println("✓ Funds unlocked successfully!")
                            println(s"  Transaction: $resultTxHash")
                            println()
                            println("The Merkle proof was verified on-chain, proving the Bitcoin")
                            println("transaction exists in the specified block.")
                            0
                        } else {
                            System.err.println()
                            System.err.println(
                              s"✗ Unlock transaction failed: ${result.getResponse}"
                            )
                            System.err.println()
                            System.err.println("This usually means the Merkle proof is invalid.")
                            System.err.println("Verify that:")
                            System.err.println(
                              "  - tx-index matches the transaction's position in the block"
                            )
                            System.err.println(
                              "  - proof contains correct sibling hashes from leaf to root"
                            )
                            System.err.println(
                              "  - the datum's merkle-root matches the block's merkle root"
                            )
                            1
                        }
                }

            case (Left(err), _) =>
                System.err.println(s"Error loading Cardano config: $err")
                1
            case (_, Left(err)) =>
                System.err.println(s"Error loading Wallet config: $err")
                1
        }
    }
}
