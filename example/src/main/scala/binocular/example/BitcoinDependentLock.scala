package binocular.example

import binocular.{reverse, BitcoinContract, BitcoinNodeConfig, BlockHeader, CardanoConfig, CardanoNetwork, MerkleTree, OracleConfig, SimpleBitcoinRpc, TransactionVerifierContract, TxVerifierDatum, TxVerifierRedeemer, WalletConfig}
import com.bloxbean.cardano.client.address.{Address, AddressProvider}
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.transaction.spec.TransactionInput
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
  * Cardano. Funds can be locked requiring a specific Bitcoin transaction to exist in a block
  * confirmed by the Oracle, and unlocked by providing valid Merkle proofs for both the transaction
  * and block inclusion.
  *
  * SECURITY: This validator requires:
  *   1. The Oracle UTxO as a reference input
  *   2. Proof that the block is in the Oracle's confirmed blocks tree
  *   3. Proof that the transaction is in the block
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
            blockHash: String,
            amountLovelace: Long
        )
        case Unlock(
            utxo: String,
            txIndex: Int,
            txProof: String,
            blockIndex: Int,
            blockProof: String,
            blockHeader: String,
            oracleUtxo: String
        )
        case Info

    /** CLI argument parsers */
    object CliParsers {

        val btcTxIdArg: Opts[String] = Opts.argument[String](
          metavar = "BTC_TX_ID"
        )

        val blockHashOpt: Opts[String] = Opts.option[String](
          "block-hash",
          help = "Bitcoin block hash containing the transaction (hex, display order)",
          short = "b"
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
          help = "Transaction index in block (0-based)",
          short = "i"
        )

        val txProofOpt: Opts[String] = Opts.option[String](
          "tx-proof",
          help = "Transaction Merkle proof (comma-separated hex hashes)",
          short = "t"
        )

        val blockIndexOpt: Opts[Int] = Opts.option[Int](
          "block-index",
          help = "Block index in Oracle's confirmed tree (0-based)",
          short = "n"
        )

        val blockProofOpt: Opts[String] = Opts.option[String](
          "block-proof",
          help = "Block Merkle proof (comma-separated hex hashes)",
          short = "p"
        )

        val blockHeaderOpt: Opts[String] = Opts.option[String](
          "block-header",
          help = "80-byte Bitcoin block header (hex)",
          short = "h"
        )

        val oracleUtxoOpt: Opts[String] = Opts.option[String](
          "oracle-utxo",
          help = "Oracle UTxO reference (txHash:index)",
          short = "o"
        )
    }

    /** Main CLI command parser */
    val command: Command[Cmd] = {
        import CliParsers.*

        val lockCommand = Opts.subcommand(
          "lock",
          "Lock funds requiring Bitcoin transaction proof (with Oracle verification) to unlock"
        ) {
            (btcTxIdArg, blockHashOpt, amountOpt).mapN(Cmd.Lock.apply)
        }

        val unlockCommand = Opts.subcommand(
          "unlock",
          "Unlock funds by providing Merkle proofs (requires Oracle reference)"
        ) {
            (utxoArg, txIndexOpt, txProofOpt, blockIndexOpt, blockProofOpt, blockHeaderOpt, oracleUtxoOpt)
                .mapN(Cmd.Unlock.apply)
        }

        val infoCommand = Opts.subcommand(
          "info",
          "Show verifier contract and Oracle information"
        ) {
            Opts(Cmd.Info)
        }

        Command(
          name = "bitcoin-dependent-lock",
          header = "Lock and unlock Cardano funds based on Bitcoin transaction proofs verified against the Binocular Oracle"
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
            case Cmd.Lock(btcTxId, blockHash, amount) =>
                lockFunds(btcTxId, blockHash, amount)
            case Cmd.Unlock(utxo, txIndex, txProof, blockIndex, blockProof, blockHeader, oracleUtxo) =>
                unlockFunds(utxo, txIndex, txProof, blockIndex, blockProof, blockHeader, oracleUtxo)
            case Cmd.Info =>
                showInfo()
        }
    }

    /** Get the TransactionVerifier script */
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

    /** Get the Oracle (BitcoinValidator) script hash.
      * Can be overridden via ORACLE_SCRIPT_HASH env var, or derived from ORACLE_SCRIPT_ADDRESS.
      */
    private def getOracleScriptHash(): ByteString = {
        // First check for explicit script hash
        sys.env.get("ORACLE_SCRIPT_HASH").filter(_.nonEmpty) match {
            case Some(hash) => ByteString.fromHex(hash)
            case None =>
                // Try to derive from script address
                sys.env.get("ORACLE_SCRIPT_ADDRESS").filter(_.nonEmpty) match {
                    case Some(addr) =>
                        // Extract script hash from bech32 address
                        val address = new Address(addr)
                        val credential = address.getPaymentCredentialHash.orElse(null)
                        if credential != null then ByteString.fromArray(credential)
                        else computeOracleScriptHashFromScript()
                    case None => computeOracleScriptHashFromScript()
                }
        }
    }

    private def computeOracleScriptHashFromScript(): ByteString = {
        val scriptCborHex = OracleConfig.getScriptCborHex()
        val plutusScript = com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(scriptCborHex)
            .build()
            .asInstanceOf[com.bloxbean.cardano.client.plutus.spec.PlutusV3Script]
        ByteString.fromArray(plutusScript.getScriptHash)
    }

    /** Show verifier contract information */
    def showInfo(): Int = {
        println("BitcoinDependentLock - TransactionVerifier Contract Info")
        println()

        val script = getVerifierScript()
        val scriptHash =
            com.bloxbean.cardano.client.util.HexUtil.encodeHexString(script.getScriptHash)

        val oracleScriptHash = getOracleScriptHash()
        val oracleScriptHashHex = scalus.utils.Hex.bytesToHex(oracleScriptHash.bytes)

        // Get addresses for different networks
        val mainnetAddr = AddressProvider
            .getEntAddress(script, Networks.mainnet())
            .getAddress
        val testnetAddr = AddressProvider
            .getEntAddress(script, Networks.testnet())
            .getAddress

        println(s"Verifier Script Hash: $scriptHash")
        println(s"Oracle Script Hash:   $oracleScriptHashHex")
        println()
        println("Contract Addresses:")
        println(s"  Mainnet: $mainnetAddr")
        println(s"  Testnet: $testnetAddr")
        println()
        println("This validator requires the Binocular Oracle as a reference input.")
        println("The block containing the transaction must be confirmed in the Oracle.")
        println()
        println("Usage:")
        println("  lock <BTC_TX_ID> --block-hash <HASH> --amount <LOVELACE>")
        println("  unlock <UTXO> --tx-index <N> --tx-proof <PROOF> \\")
        println("         --block-index <N> --block-proof <PROOF> \\")
        println("         --block-header <HEX> --oracle-utxo <TXHASH:INDEX>")

        0
    }

    /** Lock funds at the verifier contract */
    def lockFunds(btcTxId: String, blockHash: String, amountLovelace: Long): Int = {
        println("Locking funds with Bitcoin transaction requirement...")
        println()
        println(s"  Bitcoin TX:   $btcTxId")
        println(s"  Block Hash:   $blockHash")
        println(s"  Amount:       $amountLovelace lovelace (${amountLovelace / 1000000.0} ADA)")
        println()

        // Load configurations
        val cardanoConfig = CardanoConfig.load()
        val walletConfig = WalletConfig.load()

        (cardanoConfig, walletConfig) match {
            case (Right(cardanoConf), Right(walletConf)) =>
                // Create account
                val account = walletConf.createAccount() match {
                    case Right(acc) =>
                        println(s"Wallet loaded: ${acc.baseAddress()}")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet: $err")
                        return 1
                }

                // Create backend service
                val backendService = cardanoConf.createBackendService() match {
                    case Right(service) =>
                        println(s"Connected to Cardano backend")
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

                // Get oracle script hash
                val oracleScriptHash = getOracleScriptHash()

                println(s"Verifier address: $verifierAddress")
                println(s"Oracle script hash: ${scalus.utils.Hex.bytesToHex(oracleScriptHash.bytes)}")
                println()

                // Create datum
                // Note: Both txHash and blockHash must be reversed (Bitcoin display to internal byte order)
                val txHash = ByteString.fromHex(btcTxId).reverse
                val blockHashBytes = ByteString.fromHex(blockHash).reverse

                val datum = TxVerifierDatum(
                  expectedTxHash = txHash,
                  expectedBlockHash = blockHashBytes,
                  oracleScriptHash = oracleScriptHash
                )
                val datumData = Interop.toPlutusData(datum.toData)

                println("Building lock transaction...")

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
                    val resultTxHash = result.getValue
                    println()
                    println("Funds locked successfully!")
                    println(s"  Transaction: $resultTxHash")
                    println(s"  Locked UTxO: $resultTxHash:0")
                    0
                } else {
                    System.err.println(s"Lock transaction failed: ${result.getResponse}")
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

    /** Unlock funds by providing Merkle proofs with Oracle verification */
    def unlockFunds(
        utxo: String,
        txIndex: Int,
        txProofStr: String,
        blockIndex: Int,
        blockProofStr: String,
        blockHeaderHex: String,
        oracleUtxoStr: String
    ): Int = {
        println("Unlocking funds with Oracle-verified Merkle proofs...")
        println()
        println(s"  UTxO:         $utxo")
        println(s"  TX Index:     $txIndex")
        println(s"  Block Index:  $blockIndex")
        println(s"  Oracle UTxO:  $oracleUtxoStr")
        println()

        // Parse UTxO
        val (txHash, outputIndex) = utxo.split(":") match {
            case Array(hash, idx) => (hash, idx.toInt)
            case _ =>
                System.err.println("Invalid UTxO format. Use: <txHash>:<outputIndex>")
                return 1
        }

        // Parse Oracle UTxO
        val (oracleTxHash, oracleOutputIndex) = oracleUtxoStr.split(":") match {
            case Array(hash, idx) => (hash, idx.toInt)
            case _ =>
                System.err.println("Invalid Oracle UTxO format. Use: <txHash>:<outputIndex>")
                return 1
        }

        // Parse tx proof
        val txProofHashes = txProofStr.split(",").map(_.trim).filter(_.nonEmpty)
        println(s"  TX Proof size: ${txProofHashes.length} hashes")

        // Parse block proof
        val blockProofHashes = blockProofStr.split(",").map(_.trim).filter(_.nonEmpty)
        println(s"  Block Proof size: ${blockProofHashes.length} hashes")

        // Validate block header
        if blockHeaderHex.length != 160 then {
            System.err.println(s"Block header must be 80 bytes (160 hex chars), got ${blockHeaderHex.length / 2} bytes")
            return 1
        }
        println()

        // Load configurations
        val cardanoConfig = CardanoConfig.load()
        val walletConfig = WalletConfig.load()

        (cardanoConfig, walletConfig) match {
            case (Right(cardanoConf), Right(walletConf)) =>
                // Create account
                val account = walletConf.createAccount() match {
                    case Right(acc) =>
                        println(s"Wallet loaded: ${acc.baseAddress()}")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet: $err")
                        return 1
                }

                // Create backend service
                val backendService = cardanoConf.createBackendService() match {
                    case Right(service) =>
                        println(s"Connected to Cardano backend")
                        service
                    case Left(err) =>
                        System.err.println(s"Error creating backend: $err")
                        return 1
                }

                // Get scripts
                val script = getVerifierScript()
                val network =
                    if cardanoConf.network == CardanoNetwork.Mainnet then Networks.mainnet()
                    else Networks.testnet()
                val verifierAddress = AddressProvider
                    .getEntAddress(script, network)
                    .getAddress

                println()
                println("Fetching locked UTxO...")

                // Fetch verifier UTxOs
                val utxoService = backendService.getUtxoService
                val utxos =
                    try {
                        utxoService.getUtxos(verifierAddress, 100, 1)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"Error fetching UTxOs: ${e.getMessage}")
                            return 1
                    }

                if !utxos.isSuccessful then {
                    System.err.println(s"Error fetching UTxOs: ${utxos.getResponse}")
                    return 1
                }

                val allUtxos = utxos.getValue.asScala.toList
                val lockedUtxo =
                    allUtxos.find(u => u.getTxHash == txHash && u.getOutputIndex == outputIndex)

                lockedUtxo match {
                    case None =>
                        System.err.println(s"UTxO not found: $utxo")
                        return 1

                    case Some(utxoToSpend) =>
                        println(s"Found locked UTxO")

                        // Create redeemer with both proofs
                        val txProofList = txProofHashes.map(h => ByteString.fromHex(h)).toList
                        val blockProofList = blockProofHashes.map(h => ByteString.fromHex(h)).toList
                        val blockHeader = BlockHeader(ByteString.fromHex(blockHeaderHex))

                        val redeemerValue = TxVerifierRedeemer(
                          txIndex = BigInt(txIndex),
                          txMerkleProof = scalus.prelude.List.from(txProofList),
                          blockIndex = BigInt(blockIndex),
                          blockMerkleProof = scalus.prelude.List.from(blockProofList),
                          blockHeader = blockHeader
                        )
                        val redeemerData = Interop.toPlutusData(redeemerValue.toData)

                        println()
                        println("Building unlock transaction with Oracle reference...")

                        // Setup ScalusTransactionEvaluator
                        val protocolParamsResult =
                            backendService.getEpochService.getProtocolParameters
                        if !protocolParamsResult.isSuccessful then {
                            System.err.println(
                              s"Error fetching protocol params: ${protocolParamsResult.getResponse}"
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

                        val slotConfig =
                            binocular.util.SlotConfigHelper.retrieveSlotConfig(backendService)

                        val scalusEvaluator = new scalus.bloxbean.ScalusTransactionEvaluator(
                          slotConfig,
                          protocolParams,
                          utxoSupplier,
                          scriptSupplier
                        )

                        // Create Oracle reference input
                        val oracleRefInput = new TransactionInput(oracleTxHash, oracleOutputIndex)

                        // Build unlock transaction with Oracle as reference input
                        val unlockTx = new ScriptTx()
                            .collectFrom(utxoToSpend, redeemerData)
                            .readFrom(oracleRefInput)
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
                            println("Funds unlocked successfully!")
                            println(s"  Transaction: $resultTxHash")
                            println()
                            println("The on-chain validator verified:")
                            println("  1. The block is confirmed in the Binocular Oracle")
                            println("  2. The block header hashes to the expected block hash")
                            println("  3. The transaction is included in the block")
                            0
                        } else {
                            System.err.println()
                            System.err.println(
                              s"Unlock transaction failed: ${result.getResponse}"
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
