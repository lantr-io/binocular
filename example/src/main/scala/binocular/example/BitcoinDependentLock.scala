package binocular.example

import binocular.{reverse, BitcoinContract, BitcoinNodeConfig, BlockHeader, CardanoConfig, CardanoNetwork, MerkleTree, OracleConfig, SimpleBitcoinRpc, TransactionVerifierContract, TxVerifierDatum, TxVerifierRedeemer, WalletConfig}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Script, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.{BlockchainProvider, UtxoSource}
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{fromData, toData}
import com.monovore.decline.*
import cats.implicits.*

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** BitcoinDependentLock - Lock and unlock funds based on Bitcoin transaction proofs
  *
  * This application demonstrates using the Binocular oracle to verify Bitcoin transactions on
  * Cardano. Funds can be locked requiring a specific Bitcoin transaction to exist in a block
  * confirmed by the Oracle, and unlocked by providing valid Merkle proofs for both the transaction
  * and block inclusion.
  */
@main def bitcoinDependentLock(args: String*): Unit = {
    val exitCode = BitcoinDependentLockApp.run(args)
    System.exit(exitCode)
}

object BitcoinDependentLockApp {

    enum Cmd:
        case Lock(btcTxId: String, blockHash: String, amountLovelace: Long)
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

    object CliParsers {
        val btcTxIdArg: Opts[String] = Opts.argument[String](metavar = "BTC_TX_ID")
        val blockHashOpt: Opts[String] = Opts.option[String](
          "block-hash",
          help = "Bitcoin block hash containing the transaction (hex, display order)",
          short = "b"
        )
        val amountOpt: Opts[Long] = Opts
            .option[Long]("amount", help = "Amount in lovelace to lock", short = "a")
            .withDefault(2000000L)
        val utxoArg: Opts[String] = Opts.argument[String](metavar = "UTXO")
        val txIndexOpt: Opts[Int] =
            Opts.option[Int]("tx-index", help = "Transaction index in block (0-based)", short = "i")
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

    val command: Command[Cmd] = {
        import CliParsers.*

        val lockCommand =
            Opts.subcommand("lock", "Lock funds requiring Bitcoin transaction proof") {
                (btcTxIdArg, blockHashOpt, amountOpt).mapN(Cmd.Lock.apply)
            }

        val unlockCommand = Opts.subcommand("unlock", "Unlock funds by providing Merkle proofs") {
            (
              utxoArg,
              txIndexOpt,
              txProofOpt,
              blockIndexOpt,
              blockProofOpt,
              blockHeaderOpt,
              oracleUtxoOpt
            )
                .mapN(Cmd.Unlock.apply)
        }

        val infoCommand = Opts.subcommand("info", "Show verifier contract and Oracle information") {
            Opts(Cmd.Info)
        }

        Command(
          name = "bitcoin-dependent-lock",
          header =
              "Lock and unlock Cardano funds based on Bitcoin transaction proofs verified against the Binocular Oracle"
        )(lockCommand orElse unlockCommand orElse infoCommand)
    }

    def run(args: Seq[String]): Int = {
        command.parse(args) match {
            case Left(help) =>
                System.err.println(help)
                1
            case Right(cmd) => executeCommand(cmd)
        }
    }

    def executeCommand(cmd: Cmd): Int = cmd match {
        case Cmd.Lock(btcTxId, blockHash, amount) => lockFunds(btcTxId, blockHash, amount)
        case Cmd.Unlock(utxo, txIndex, txProof, blockIndex, blockProof, blockHeader, oracleUtxo) =>
            unlockFunds(utxo, txIndex, txProof, blockIndex, blockProof, blockHeader, oracleUtxo)
        case Cmd.Info => showInfo()
    }

    /** Get the TransactionVerifier as a PlutusV3 script */
    private def getVerifierScript(): Script.PlutusV3 = {
        val program = TransactionVerifierContract.validator
        Script.PlutusV3(program.cborByteString)
    }

    /** Get the verifier script address for a given network */
    private def getVerifierAddress(network: Network): Address = {
        val script = getVerifierScript()
        Address(network, Credential.ScriptHash(script.scriptHash))
    }

    /** Get the Oracle script hash */
    private def getOracleScriptHash(): ByteString = {
        sys.env.get("ORACLE_SCRIPT_HASH").filter(_.nonEmpty) match {
            case Some(hash) => ByteString.fromHex(hash)
            case None =>
                sys.env.get("ORACLE_SCRIPT_ADDRESS").filter(_.nonEmpty) match {
                    case Some(addr) =>
                        val address = Address.fromBech32(addr)
                        address match {
                            case shelley: scalus.cardano.address.ShelleyAddress =>
                                shelley.payment match {
                                    case scalus.cardano.address.ShelleyPaymentPart.Script(hash) =>
                                        hash
                                    case _ => computeOracleScriptHashFromScript()
                                }
                            case _ => computeOracleScriptHashFromScript()
                        }
                    case None => computeOracleScriptHashFromScript()
                }
        }
    }

    private def computeOracleScriptHashFromScript(): ByteString = {
        OracleConfig.getScriptHash()
    }

    def showInfo(): Int = {
        println("BitcoinDependentLock - TransactionVerifier Contract Info")
        println()

        val script = getVerifierScript()
        val scriptHash = script.scriptHash.toHex
        val oracleScriptHash = getOracleScriptHash()
        val oracleScriptHashHex = oracleScriptHash.toHex

        val mainnetAddr = getVerifierAddress(Network.Mainnet)
            .asInstanceOf[scalus.cardano.address.ShelleyAddress]
            .toBech32
            .get
        val testnetAddr = getVerifierAddress(Network.Testnet)
            .asInstanceOf[scalus.cardano.address.ShelleyAddress]
            .toBech32
            .get

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

    def lockFunds(btcTxId: String, blockHash: String, amountLovelace: Long): Int = {
        println("Locking funds with Bitcoin transaction requirement...")
        println()
        println(s"  Bitcoin TX:   $btcTxId")
        println(s"  Block Hash:   $blockHash")
        println(s"  Amount:       $amountLovelace lovelace (${amountLovelace / 1000000.0} ADA)")
        println()

        val cardanoConfig = CardanoConfig.load()
        val walletConfig = WalletConfig.load()

        (cardanoConfig, walletConfig) match {
            case (Right(cardanoConf), Right(walletConf)) =>
                given ec: ExecutionContext = ExecutionContext.global

                val hdAccount = walletConf.createHdAccount() match {
                    case Right(acc) =>
                        val addr = acc
                            .baseAddress(cardanoConf.scalusNetwork)
                            .asInstanceOf[scalus.cardano.address.ShelleyAddress]
                            .toBech32
                            .get
                        println(s"Wallet loaded: $addr")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet: $err")
                        return 1
                }
                val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
                val sponsorAddress = hdAccount.baseAddress(cardanoConf.scalusNetwork)

                val provider = cardanoConf.createBlockchainProvider() match {
                    case Right(p) =>
                        println(s"Connected to Cardano backend")
                        p
                    case Left(err) =>
                        System.err.println(s"Error creating backend: $err")
                        return 1
                }

                val verifierAddress = getVerifierAddress(cardanoConf.scalusNetwork)
                val verifierAddressBech32 = verifierAddress
                    .asInstanceOf[scalus.cardano.address.ShelleyAddress]
                    .toBech32
                    .get

                val oracleScriptHash = getOracleScriptHash()

                println(s"Verifier address: $verifierAddressBech32")
                println(s"Oracle script hash: ${oracleScriptHash.toHex}")
                println()

                // Create datum
                val txHash = ByteString.fromHex(btcTxId).reverse
                val blockHashBytes = ByteString.fromHex(blockHash).reverse

                val datum = TxVerifierDatum(
                  expectedTxHash = txHash,
                  expectedBlockHash = blockHashBytes,
                  oracleScriptHash = oracleScriptHash
                )

                println("Building lock transaction...")

                try {
                    val tx = Await
                        .result(
                          TxBuilder(provider.cardanoInfo)
                              .payTo(verifierAddress, Value.lovelace(amountLovelace), datum)
                              .complete(provider, sponsorAddress),
                          60.seconds
                        )
                        .sign(signer)
                        .transaction

                    val result = Await.result(provider.submit(tx), 30.seconds)
                    result match {
                        case Right(resultTxHash) =>
                            val hash = resultTxHash.toHex
                            println()
                            println("Funds locked successfully!")
                            println(s"  Transaction: $hash")
                            println(s"  Locked UTxO: $hash:0")
                            0
                        case Left(err) =>
                            System.err.println(s"Lock transaction failed: $err")
                            1
                    }
                } catch {
                    case e: Exception =>
                        System.err.println(s"Error building transaction: ${e.getMessage}")
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

        val (txHash, outputIndex) = utxo.split(":") match {
            case Array(hash, idx) => (hash, idx.toInt)
            case _ =>
                System.err.println("Invalid UTxO format. Use: <txHash>:<outputIndex>")
                return 1
        }

        val (oracleTxHash, oracleOutputIndex) = oracleUtxoStr.split(":") match {
            case Array(hash, idx) => (hash, idx.toInt)
            case _ =>
                System.err.println("Invalid Oracle UTxO format. Use: <txHash>:<outputIndex>")
                return 1
        }

        val txProofHashes = txProofStr.split(",").map(_.trim).filter(_.nonEmpty)
        println(s"  TX Proof size: ${txProofHashes.length} hashes")

        val blockProofHashes = blockProofStr.split(",").map(_.trim).filter(_.nonEmpty)
        println(s"  Block Proof size: ${blockProofHashes.length} hashes")

        if blockHeaderHex.length != 160 then {
            System.err.println(
              s"Block header must be 80 bytes (160 hex chars), got ${blockHeaderHex.length / 2} bytes"
            )
            return 1
        }
        println()

        val cardanoConfig = CardanoConfig.load()
        val walletConfig = WalletConfig.load()

        (cardanoConfig, walletConfig) match {
            case (Right(cardanoConf), Right(walletConf)) =>
                given ec: ExecutionContext = ExecutionContext.global

                val hdAccount = walletConf.createHdAccount() match {
                    case Right(acc) =>
                        val addr = acc
                            .baseAddress(cardanoConf.scalusNetwork)
                            .asInstanceOf[scalus.cardano.address.ShelleyAddress]
                            .toBech32
                            .get
                        println(s"Wallet loaded: $addr")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet: $err")
                        return 1
                }
                val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
                val sponsorAddress = hdAccount.baseAddress(cardanoConf.scalusNetwork)

                val provider = cardanoConf.createBlockchainProvider() match {
                    case Right(p) =>
                        println(s"Connected to Cardano backend")
                        p
                    case Left(err) =>
                        System.err.println(s"Error creating backend: $err")
                        return 1
                }

                val script = getVerifierScript()
                val verifierAddress = getVerifierAddress(cardanoConf.scalusNetwork)

                println()
                println("Fetching locked UTxO...")

                val utxosResult = Await.result(provider.findUtxos(verifierAddress), 30.seconds)
                val allUtxos: List[Utxo] = utxosResult match {
                    case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
                    case Left(err) =>
                        System.err.println(s"Error fetching UTxOs: $err")
                        return 1
                }

                val lockedUtxo = allUtxos.find(u =>
                    u.input.transactionId.toHex == txHash && u.input.index == outputIndex
                )

                lockedUtxo match {
                    case None =>
                        System.err.println(s"UTxO not found: $utxo")
                        return 1

                    case Some(utxoToSpend) =>
                        println(s"Found locked UTxO")

                        // Create redeemer
                        val txProofList = txProofHashes.map(h => ByteString.fromHex(h)).toList
                        val blockProofList = blockProofHashes.map(h => ByteString.fromHex(h)).toList
                        val blockHeader = BlockHeader(ByteString.fromHex(blockHeaderHex))

                        val redeemerValue = TxVerifierRedeemer(
                          txIndex = BigInt(txIndex),
                          txMerkleProof =
                              scalus.cardano.onchain.plutus.prelude.List.from(txProofList),
                          blockIndex = BigInt(blockIndex),
                          blockMerkleProof =
                              scalus.cardano.onchain.plutus.prelude.List.from(blockProofList),
                          blockHeader = blockHeader
                        )

                        println()
                        println("Building unlock transaction with Oracle reference...")

                        // Create Oracle reference input UTxO
                        val oracleInput = TransactionInput(
                          TransactionHash.fromHex(oracleTxHash),
                          oracleOutputIndex
                        )
                        val oracleUtxoResult =
                            Await.result(provider.findUtxo(oracleInput), 30.seconds)
                        val oracleUtxo: Utxo = oracleUtxoResult match {
                            case Right(u) => u
                            case Left(_) =>
                                System.err.println(s"Oracle UTxO not found: $oracleUtxoStr")
                                return 1
                        }

                        try {
                            val tx = Await
                                .result(
                                  TxBuilder(provider.cardanoInfo)
                                      .references(oracleUtxo)
                                      .spend(utxoToSpend, redeemerValue, script)
                                      .payTo(sponsorAddress, utxoToSpend.output.value)
                                      .complete(provider, sponsorAddress),
                                  60.seconds
                                )
                                .sign(signer)
                                .transaction

                            val result = Await.result(provider.submit(tx), 30.seconds)
                            result match {
                                case Right(resultTxHash) =>
                                    val hash = resultTxHash.toHex
                                    println()
                                    println("Funds unlocked successfully!")
                                    println(s"  Transaction: $hash")
                                    println()
                                    println("The on-chain validator verified:")
                                    println("  1. The block is confirmed in the Binocular Oracle")
                                    println(
                                      "  2. The block header hashes to the expected block hash"
                                    )
                                    println("  3. The transaction is included in the block")
                                    0
                                case Left(err) =>
                                    System.err.println()
                                    System.err.println(s"Unlock transaction failed: $err")
                                    1
                            }
                        } catch {
                            case e: Exception =>
                                System.err.println(s"Error building transaction: ${e.getMessage}")
                                e.printStackTrace()
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
