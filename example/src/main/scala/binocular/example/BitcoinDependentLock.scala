package binocular.example

import binocular.{reverse, BinocularConfig, BitcoinContract, BlockHeader, ChainState, MerkleTree, SimpleBitcoinRpc, TransactionVerifierContract, TxVerifierDatum, TxVerifierRedeemer}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Credential, Script, Utxo, Value}
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.utils.Hex.hexToBytes
import com.monovore.decline.*
import cats.implicits.*

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scalus.utils.await

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
        case Unlock(utxo: String)
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
    }

    val command: Command[Cmd] = {
        import CliParsers.*

        val lockCommand =
            Opts.subcommand("lock", "Lock funds requiring Bitcoin transaction proof") {
                (btcTxIdArg, blockHashOpt, amountOpt).mapN(Cmd.Lock.apply)
            }

        val unlockCommand = Opts.subcommand(
          "unlock",
          "Unlock funds by providing Oracle-verified proofs (derives proofs from Bitcoin RPC)"
        ) {
            utxoArg.map(Cmd.Unlock.apply)
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
        case Cmd.Unlock(utxo)                     => unlockFunds(utxo)
        case Cmd.Info                             => showInfo()
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
        val configOpt = try { Some(BinocularConfig.load()) }
        catch { case _: Exception => None }
        configOpt match {
            case Some(config) =>
                config.oracle.toBitcoinValidatorParams() match {
                    case Right(params) =>
                        BitcoinContract.makeContract(params).script.scriptHash
                    case Left(_) =>
                        computeDummyOracleScriptHash()
                }
            case None =>
                computeDummyOracleScriptHash()
        }
    }

    private def computeDummyOracleScriptHash(): ByteString = {
        import scalus.cardano.onchain.plutus.v1.PubKeyHash
        import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
        import scalus.uplc.builtin.ByteString as BS
        val dummyTxOutRef = TxOutRef(TxId(BS.fill(32, 0)), BigInt(0))
        val dummyOwner = PubKeyHash(BS.fill(28, 0))
        BitcoinContract
            .makeContract(BitcoinContract.validatorParams(dummyTxOutRef, dummyOwner))
            .script
            .scriptHash
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
        println("  unlock <UTXO>")

        0
    }

    def lockFunds(btcTxId: String, blockHash: String, amountLovelace: Long): Int = {
        println("Locking funds with Bitcoin transaction requirement...")
        println()
        println(s"  Bitcoin TX:   $btcTxId")
        println(s"  Block Hash:   $blockHash")
        println(s"  Amount:       $amountLovelace lovelace (${amountLovelace / 1000000.0} ADA)")
        println()

        val config =
            try { BinocularConfig.load() }
            catch {
                case e: Exception =>
                    System.err.println(s"Error loading config: ${e.getMessage}")
                    return 1
            }
        val cardanoConf = config.cardano
        val walletConf = config.wallet

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
            val tx = TxBuilder(provider.cardanoInfo)
                .payTo(verifierAddress, Value.lovelace(amountLovelace), datum)
                .complete(provider, sponsorAddress)
                .await(60.seconds)
                .sign(signer)
                .transaction

            val result = provider.submit(tx).await(30.seconds)
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
    }

    def unlockFunds(utxo: String): Int = {
        println("Unlocking funds with Oracle-verified proofs...")
        println()
        println(s"  UTxO: $utxo")
        println()

        val (txHash, outputIndex) = utxo.split(":") match {
            case Array(hash, idx) => (hash, idx.toInt)
            case _ =>
                System.err.println("Invalid UTxO format. Use: <txHash>:<outputIndex>")
                return 1
        }

        val config =
            try { BinocularConfig.load() }
            catch {
                case e: Exception =>
                    System.err.println(s"Error loading config: ${e.getMessage}")
                    return 1
            }
        val btcConf = config.bitcoinNode
        val cardanoConf = config.cardano
        val oracleConf = config.oracle
        val walletConf = config.wallet

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

        // Step 1: Fetch locked UTxO and parse datum
        println("Step 1: Fetching locked UTxO...")

        val utxosResult = provider.findUtxos(verifierAddress).await(30.seconds)
        val allUtxos: List[Utxo] = utxosResult match {
            case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
            case Left(err) =>
                System.err.println(s"Error fetching UTxOs: $err")
                return 1
        }

        val utxoToSpend = allUtxos.find(u =>
            u.input.transactionId.toHex == txHash && u.input.index == outputIndex
        ) match {
            case Some(u) =>
                println(s"  Found locked UTxO")
                u
            case None =>
                System.err.println(s"UTxO not found: $utxo")
                return 1
        }

        val datum =
            try {
                utxoToSpend.output.requireInlineDatum.to[TxVerifierDatum]
            } catch {
                case e: Exception =>
                    System.err.println(s"Error parsing TxVerifierDatum: ${e.getMessage}")
                    return 1
            }

        println(s"  Expected TX hash:    ${datum.expectedTxHash.reverse.toHex}")
        println(s"  Expected block hash: ${datum.expectedBlockHash.reverse.toHex}")

        // Step 2: Find oracle UTxO by NFT at oracle script address
        println()
        println("Step 2: Finding oracle UTxO by NFT...")

        val params = oracleConf.toBitcoinValidatorParams() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error parsing oracle params: $err")
                return 1
        }
        val oracleScriptHash =
            BitcoinContract.makeContract(params).script.scriptHash
        val oracleScriptAddressBech32 = oracleConf.scriptAddress(cardanoConf.cardanoNetwork) match {
            case Right(addr) => addr
            case Left(err) =>
                System.err.println(s"Error deriving oracle script address: $err")
                return 1
        }
        val oracleAddress = Address.fromBech32(oracleScriptAddressBech32)

        val oracleUtxosResult =
            provider.findUtxos(oracleAddress).await(30.seconds)
        val oracleUtxos: List[Utxo] = oracleUtxosResult match {
            case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
            case Left(err) =>
                System.err.println(s"Error fetching oracle UTxOs: $err")
                return 1
        }

        val oracleUtxo: Utxo = oracleUtxos.find(u =>
            u.output.value.hasAsset(oracleScriptHash, AssetName.empty)
        ) match {
            case Some(u) =>
                val ref =
                    s"${u.input.transactionId.toHex}:${u.input.index}"
                println(s"  Found oracle UTxO: $ref")
                u
            case None =>
                System.err.println(
                  s"Oracle UTxO with NFT not found at $oracleScriptAddressBech32"
                )
                return 1
        }

        val oracleState =
            try {
                oracleUtxo.output.requireInlineDatum.to[ChainState]
            } catch {
                case e: Exception =>
                    System.err.println(s"Error parsing ChainState: ${e.getMessage}")
                    return 1
            }

        println(s"  Oracle confirmed height: ${oracleState.blockHeight}")
        println(s"  Oracle confirmed hash:   ${oracleState.blockHash.toHex}")

        // Step 3: Fetch block from Bitcoin RPC and build tx merkle proof
        println()
        println("Step 3: Fetching block from Bitcoin RPC...")

        val rpc = new SimpleBitcoinRpc(btcConf)

        // Convert expected block hash to display-order hex for RPC
        val blockHashHex = datum.expectedBlockHash.reverse.toHex
        val expectedTxHashHex = datum.expectedTxHash.reverse.toHex

        val blockInfo =
            try {
                rpc.getBlock(blockHashHex).await(60.seconds)
            } catch {
                case e: Exception =>
                    System.err.println(
                      s"Error fetching block $blockHashHex: ${e.getMessage}"
                    )
                    return 1
            }

        println(s"  Block height: ${blockInfo.height}")
        println(s"  Transactions: ${blockInfo.tx.size}")

        // Find the transaction index
        val txIndex = blockInfo.tx.indexWhere(_.txid == expectedTxHashHex)
        if txIndex < 0 then {
            System.err.println(
              s"Transaction $expectedTxHashHex not found in block $blockHashHex"
            )
            return 1
        }
        println(s"  TX index: $txIndex")

        // Build tx merkle proof
        val txHashes = blockInfo.tx.map(t => ByteString.fromHex(t.txid).reverse)
        val merkleTree = MerkleTree.fromHashes(txHashes)
        val txMerkleProof = merkleTree.makeMerkleProof(txIndex)
        val txMerkleProofList = ScalusList.from(txMerkleProof.toList)

        println(s"  TX merkle proof size: ${txMerkleProof.size} hashes")

        // Step 4: Fetch raw block header
        println()
        println("Step 4: Fetching raw block header...")

        val blockHeaderHex =
            try {
                rpc.getBlockHeaderRaw(blockHashHex).await(30.seconds)
            } catch {
                case e: Exception =>
                    System.err.println(
                      s"Error fetching block header: ${e.getMessage}"
                    )
                    return 1
            }

        val blockHeader = BlockHeader(ByteString.fromHex(blockHeaderHex))
        println(s"  Block header: ${blockHeaderHex.take(40)}...")

        // Step 5: Reconstruct off-chain MPF and generate membership proof
        println()
        println("Step 5: Reconstructing MPF and generating membership proof...")

        val initialMpf =
            OffChainMPF.empty.insert(oracleState.blockHash, oracleState.blockHash)
        val offChainMpf: OffChainMPF =
            if initialMpf.rootHash == oracleState.confirmedBlocksRoot then
                println(s"  MPF state: single confirmed block")
                initialMpf
            else
                // Previous promotions occurred - rebuild MPF from Bitcoin RPC
                val startHeight = oracleConf.startHeight match {
                    case Some(h) => h
                    case None =>
                        System.err.println(
                          s"  Error: Previous promotions detected but ORACLE_START_HEIGHT not configured."
                        )
                        return 1
                }
                println(
                  s"  Rebuilding MPF from blocks $startHeight to ${oracleState.blockHeight}..."
                )
                def rebuildMpf(
                    heights: List[Long],
                    mpf: OffChainMPF
                ): Future[OffChainMPF] = {
                    heights match {
                        case Nil => Future.successful(mpf)
                        case h :: tail =>
                            for {
                                hashHex <- rpc.getBlockHash(h.toInt)
                                blockHash =
                                    ByteString.fromArray(hashHex.hexToBytes.reverse)
                                updatedMpf = mpf.insert(blockHash, blockHash)
                                result <- rebuildMpf(tail, updatedMpf)
                            } yield result
                    }
                }
                val heights =
                    (startHeight to oracleState.blockHeight.toLong).toList
                val rebuiltMpf =
                    try {
                        rebuildMpf(heights, OffChainMPF.empty).await(120.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"  Error rebuilding MPF: ${e.getMessage}"
                            )
                            return 1
                    }
                if rebuiltMpf.rootHash != oracleState.confirmedBlocksRoot then {
                    System.err.println(
                      s"  Error: Rebuilt MPF root does not match on-chain confirmedBlocksRoot."
                    )
                    System.err.println(
                      s"  Expected: ${oracleState.confirmedBlocksRoot.toHex}"
                    )
                    System.err.println(s"  Got: ${rebuiltMpf.rootHash.toHex}")
                    return 1
                }
                println(s"  MPF rebuilt successfully (${heights.size} blocks)")
                rebuiltMpf

        // Generate block membership proof
        val blockMpfProof =
            try {
                offChainMpf.proveMembership(datum.expectedBlockHash)
            } catch {
                case e: NoSuchElementException =>
                    System.err.println(
                      s"  Error: Block ${datum.expectedBlockHash.reverse.toHex} not found in Oracle's confirmed blocks."
                    )
                    System.err.println(
                      s"  The block may not yet be confirmed in the Oracle."
                    )
                    return 1
            }

        println(s"  Block MPF proof size: ${blockMpfProof.length} steps")

        // Step 6: Build and submit unlock transaction
        println()
        println("Step 6: Building unlock transaction...")

        val redeemerValue = TxVerifierRedeemer(
          txIndex = BigInt(txIndex),
          txMerkleProof = txMerkleProofList,
          blockMpfProof = blockMpfProof,
          blockHeader = blockHeader
        )

        try {
            val tx = TxBuilder(provider.cardanoInfo)
                .references(oracleUtxo)
                .spend(utxoToSpend, redeemerValue, script)
                .payTo(sponsorAddress, utxoToSpend.output.value)
                .complete(provider, sponsorAddress)
                .await(60.seconds)
                .sign(signer)
                .transaction

            val result = provider.submit(tx).await(30.seconds)
            result match {
                case Right(resultTxHash) =>
                    val hash = resultTxHash.toHex
                    println()
                    println("Funds unlocked successfully!")
                    println(s"  Transaction: $hash")
                    println()
                    println("The on-chain validator verified:")
                    println("  1. The block is confirmed in the Binocular Oracle")
                    println("  2. The block header hashes to the expected block hash")
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
}
