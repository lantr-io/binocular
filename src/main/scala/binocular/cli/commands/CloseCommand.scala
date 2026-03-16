package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.txbuilder.TransactionSigner

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scalus.utils.await

/** Close oracle, burn NFT, return min_ada */
case class CloseCommand(utxo: String) extends Command {

    override def execute(config: BinocularConfig): Int = {
        println(s"Closing oracle at $utxo...")
        println()

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                closeOracle(txHash, outputIndex, config)
        }
    }

    private def closeOracle(txHash: String, outputIndex: Int, config: BinocularConfig): Int = {
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

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = oracleConf.transactionTimeout.seconds

        val hdAccount = walletConf.createHdAccount() match {
            case Right(acc) => acc
            case Left(err) =>
                System.err.println(s"Error creating wallet account: $err")
                return 1
        }
        val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
        val sponsorAddress = hdAccount.baseAddress(cardanoConf.scalusNetwork)

        val provider = cardanoConf.createBlockchainProvider() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error creating blockchain provider: $err")
                return 1
        }

        // Fetch oracle UTxO
        val input = TransactionInput(TransactionHash.fromHex(txHash), outputIndex)
        val oracleUtxo: Utxo = provider.findUtxo(input).await(timeout) match {
            case Right(u) => u
            case Left(err) =>
                System.err.println(s"Error: Oracle UTxO not found at $txHash:$outputIndex")
                return 1
        }

        println(s"Found oracle UTxO: $txHash:$outputIndex")

        val scriptAddress = Address.fromBech32(oracleScriptAddress)
        val script = BitcoinContract.makeContract(params).script

        // Find reference script
        val referenceScriptUtxo: Option[Utxo] = {
            val refs = OracleTransactions.findReferenceScriptUtxos(
              provider,
              scriptAddress,
              script.scriptHash,
              timeout
            )
            refs.headOption.flatMap { case (refHash, refIdx) =>
                val refInput = TransactionInput(TransactionHash.fromHex(refHash), refIdx)
                provider.findUtxo(refInput).await(timeout) match {
                    case Right(u) => Some(u)
                    case Left(_)  => None
                }
            }
        }

        println("Building close transaction...")

        OracleTransactions.buildAndSubmitCloseTransaction(
          signer,
          provider,
          scriptAddress,
          sponsorAddress,
          oracleUtxo,
          script,
          referenceScriptUtxo,
          timeout
        ) match {
            case Right(closeTxHash) =>
                println()
                println("Oracle closed successfully!")
                println(s"  Transaction Hash: $closeTxHash")
                println(s"  NFT burned, min_ada returned to wallet")
                0
            case Left(err) =>
                System.err.println(s"Error closing oracle: $err")
                1
        }
    }
}
