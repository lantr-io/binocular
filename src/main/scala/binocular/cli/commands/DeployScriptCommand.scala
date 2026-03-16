package binocular.cli.commands

import binocular.*
import binocular.cli.Command
import scalus.cardano.address.Address
import scalus.cardano.txbuilder.TransactionSigner

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

/** Deploy the oracle validator as a reference script UTxO */
case class DeployScriptCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        println("Deploying oracle validator reference script...")
        println()

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

        val scriptAddress = Address.fromBech32(oracleScriptAddress)
        val script = BitcoinContract.makeContract(params).script

        // Check if already deployed
        val existingRefs = OracleTransactions.findReferenceScriptUtxos(
          provider,
          scriptAddress,
          script.scriptHash,
          timeout
        )

        if existingRefs.nonEmpty then {
            val (refTxHash, refIdx) = existingRefs.head
            println(s"Reference script already deployed at $refTxHash:$refIdx")
            return 0
        }

        println("No existing reference script found. Deploying...")

        OracleTransactions.deployReferenceScript(
          signer,
          provider,
          sponsorAddress,
          scriptAddress,
          script,
          timeout
        ) match {
            case Right((deployTxHash, deployOutputIdx, _)) =>
                println()
                println("Reference script deployed successfully!")
                println(s"  TX Hash: $deployTxHash")
                println(s"  Output Index: $deployOutputIdx")
                println(s"  Reference: $deployTxHash:$deployOutputIdx")
                0
            case Left(err) =>
                System.err.println(s"Error deploying reference script: $err")
                1
        }
    }
}
