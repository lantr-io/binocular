package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await

/** Create a TMTx UTxO on Cardano for testing the relay command.
  *
  * Mints a TMTx token using PlutusV3.alwaysOk as the minting policy, attaches an inline datum
  * containing a (possibly invalid) Bitcoin transaction, and sends it to the script address derived
  * from the minting policy hash.
  */
case class CreateTmtxCommand(btcTxHex: String) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Create TMTx UTxO")
        println()
        createTmtx(config)
    }

    private def createTmtx(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = 120.seconds

        // Set up Cardano provider and wallet
        val provider: BlockchainProvider = config.cardano.createBlockchainProvider() match {
            case Right(p) => p
            case Left(err) =>
                Console.error(s"Cardano provider: $err")
                break(1)
        }

        val hdAccount: HdAccount = config.wallet.createHdAccount() match {
            case Right(a) => a
            case Left(err) =>
                Console.error(s"Wallet: $err")
                break(1)
        }

        val network = config.cardano.scalusNetwork
        val signer = hdAccount.signerForUtxos
        val sponsorAddress = hdAccount.baseAddress(network)

        // Use PlutusV3.alwaysOk as the minting policy (matches demo alwaysOK script)
        val mintingScript = PlutusV3.alwaysOk
        val policyId = mintingScript.script.scriptHash
        val assetName = AssetName.fromString(config.relay.tmtxAssetName)
        val scriptAddress = Address(network, Credential.ScriptHash(policyId))

        Console.info("Cardano", config.cardano.network)
        Console.info(
          "Wallet",
          sponsorAddress.encode.getOrElse("?")
        )
        Console.info("TMTx policy", policyId.toHex)
        Console.info("TMTx asset", config.relay.tmtxAssetName)
        Console.info("Script address", scriptAddress.encode.getOrElse("?"))
        Console.info("BTC tx hex", s"${btcTxHex.take(40)}... (${btcTxHex.length / 2} bytes)")

        // Check if the policy ID matches the config
        if policyId.toHex != config.relay.tmtxPolicyId then
            Console.warn(
              s"Policy ID mismatch! Script produces ${policyId.toHex}, config expects ${config.relay.tmtxPolicyId}"
            )
            Console.warn("Update relay.tmtx-policy-id in your config to match.")

        Console.separator()
        println()

        // Build the datum: Constr 0 [ByteString]
        val btcTxBytes = ByteString.fromHex(btcTxHex)
        import scalus.cardano.onchain.plutus.prelude.List as ScalusList
        val datum = Data.Constr(0, ScalusList(Data.B(btcTxBytes)))

        // Mint TMTx token and send to script address with datum
        val mintAssets = Map(assetName -> 1L)
        val outputValue = Value.asset(policyId, assetName, 1, Coin(2_000_000))

        Console.log("Building transaction...")

        try {
            val tx = TxBuilder(provider.cardanoInfo)
                .mint(mintingScript, mintAssets, Data.unit)
                .payTo(scriptAddress, outputValue, datum)
                .complete(provider, sponsorAddress)
                .await(timeout)
                .sign(signer)
                .transaction

            Console.log("Submitting transaction...")

            val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
                case Right(hash) => hash
                case Left(err) =>
                    Console.error(s"Submit failed: $err")
                    break(1)
            }

            Console.logSuccess(s"Transaction submitted: $txHash")
            Console.log("Waiting for confirmation...")

            val status = provider
                .pollForConfirmation(
                  TransactionHash.fromHex(txHash),
                  maxAttempts = 60,
                  delayMs = 2000
                )
                .await(timeout)

            status match {
                case TransactionStatus.Confirmed =>
                    Console.logSuccess(s"TMTx UTxO created and confirmed!")
                    Console.info("TxHash", txHash)
                    Console.info("Script address", scriptAddress.encode.getOrElse("?"))
                    println()
                    Console.log(
                      "Run 'binocular relay --dry-run' to verify the relay picks up this UTxO."
                    )
                    0
                case other =>
                    Console.logError(s"Transaction status: $other")
                    1
            }
        } catch {
            case e: Exception =>
                Console.error(s"Failed: ${e.getMessage}")
                e.printStackTrace()
                1
        }
    }
}
