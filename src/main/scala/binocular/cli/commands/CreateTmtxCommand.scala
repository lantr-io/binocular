package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

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

        // Marker token: the project-unique alwaysOk policy (salt baked in for a distinct policy ID).
        val mintingScript = TmtxScript.pinnedScript
        val policyId = mintingScript.scriptHash
        val assetName = AssetName.fromString(config.relay.tmtxAssetName)

        // The Unconfirmed TM UTxO lives at the TreasuryMovementValidator address (parameterized by
        // the oracle script hash) — the same place confirm-tmtx spends it from.
        val oracleParams = config.oracle
            .toBitcoinValidatorParams(config.bitcoinNode.bitcoinNetwork)
            .valueOr { err =>
                Console.error(s"Oracle params: $err"); break(1)
            }
        val oracleScriptHashBS =
            ByteString.fromArray(BitcoinContract.script(oracleParams).scriptHash.bytes)
        val tmScript = TreasuryMovementContract.script(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.configNftPolicyId),
          ByteString.fromHex(config.bridge.configNftAssetName)
        )
        val scriptAddress = Address(network, Credential.ScriptHash(tmScript.scriptHash))

        Console.info("Cardano", config.cardano.network)
        Console.info(
          "Wallet",
          sponsorAddress.encode.getOrElse("?")
        )
        Console.info("Marker token policy", policyId.toHex)
        Console.info("Marker token asset", config.relay.tmtxAssetName)
        Console.info("TM validator address", scriptAddress.encode.getOrElse("?"))
        Console.info("BTC tx hex", s"${btcTxHex.take(40)}... (${btcTxHex.length / 2} bytes)")

        // Check if the policy ID matches the config
        if policyId.toHex != config.relay.tmtxPolicyId then
            Console.warn(
              s"Policy ID mismatch! Script produces ${policyId.toHex}, config expects ${config.relay.tmtxPolicyId}"
            )
            Console.warn("Update relay.tmtx-policy-id in your config to match.")

        Console.separator()
        println()

        // Build the Unconfirmed datum: Constr 0 [signed_btc_tx, creator, created] — matches
        // heimdall publish.rs and what TreasuryMovementValidator expects to spend. creator =
        // the sponsor payment key (it may GC the Confirmed record after the grace period);
        // created = now (the real TM mint policy anchors it to the tx validity interval).
        val btcTxBytes = ByteString.fromHex(btcTxHex)
        import scalus.cardano.onchain.plutus.prelude.List as ScalusList
        val creatorPkh = ByteString.fromArray(hdAccount.paymentKeyHash.bytes)
        // The real TM mint policy requires created == the tx validity upper bound (whole-second
        // instant: 1s slots make the ledger's slot->ms translation exact). The scaffold policy
        // used here does not check it, but keep the datum consistent with real posts.
        val validTo = java.time.Instant.ofEpochSecond(java.time.Instant.now().getEpochSecond + 1800)
        val createdMs = BigInt(validTo.toEpochMilli)
        val datum = Data.Constr(
          0,
          ScalusList(Data.B(btcTxBytes), Data.B(creatorPkh), Data.I(createdMs))
        )

        // Mint TMTx token and send to script address with datum
        val mintAssets = Map(assetName -> 1L)
        val outputValue = Value.asset(policyId, assetName, 1, Coin(2_000_000))

        Console.log("Building transaction...")

        try {
            val tx = TxBuilder(provider.cardanoInfo)
                .mint(mintingScript, mintAssets, Data.unit)
                .validTo(validTo)
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

            Console.logSuccess(s"Cardano tx submitted: $txHash")
            Console.log("Waiting for Cardano confirmation...")

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
                    Console.info("Cardano txid", txHash)
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
