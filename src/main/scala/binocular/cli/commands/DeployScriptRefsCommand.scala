package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, Console}

import scalus.cardano.ledger.{Coin, Script, ScriptRef, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.{TransactionBuilderStep, TxBuilder}
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Publishes the 3 heavy Plutus scripts the pegin-complete path uses as reference UTxOs.
  *
  * Each script gets pinned to a Babbage-era output at the sponsor's wallet address with
  * `script_ref` set. Once these three outputs land on chain, pegin-complete passes their outRefs as
  * reference inputs and drops the ~28 KB of inlined script bytes from its witness set — bringing
  * the tx well under Cardano's 16 KB max-tx-size limit (we hit 21 KB without this).
  *
  * The outputs live at the wallet's own address so they remain spendable if the bridge is ever
  * decommissioned; a reference input only requires the UTxO to still exist, not for it to be at a
  * script address. Prints the three resulting outpoints so they can go into the bridge config.
  */
case class DeployScriptRefsCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Deploy Bridge Script References")
        if dryRun then Console.warn("Dry-run mode — will build but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = binocular.cli.CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val sponsorAddress = setup.sponsorAddress
        val oraclePolicyId = ByteString.fromArray(setup.script.scriptHash.bytes)

        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(
                      s"Loading bridge blueprint (${config.bridge.plutusJson}): ${e.getMessage}"
                    )
                    break(1)
            }

        def parseRef(label: String, s: String): TransactionInput = s.split("#") match {
            case Array(h, i) if i.toIntOption.isDefined =>
                TransactionInput(TransactionHash.fromHex(h), i.toInt)
            case _ =>
                Console.error(s"Invalid $label: expected TX_HASH#INDEX, got '$s'"); break(1)
        }

        val cfg = config.bridge
        if cfg.completedPegInsOneShotRef.isEmpty then {
            Console.error(
              "bridge.completed-peg-ins-one-shot-ref is empty — run deploy-bridge first"
            )
            break(1)
        }
        val configNftPolicy = ByteString.fromHex(cfg.configNftPolicyId)
        val configNftAsset = ByteString.fromHex(cfg.configNftAssetName)
        val tmControlPolicy = ByteString.fromHex(cfg.tmControlNftPolicy)
        val tmControlAsset = ByteString.fromHex(cfg.tmControlNftName)
        val cpiRefInput = parseRef("completed-peg-ins-one-shot-ref", cfg.completedPegInsOneShotRef)
        val cpiOneShotRef = TxOutRef(TxId(cpiRefInput.transactionId), cpiRefInput.index)

        // Re-derive the 3 scripts the pegin-complete path needs — single source of truth is the
        // same constructor invocations DeployBridgeCommand uses, so the hashes line up exactly.
        val tmNftPolicy = ByteString.fromArray(
          TreasuryMovementContract
              .contract(oraclePolicyId, tmControlPolicy, tmControlAsset)
              .script
              .scriptHash
              .bytes
        )
        val pegIn =
            PegInContract(blueprint, oraclePolicyId, configNftPolicy, configNftAsset, tmNftPolicy)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy, configNftAsset)
        val cpi =
            CompletedPegInsContract(blueprint, configNftPolicy, configNftAsset, cpiOneShotRef)

        Console.info("peg_in script hash", pegIn.policyId.toHex)
        Console.info("bridged_token script hash", bridgedToken.policyId.toHex)
        Console.info("completed_peg_ins script hash", cpi.policyId.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (computed hashes, not submitting)")
            break(0)
        }

        val signer = setup.signer

        // 50 ADA per output: generously above the minUTxO formula for a ~13 KB script-bearing
        // output. The excess comes back as change when the ref UTxO is ever spent.
        val baseAda = Coin(50_000_000L)
        def refOutput(script: Script.PlutusV3): TransactionOutput =
            TransactionOutput.Babbage(
              sponsorAddress,
              Value(baseAda),
              datumOption = None,
              scriptRef = Some(ScriptRef(script))
            )

        def submitOne(label: String, output: TransactionOutput): Option[(String, Int)] = {
            Console.step(0, s"Publishing $label reference script")
            val tx: Transaction =
                try
                    TxBuilder(provider.cardanoInfo)
                        .addSteps(TransactionBuilderStep.Send(output))
                        .complete(provider, sponsorAddress)
                        .await(timeout)
                        .sign(signer)
                        .transaction
                catch {
                    case e: Exception =>
                        Console.error(s"$label build: ${e.getMessage}")
                        return None
                }
            val submitResult: Either[String, String] =
                binocular.oracle.OracleTransactions.submitTx(provider, tx, timeout)
            submitResult match {
                case Left(err) =>
                    Console.error(s"$label submit: $err")
                    None
                case Right(txHash) =>
                    val status = provider
                        .pollForConfirmation(
                          TransactionHash.fromHex(txHash),
                          maxAttempts = 60,
                          delayMs = 2000
                        )
                        .await(timeout)
                    status match {
                        case TransactionStatus.Confirmed =>
                            // Wait for the address-based UTxO index to reflect this tx before
                            // the next submit, so its fee/change selection doesn't pick the same
                            // already-spent inputs (pollForConfirmation checks tx status, not
                            // the address index; Blockfrost lags by a few slots between them).
                            // Same convention as DeployBridgeCommand:200-215.
                            binocular.oracle.OracleTransactions
                                .waitForUtxoAtAddress(
                                  provider,
                                  sponsorAddress,
                                  TransactionHash.fromHex(txHash),
                                  timeout
                                ) match {
                                case Left(err) =>
                                    Console.error(s"$label UTxO-index wait: $err")
                                    None
                                case Right(_) =>
                                    Console.success(s"$label ref UTxO: $txHash#0")
                                    Some((txHash, 0))
                            }
                        case other =>
                            Console.error(s"$label not confirmed: $other")
                            None
                    }
            }
        }

        // Submit serially so each tx selects fresh wallet UTxOs.
        val results: List[(String, (String, Int))] =
            List(
              "peg_in" -> refOutput(pegIn.script),
              "bridged_token" -> refOutput(bridgedToken.script),
              "completed_peg_ins" -> refOutput(cpi.script)
            ).flatMap { case (label, out) =>
                submitOne(label, out).map(r => label -> r)
            }

        if results.size != 3 then {
            Console.error(s"Only ${results.size}/3 reference scripts published")
            break(1)
        }

        println()
        Console.success("Bridge script references deployed. Set these in binocular.bridge:")
        for (label, (hash, idx)) <- results do Console.info(s"$label-script-ref", s"$hash#$idx")
        0
    }
}
