package binocular.cli.commands

import binocular.*
import binocular.watchtower.*
import binocular.cli.{Command, Console}

import scalus.cardano.ledger.{Coin, Script, ScriptRef, Transaction, TransactionHash, TransactionInput, TransactionOutput, Value}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.{TransactionBuilderStep, TxBuilder}
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Publishes the heavy Plutus scripts the completion paths use as reference UTxOs: peg-in side
  * (`peg_in`, `bridged_token`, `completed_peg_ins`) and peg-out side (`peg_out`,
  * `completed_peg_outs`) — 5 in total (`bridged_token` is shared by both burns/mints).
  *
  * Each script gets pinned to a Babbage-era output at the sponsor's wallet address with
  * `script_ref` set. Once these outputs land on chain, pegin-/pegout-complete pass their outRefs as
  * reference inputs and drop the inlined script bytes from their witness sets — bringing each tx
  * well under Cardano's 16 KB max-tx-size limit (we hit 21 KB without this).
  *
  * The outputs live at the wallet's own address so they remain spendable if the bridge is ever
  * decommissioned; a reference input only requires the UTxO to still exist, not for it to be at a
  * script address. Prints the resulting outpoints so they can go into the bridge config.
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
        val cpiRefInput = parseRef("completed-peg-ins-one-shot-ref", cfg.completedPegInsOneShotRef)
        val cpiOneShotRef = TxOutRef(TxId(cpiRefInput.transactionId), cpiRefInput.index)
        if cfg.completedPegOutsOneShotRef.forall(_.trim.isEmpty) then {
            Console.error(
              "bridge.completed-peg-outs-one-shot-ref is not set — run deploy-bridge first"
            )
            break(1)
        }
        val cpoRefInput =
            parseRef("completed-peg-outs-one-shot-ref", cfg.completedPegOutsOneShotRef.get)
        val cpoOneShotRef = TxOutRef(TxId(cpoRefInput.transactionId), cpoRefInput.index)

        // Re-derive the 5 scripts the completion paths need (peg-in: peg_in, bridged_token,
        // completed_peg_ins; peg-out: peg_out, completed_peg_outs) — same constructor invocations
        // DeployBridgeCommand uses, so the hashes line up exactly. (bridged_token is shared.)
        // Blueprint script() — must match DeployBridgeCommand and the watchtower exactly.
        val tmNftPolicy = ByteString.fromArray(
          TreasuryMovementContract
              .script(oraclePolicyId, configNftPolicy, configNftAsset)
              .scriptHash
              .bytes
        )
        val pegIn =
            PegInContract(blueprint, oraclePolicyId, configNftPolicy, configNftAsset, tmNftPolicy)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy, configNftAsset)
        val cpi =
            CompletedPegInsContract(blueprint, configNftPolicy, configNftAsset, cpiOneShotRef)
        val pegOut = PegOutContract(blueprint, oraclePolicyId, configNftPolicy, configNftAsset)
        val cpo =
            CompletedPegOutsContract(blueprint, configNftPolicy, configNftAsset, cpoOneShotRef)

        Console.info("peg_in script hash", pegIn.policyId.toHex)
        Console.info("bridged_token script hash", bridgedToken.policyId.toHex)
        Console.info("completed_peg_ins script hash", cpi.policyId.toHex)
        Console.info("peg_out script hash", pegOut.policyId.toHex)
        Console.info("completed_peg_outs script hash", cpo.policyId.toHex)
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
            // BlockfrostProvider drops `scriptRef` from returned UTxOs, so coin selection can pick a
            // reference-script UTxO (e.g. a leftover deploy from a prior run) and under-estimate the
            // Conway reference-script fee → FeeTooSmallUTxO; spending it would also destroy a deployed
            // ref script. Exclude every ref-script UTxO at the sponsor address from selection and pass
            // the filtered set to the sync `complete`. Recomputed per tx so refs published earlier in
            // THIS run are excluded too (their UTxOs are indexed before the next submit). Same fix as
            // OracleTransactions.buildOptimalUpdateTransaction's `excludeInputs`.
            val excludeInputs =
                binocular.cli.CommandHelpers.refScriptOutpoints(config, sponsorAddress.encode.get)
            val sponsorUtxos =
                provider.findUtxos(sponsorAddress).await(timeout) match {
                    case Right(u) =>
                        u.filterNot { case (input, _) => excludeInputs.contains(input) }
                    case Left(err) =>
                        Console.error(s"$label fetch sponsor UTxOs: $err"); return None
                }
            val tx: Transaction =
                try
                    TxBuilder(provider.cardanoInfo)
                        .addSteps(TransactionBuilderStep.Send(output))
                        .complete(sponsorUtxos, sponsorAddress)
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
                    // await window MUST exceed the poll budget (attempts*delayMs) or it preempts the
                    // poll and throws even when the tx confirms. See DeployBridgeCommand.confirmAwait.
                    val status = provider
                        .pollForConfirmation(
                          TransactionHash.fromHex(txHash),
                          maxAttempts = DeployBridgeCommand.ConfirmPollAttempts,
                          delayMs = DeployBridgeCommand.ConfirmPollDelayMs
                        )
                        .await(DeployBridgeCommand.confirmAwait)
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

        // Each entry: (label, script). Skip any whose script hash already has a reference UTxO at the
        // sponsor wallet so re-running (e.g. to add the peg-out side after the peg-in side) doesn't
        // re-publish — and waste ~50 ADA on — refs that already exist. Discovery is on-chain
        // (BlockfrostProvider drops scriptRef, so scan `reference_script_hash` directly), not config.
        val candidates = List(
          ("peg_in", pegIn.script),
          ("bridged_token", bridgedToken.script),
          ("completed_peg_ins", cpi.script),
          ("peg_out", pegOut.script),
          ("completed_peg_outs", cpo.script)
        )
        val deployedHashes = binocular.cli.CommandHelpers
            .refScriptUtxosByHash(config, sponsorAddress.encode.getOrElse(""))
            .keySet
        val (already, toPublish) =
            candidates.partition { case (_, script) => deployedHashes.contains(script.scriptHash) }
        already.foreach { case (label, script) =>
            Console.info(s"$label ref already deployed, skipping", script.scriptHash.toHex)
        }

        // Submit serially so each tx selects fresh wallet UTxOs.
        val results: List[(String, (String, Int))] =
            toPublish.flatMap { case (label, script) =>
                submitOne(label, refOutput(script)).map(r => label -> r)
            }

        if results.size != toPublish.size then {
            Console.error(s"Only ${results.size}/${toPublish.size} reference scripts published")
            break(1)
        }

        println()
        Console.success(
          "Bridge script references deployed. No config needed — the completion paths discover " +
              "them on-chain by script hash. Published outpoints (for your records):"
        )
        for (label, (hash, idx)) <- results do Console.info(s"$label ref", s"$hash#$idx")
        0
    }
}
