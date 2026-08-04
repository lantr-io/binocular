package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.ledger.{AssetName, TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.Data

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Mint a fresh completed-peg-outs trie against a LIVE bridge — the executable half of the
  * ConfigDatum field-3 migration.
  *
  * The trie's `"CPO"` NFT can only be created by the trie validator's own mint branch, which
  * consumes a one-shot UTxO. Until this command existed the only code path that ran that mint was
  * `deploy-bridge`, which mints a whole NEW bridge (new config NFT, new peg_in/peg_out hashes) — so
  * an in-place migration of an existing bridge was not possible.
  *
  * Trie v2 makes the completed-peg-outs validator take `(tm_nft_policy_id, one_shot_input_ref)`.
  * The TM script hash comes from the live bridge (oracle hash + the deployed config NFT), so the
  * only free parameter is the one-shot, and a fresh one-shot yields a fresh policy with an
  * empty-root trie UTxO.
  *
  * Migration order (the rest is the runbook's):
  *   1. run this command; it prints the new policy id and the one-shot ref;
  *   2. run `update-config --completed-peg-outs-policy <policy> --peg-out-withdraw-hash <hash>
  *      [--peg-in-withdraw-hash <hash>]` — ONE transaction, all fields together;
  *   3. set `bridge.completed-peg-outs-one-shot-ref` to the printed ref, so `confirm-tmtx` derives
  *      the same script;
  *   4. restart the watchtower.
  *
  * Between steps 1 and 2 nothing is live: the config still publishes the old policy, so the TM
  * validator still looks for the old trie and the freshly minted one is simply idle.
  *
  * The trie starts EMPTY. Any peg-out recorded in a previous trie is not carried over — see the
  * runbook's upgrade rule; this command deliberately does not attempt a migration of trie contents,
  * because the Aiken mint handler pins the genesis root to 32 zero bytes.
  */
case class BootstrapCompletedPegOutsCommand(
    oneShotRef: Option[String] = None,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Bootstrap Completed-Peg-Outs Trie (config field 3 migration)")
        if dryRun then Console.warn("Dry-run mode — will compute the policy but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val sponsorAddress = setup.sponsorAddress

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)

        // The TM NFT policy = the deployed TreasuryMovementValidator script hash. Derived exactly
        // as confirm-tmtx and deploy-bridge derive it, so all three agree on the trie policy.
        val tmNftPolicy = CommandHelpers.tmNftPolicy(config, setup.script.scriptHash)
        Console.info("TM NFT policy", tmNftPolicy.toHex)

        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }

        // An explicit --one-shot-ref must still be an UNSPENT wallet UTxO: the mint handler checks
        // the outref is among the tx inputs, so a spent or foreign ref can never satisfy it.
        val oneShotUtxo: Utxo = oneShotRef.map(_.trim).filter(_.nonEmpty) match {
            case Some(s) =>
                val wanted = s.split('#') match {
                    case Array(h, i) if h.length == 64 && i.toIntOption.isDefined =>
                        TransactionInput(TransactionHash.fromHex(h), i.toInt)
                    case _ =>
                        Console.error(s"--one-shot-ref must be TX_HASH#INDEX, got '$s'")
                        break(1)
                }
                walletUtxos.find(_.input == wanted).getOrElse {
                    Console.error(
                      s"--one-shot-ref $s is not an unspent UTxO of the sponsor wallet " +
                          s"($sponsorAddress). The mint requires it to be spent by this tx."
                    )
                    break(1)
                }
            case None =>
                // Same selection rule as deploy-bridge: pure ADA, big enough, and never a CIP-33
                // reference-script UTxO (spending one destroys a deployed reference script).
                val excluded =
                    CommandHelpers.refScriptOutpoints(config, sponsorAddress.encode.getOrElse(""))
                BridgeBootstrap.pickOneShot(walletUtxos, excluded).getOrElse {
                    Console.error(
                      s"No clean pure-ADA wallet UTxO (>=${BridgeBootstrap.MinOneShotLovelace / 1000000} " +
                          "ADA, excluding reference-script UTxOs) to use as the one-shot; fund the " +
                          "sponsor wallet or pass --one-shot-ref"
                    )
                    break(1)
                }
        }
        val oneShot = TxOutRef(TxId(oneShotUtxo.input.transactionId), oneShotUtxo.input.index)
        val oneShotDisplay = s"${oneShotUtxo.input.transactionId.toHex}#${oneShotUtxo.input.index}"

        val cpoContract = CompletedPegOutsContract(blueprint, tmNftPolicy, oneShot)
        val (trieAddress, trieValue, trieDatum) =
            BridgeBootstrap.completedPegOutsOutput(cpoContract, network)

        Console.info("one-shot", oneShotDisplay)
        Console.info("completed-peg-outs policy", cpoContract.policyId.toHex)
        Console.info("completed-peg-outs asset", CompletedPegOutsContract.assetName.toHex)
        Console.info("completed-peg-outs address", trieAddress.encode.getOrElse("?"))
        Console.info("genesis root", BridgeBootstrap.EmptyRoot.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (computed the trie policy; not submitting)")
            printNextSteps(cpoContract.policyId.toHex, oneShotDisplay)
            break(0)
        }

        Console.step(1, "Minting the completed-peg-outs trie NFT")
        val tx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(oneShotUtxo)
                    .mint(
                      cpoContract.script,
                      Map(AssetName(CompletedPegOutsContract.assetName) -> 1L),
                      Data.unit
                    )
                    .payTo(trieAddress, trieValue, trieDatum)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(setup.hdAccount.signerForUtxos)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building the trie bootstrap tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }

        val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
            case Right(h)  => h
            case Left(err) => Console.error(s"Submit: $err"); break(1)
        }
        val status = provider
            .pollForConfirmation(
              TransactionHash.fromHex(txHash),
              maxAttempts = DeployBridgeCommand.ConfirmPollAttempts,
              delayMs = DeployBridgeCommand.ConfirmPollDelayMs
            )
            .await(DeployBridgeCommand.confirmAwait)
        status match {
            case TransactionStatus.Confirmed =>
                Console.success(s"Completed-peg-outs trie bootstrapped: $txHash")
                println()
                printNextSteps(cpoContract.policyId.toHex, oneShotDisplay)
                0
            case other =>
                Console.error(s"Not confirmed: $other")
                1
        }
    }

    /** Print the exact follow-up commands, so the migration cannot be half-applied by guesswork. */
    private def printNextSteps(policyHex: String, oneShotDisplay: String): Unit = {
        Console.separator()
        Console.info("completed-peg-outs-policy-id", policyHex)
        Console.info("completed-peg-outs-one-shot-ref", oneShotDisplay)
        Console.info(
          "next 1",
          s"binocular update-config --completed-peg-outs-policy $policyHex " +
              "--peg-out-withdraw-hash <new peg_out hash> [--peg-in-withdraw-hash <new peg_in hash>]"
        )
        Console.info(
          "next 2",
          s"set binocular.bridge.completed-peg-outs-one-shot-ref = $oneShotDisplay, then restart " +
              "the watchtower"
        )
        Console.separator()
    }
}
