package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, Console, DaemonExecution}
import binocular.notify.Notifier
import binocular.watchtower.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import cats.syntax.either.*

/** Complete PAID peg-out requests manually — the one-shot form of the watchtower's POR sweeper.
  *
  * Since spec rev 5.1 completion is PERMISSIONLESS cleanup: it burns a request's locked fBTC
  * against a value-bound membership proof in the completed-peg-outs trie, and whoever submits it
  * keeps the request's MIN_ADA. There is no owner signature, no Bitcoin SPV bundle, and no trie
  * update — the trie is a reference input, so completions never contend with each other.
  *
  * The watchtower runs this automatically after every TM Confirm (`bridge.por-sweeper`, on by
  * default), so this command exists for operators and for the migration runbook's verification
  * step: proving that a third party who is not the request's owner can complete it.
  *
  * It shares [[BridgeSweepSetup]] and [[PorSweeper]] with the watchtower, so it resolves the same
  * UTxOs and fails with the same messages. With no `--pegout` it completes EVERY completable
  * request at the peg-out address; with one it restricts submission to that request. `--dry-run`
  * reports what it would do and submits nothing.
  *
  * This replaces the pre-rev-5 flow entirely: the `--tm` and `--prior-pegout` flags are gone,
  * because a completion no longer re-proves the Treasury Movement (Confirm did that once) and no
  * longer inserts into the trie (heimdall's attested root does).
  */
case class PegOutCompleteCommand(pegOut: Option[String] = None, dryRun: Boolean = false)
    extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Peg-Out Complete (permissionless cleanup)")
        if dryRun then Console.warn("Dry-run mode — will report but not submit")
        println()

        given ec: ExecutionContext = DaemonExecution.ec
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val oracleScriptHashBS = ByteString.fromArray(setup.script.scriptHash.bytes)

        val (bridgeBlueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }

        // The trie validator is parameterized by the TM script hash, so the TM script has to be
        // derived here too even though no TM is touched.
        val tmScript = TreasuryMovementContract.script(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.configNftPolicyId),
          ByteString.fromHex(config.bridge.configNftAssetName)
        )
        val tmAddress = Address(network, Credential.ScriptHash(tmScript.scriptHash))
        val cpoOneShot = config.bridge.completedPegOutsOneShotRef
            .map(_.trim)
            .filter(_.nonEmpty)
            .flatMap(s =>
                s.split('#') match {
                    case Array(h, i) if h.length == 64 && i.toIntOption.isDefined =>
                        Some(TxOutRef(TxId(ByteString.fromHex(h)), BigInt(i.toInt)))
                    case _ => None
                }
            )
            .getOrElse {
                Console.error(
                  "bridge.completed-peg-outs-one-shot-ref must be TX_HASH#INDEX — without it the " +
                      "completed-peg-outs trie validator cannot be derived, and the CPO singleton " +
                      "cannot be located."
                )
                break(1)
            }
        val trieScript = CompletedPegOutsContract(
          bridgeBlueprint,
          ByteString.fromArray(tmScript.scriptHash.bytes),
          cpoOneShot
        ).script
        val configNftPolicy = ScriptHash.fromHex(config.bridge.configNftPolicyId)
        val configNftAsset = AssetName(ByteString.fromHex(config.bridge.configNftAssetName))
        val configAddress = Address(network, Credential.ScriptHash(configNftPolicy))

        Console.info("Cardano", config.cardano.network)
        Console.info("bridge blueprint", blueprintSource)
        Console.info("completed-peg-outs policy", trieScript.scriptHash.toHex)

        val sweeper = BridgeSweepSetup
            .buildPorSweeper(
              config,
              provider,
              setup.hdAccount,
              bridgeBlueprint,
              configNftPolicy,
              configNftAsset,
              tmAddress,
              network,
              Notifier.fromConfig(config.notifications),
              dryRun,
              timeout
            )
            .valueOr { err =>
                Console.error(s"Building the peg-out sweeper: $err"); break(1)
            }

        val trieCtx = BridgeSweepSetup
            .loadTrieContext(
              provider,
              configAddress,
              configNftPolicy,
              configNftAsset,
              trieScript,
              AssetName(CompletedPegOutsContract.assetName),
              network,
              timeout
            )
            .valueOr { err =>
                Console.error(s"completed-peg-outs trie: $err"); break(1)
            }
        Console.info("on-chain trie root", trieCtx.currentRoot.toHex)
        Console.separator()
        println()

        // The mirror is caught up from chain history here — a standalone run has no confirm hints —
        // and the reconstruction is cross-checked against the on-chain root before any proof is
        // built. A halt means the trie could not be reconciled and nothing was submitted.
        sweeper.sweep(trieCtx.configUtxo, trieCtx.trieUtxo, pegOut.map(normalizeRef))
        if sweeper.isHalted then 1 else 0
    }

    /** Accept both `TX_HASH#INDEX` and `TX_HASH:INDEX`; the sweeper reports refs in the `#` form.
      */
    private def normalizeRef(s: String): String = s.trim.replace(':', '#')
}
