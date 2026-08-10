package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, Console, DaemonExecution}
import binocular.notify.Notifier
import binocular.watchtower.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash}
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import cats.syntax.either.*

/** Complete PAID peg-out requests manually — the one-shot form of the watchtower's POR sweeper.
  *
  * Since spec rev 5.1 completion is PERMISSIONLESS cleanup: it burns a request's locked fBTC
  * against a value-bound membership proof of the singleton's `cpo_root`, and whoever submits it
  * keeps the request's MIN_ADA. There is no owner signature, no Bitcoin SPV bundle, and no trie
  * update — the bridge-state singleton is a reference input, so completions never contend with each
  * other.
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

        // The TM address is needed only for the sweeper's chain-history reconstruction; the
        // singleton itself is located through the Config at runtime ([PAR-1]), so this command
        // needs no one-shot ref and derives no bridge_state script (it only REFERENCES the
        // singleton, never spends it).
        val tmScript = TreasuryMovementContract.script(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.configNftPolicyId),
          ByteString.fromHex(config.bridge.configNftAssetName)
        )
        val tmAddress = Address(network, Credential.ScriptHash(tmScript.scriptHash))
        val configNftPolicy = ScriptHash.fromHex(config.bridge.configNftPolicyId)
        val configNftAsset = AssetName(ByteString.fromHex(config.bridge.configNftAssetName))
        val configAddress = Address(network, Credential.ScriptHash(configNftPolicy))

        Console.info("Cardano", config.cardano.network)
        Console.info("bridge blueprint", blueprintSource)

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

        val ctx = BridgeSweepSetup
            .loadSingletonContext(
              provider,
              configAddress,
              configNftPolicy,
              configNftAsset,
              network,
              timeout
            )
            .valueOr { err =>
                Console.error(s"bridge-state singleton: $err"); break(1)
            }
        Console.info("bridge-state policy", ctx.config.bridgeStatePolicy.toHex)
        Console.info("on-chain cpo_root", ctx.state.cpoRoot.toHex)
        Console.separator()
        println()

        // The mirror is caught up from chain history here — a standalone run has no confirm hints —
        // and the reconstruction is cross-checked against the on-chain root before any proof is
        // built. A halt means the trie could not be reconciled and nothing was submitted.
        sweeper.sweep(ctx.configUtxo, ctx.singletonUtxo, pegOut.map(normalizeRef))
        if sweeper.isHalted then 1 else 0
    }

    /** Accept both `TX_HASH#INDEX` and `TX_HASH:INDEX`; the sweeper reports refs in the `#` form.
      */
    private def normalizeRef(s: String): String = s.trim.replace(':', '#')
}
