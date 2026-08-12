package binocular.cli.commands

import binocular.BinocularConfig
import binocular.cli.{CommandHelpers, Console}
import binocular.notify.Notifier
import binocular.watchtower.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Try
import scalus.utils.await

/** Wiring shared by the two commands that touch the bridge-state singleton: `confirm-tmtx` (which
  * SPENDS and recreates it) and `peg-out-complete` (which REFERENCES it).
  *
  * It lives outside both so the manual completion command and the watchtower's automatic sweeper
  * resolve the same UTxOs and fail with the same messages. A divergence between them would show up
  * as "the sweeper works but the manual command does not", which is exactly the kind of drift an
  * operator cannot debug.
  */
object BridgeSweepSetup {

    /** Everything a Confirm or Complete transaction needs from the live chain: the Config UTxO
      * (always a reference input), the singleton UTxO carrying the `(bridge_state_policy, "BSS")`
      * NFT (Confirm SPENDS it; Complete REFERENCES it), and its decoded [[BridgeState]].
      */
    final case class SingletonContext(
        configUtxo: Utxo,
        config: ConfigDatum,
        singletonUtxo: Utxo,
        state: BridgeState
    )

    /** Locate the Config and bridge-state singleton UTxOs.
      *
      * The singleton policy is taken from the Config's `bridge_state_policy` at runtime ([PAR-1]) —
      * no local script derivation, so pure READERS (the sweeper, `peg-out-complete`) need no
      * one-shot ref. `confirm-tmtx`, which must SPEND the singleton, additionally checks its
      * locally derived `bridge_state` script hashes to this policy.
      */
    def loadSingletonContext(
        provider: BlockchainProvider,
        configAddress: Address,
        configNftPolicy: ScriptHash,
        configNftAsset: AssetName,
        network: Network,
        timeout: Duration
    )(using ExecutionContext): Either[String, SingletonContext] =
        for {
            configUtxos <- provider
                .findUtxos(configAddress)
                .await(timeout)
                .left
                .map(err => s"fetching config UTxOs at $configAddress: $err")
            configUtxo <- configUtxos.toList
                .collectFirst {
                    case (in, out) if out.value.hasAsset(configNftPolicy, configNftAsset) =>
                        Utxo(in, out)
                }
                .toRight(s"no UTxO carrying the config NFT at $configAddress")
            cfg <- configUtxo.output.inlineDatum
                .flatMap(d => Try(d.to[ConfigDatum]).toOption)
                .toRight("config datum does not decode as the rev-5.4 ConfigDatum")
            bssPolicy = ScriptHash.fromHex(cfg.bridgeStatePolicy.toHex)
            bssAddress = Address(network, Credential.ScriptHash(bssPolicy))
            bssAsset = AssetName(BridgeStateContract.assetName)
            singletonUtxos <- provider
                .findUtxos(bssAddress)
                .await(timeout)
                .left
                .map(err => s"fetching singleton UTxOs at $bssAddress: $err")
            singletonUtxo <- singletonUtxos.toList
                .collectFirst {
                    case (in, out) if out.value.hasAsset(bssPolicy, bssAsset) => Utxo(in, out)
                }
                .toRight(
                  s"no UTxO carrying the \"BSS\" NFT at $bssAddress — the bridge-state singleton " +
                      "has not been bootstrapped under this policy"
                )
            state <- singletonUtxo.output.inlineDatum
                .flatMap(d => Try(d.to[BridgeState]).toOption)
                .toRight(
                  "the singleton datum does not decode as the 4-field BridgeState ([LIB-1])"
                )
        } yield SingletonContext(configUtxo, cfg, singletonUtxo, state)

    /** Assemble the POR sweeper: the two scripts a Complete transaction runs, a resolver for their
      * CIP-33 reference UTxOs, the trie mirror's state directory, and the chain-history backend
      * used for cold-start reconstruction.
      *
      * Nothing here reads the deployed Config — the derived hashes are checked against it on every
      * sweep instead, so a watchtower started before the migration Update still boots, still
      * confirms, and starts sweeping by itself once field 5 is swapped.
      *
      * NOTHING HERE TOUCHES THE NETWORK. Every value is derived from the blueprint and the local
      * config, so the only way to fail is a malformed config value — which no amount of retrying
      * fixes. Reference-script discovery, the one part that does need the network, is deferred into
      * `resolveScriptRefs` and re-run on every sweep: doing it once at startup turned a transient
      * provider error into a process-lifetime loss of sweeping, and left a reference script
      * deployed later unused forever.
      *
      * Returns Left when the sweeper cannot be built at all. The watchtower degrades to
      * confirming-without-sweeping rather than refusing to start: cleanup is not on the bridge's
      * critical path.
      */
    def buildPorSweeper(
        config: BinocularConfig,
        provider: BlockchainProvider,
        hdAccount: HdAccount,
        blueprint: BifrostBlueprint,
        configNftPolicy: ScriptHash,
        configNftAsset: AssetName,
        tmAddress: Address,
        network: Network,
        notifier: Notifier,
        dryRun: Boolean,
        timeout: Duration
    )(using ExecutionContext): Either[String, PorSweeper] = Try {
        val configNftPolicyBs = ByteString.fromArray(configNftPolicy.bytes)
        val configNftAssetBs = configNftAsset.bytes
        val pegOut = PegOutContract(blueprint, configNftPolicyBs)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicyBs)
        val ctx = PorSweeper.Context(
          network = network,
          tmAddress = tmAddress,
          pegOutScript = pegOut.script,
          pegOutAddress = pegOut.address(network),
          bridgedTokenScript = bridgedToken.script,
          bridgedTokenPolicy = bridgedToken.policyId,
          bridgedTokenAsset = AssetName(ByteString.fromHex(config.bridge.bridgedTokenAssetName)),
          resolveScriptRefs = () =>
              resolveScriptRefs(config, provider, hdAccount, network, pegOut, bridgedToken, timeout)
        )
        val stateDir = CpoTrieMirror.resolveStateDir(config.bridge.stateDir)
        // A missing history backend is NOT fatal: the mirror advances from confirm hints alone in
        // steady state. It only blocks the recovery path, and the sweeper says so when it needs it.
        val history = BlockfrostCpoHistory.fromConfig(config.cardano) match {
            case Right(h) => Some(h)
            case Left(err) =>
                Console.logWarn(
                  s"POR sweeper: no chain-history backend ($err) — cold-start reconstruction " +
                      "will not be available"
                )
                None
        }
        Console.info("POR sweeper", s"peg_out ${pegOut.policyId.toHex}, state $stateDir")
        new PorSweeper(provider, hdAccount, ctx, stateDir, history, notifier, dryRun, timeout)
    }.toEither.left.map(e => s"${e.getClass.getSimpleName}: ${e.getMessage}")

    /** Discover the CIP-33 reference UTxOs for `peg_out` and `bridged_token` in the sponsor wallet.
      *
      * Best-effort by design: a missing reference just means the script is inlined in the witness
      * set, which costs transaction size and nothing else. Never throws — a discovery failure must
      * not cost the completion itself.
      */
    private def resolveScriptRefs(
        config: BinocularConfig,
        provider: BlockchainProvider,
        hdAccount: HdAccount,
        network: Network,
        pegOut: PegOutContract,
        bridgedToken: BridgedTokenContract,
        timeout: Duration
    )(using ExecutionContext): PegOutCompleteTx.ScriptRefs =
        Try {
            val sponsorAddress = hdAccount.baseAddress(network)
            val refByHash =
                CommandHelpers.refScriptUtxosByHash(config, sponsorAddress.encode.getOrElse(""))
            // The provider drops `scriptRef` when listing UTxOs, so a reference UTxO fetched back
            // has to be re-enriched with the script it carries or TxBuilder cannot attach it.
            def refUtxo(script: Script.PlutusV3): Option[Utxo] =
                refByHash.get(script.scriptHash).flatMap { in =>
                    provider.findUtxo(in).await(timeout).toOption.map { u =>
                        val enriched = u.output match {
                            case b: TransactionOutput.Babbage =>
                                b.copy(scriptRef = Some(ScriptRef(script)))
                            case s: TransactionOutput.Shelley =>
                                TransactionOutput.Babbage(
                                  s.address,
                                  s.value,
                                  datumOption = None,
                                  scriptRef = Some(ScriptRef(script))
                                )
                        }
                        Utxo(u.input, enriched)
                    }
                }
            PegOutCompleteTx.ScriptRefs(
              pegOut = refUtxo(pegOut.script),
              bridgedToken = refUtxo(bridgedToken.script)
            )
        }.getOrElse(PegOutCompleteTx.ScriptRefs(None, None))
}
