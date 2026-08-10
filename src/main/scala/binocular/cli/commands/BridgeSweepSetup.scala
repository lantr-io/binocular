package binocular.cli.commands

import binocular.BinocularConfig
import binocular.cli.{CommandHelpers, Console}
import binocular.notify.Notifier
import binocular.watchtower.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Try
import scalus.utils.await

/** Wiring shared by the two commands that touch the completed-peg-outs trie: `confirm-tmtx` (which
  * SPENDS and recreates the singleton) and `peg-out-complete` (which REFERENCES it).
  *
  * It lives outside both so the manual completion command and the watchtower's automatic sweeper
  * resolve the same UTxOs, derive the same scripts, and fail with the same messages. A divergence
  * between them would show up as "the sweeper works but the manual command does not", which is
  * exactly the kind of drift an operator cannot debug.
  */
object BridgeSweepSetup {

    /** Everything a Confirm or Complete transaction needs from the completed-peg-outs trie.
      *
      * @param configUtxo
      *   the Config UTxO, always a reference input; validators read the trie policy from its field
      *   3.
      * @param trieUtxo
      *   the UTxO carrying the `"CPO"` NFT. Confirm SPENDS and recreates it; Complete REFERENCES
      *   it.
      * @param trieScript
      *   the trie validator, needed to spend `trieUtxo`. Its hash equals Config field 3.
      * @param currentRoot
      *   the root in `trieUtxo`'s datum.
      */
    final case class TrieContext(
        configUtxo: Utxo,
        trieUtxo: Utxo,
        trieScript: Script.PlutusV3,
        currentRoot: ByteString
    )

    /** Locate the Config and completed-peg-outs trie UTxOs.
      *
      * The trie policy is taken from Config field 3 (rev 5.4: `bridge_state_policy`, which INTERIM
      * still carries the trie policy — see the TM validator's Confirm branch). A RAW field read,
      * not a typed `ConfigDatum` decode, so a deployed datum with a different field count still
      * works. The locally derived trie script must hash to that policy, otherwise the config still
      * publishes the pre-migration trie and the migration Update has not run yet.
      */
    def loadTrieContext(
        provider: BlockchainProvider,
        configAddress: Address,
        configNftPolicy: ScriptHash,
        configNftAsset: AssetName,
        trieScript: Script.PlutusV3,
        trieAssetName: AssetName,
        network: Network,
        timeout: Duration
    )(using ExecutionContext): Either[String, TrieContext] =
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
            triePolicyBytes <- configUtxo.output.inlineDatum match {
                case Some(Data.Constr(0, fields)) =>
                    fields.asScala.toList.lift(3) match {
                        case Some(Data.B(p)) => Right(p)
                        case other           => Left(s"config field 3 is not a byte string: $other")
                    }
                case other => Left(s"config datum is not a Constr 0 inline datum: $other")
            }
            _ <- Either.cond(
              triePolicyBytes.toHex == trieScript.scriptHash.toHex,
              (),
              s"config field 3 publishes trie policy ${triePolicyBytes.toHex}, but the trie " +
                  s"validator derived from (TM hash, one-shot) hashes to " +
                  s"${trieScript.scriptHash.toHex}. Run `update-config " +
                  s"--completed-peg-outs-policy ${trieScript.scriptHash.toHex}` (together with the " +
                  "field 4/5 swaps) before confirming under the new TM script."
            )
            trieAddress = Address(network, Credential.ScriptHash(trieScript.scriptHash))
            trieUtxos <- provider
                .findUtxos(trieAddress)
                .await(timeout)
                .left
                .map(err => s"fetching trie UTxOs at $trieAddress: $err")
            trieUtxo <- trieUtxos.toList
                .collectFirst {
                    case (in, out) if out.value.hasAsset(trieScript.scriptHash, trieAssetName) =>
                        Utxo(in, out)
                }
                .toRight(
                  s"no UTxO carrying the \"CPO\" NFT at $trieAddress — the trie has not been " +
                      "bootstrapped under this policy"
                )
            onChainRoot <- trieUtxo.output.inlineDatum
                .flatMap(d => Try(d.to[CompletedPegOutsTrieDatum].root).toOption)
                .toRight("the trie UTxO has no decodable inline root datum")
        } yield TrieContext(configUtxo, trieUtxo, trieScript, onChainRoot)

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
        val pegOut = PegOutContract(blueprint, configNftPolicyBs, configNftAssetBs)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicyBs, configNftAssetBs)
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
