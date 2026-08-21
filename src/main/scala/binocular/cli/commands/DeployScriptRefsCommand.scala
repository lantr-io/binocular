package binocular.cli.commands

import binocular.*
import binocular.watchtower.*
import binocular.cli.{Command, Console}

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Coin, Credential, Script, ScriptHash, ScriptRef, Transaction, TransactionHash, TransactionInput, TransactionOutput, Value}
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

/** Publishes every heavy Plutus script the bridge's transactions would otherwise inline, as
  * reference UTxOs.
  *
  *   - completion half (6): peg-in side (`peg_in`, `bridged_token`, `completed_peg_ins`), peg-out
  *     side (`peg_out`), the `bridge_state` singleton validator (spent every TM Confirm), and the
  *     `treasury_movement` validator. `bridged_token` is shared by both burns/mints.
  *   - `treasury_movement` is published for a DIFFERENT reason from the rest, and must stay
  *     published even though nothing here spends it: heimdall needs the compiled script to mint the
  *     TM NFT, and it now sources it FROM THE CHAIN by the hash the Config publishes (#5) rather
  *     than from an operator-pasted `tm_script_cbor`. A script exists on chain only once something
  *     uses it, so without this output the very first movement could never be posted — the script
  *     would be needed to make the transaction that would put it there. Publishing it at deployment
  *     breaks that circle.
  *   - federation half (5): `spos_registry`, `spo_bans` and the three DKG fault verifiers. These
  *     need `bridge.federation-one-shot-ref`; without it the command publishes the completion half
  *     and says so. `register_spo` would otherwise carry the registry script twice.
  *
  * Each script gets pinned to a Babbage-era output with `script_ref` set. Once these outputs land
  * on chain, pegin-/pegout-complete pass their outRefs as reference inputs and drop the inlined
  * script bytes from their witness sets — bringing each tx well under Cardano's 16 KB max-tx-size
  * limit (we hit 21 KB without this).
  *
  * The outputs live at [[binocular.cli.CommandHelpers.refScriptHoldingAddress]], the enterprise
  * address of the sponsor's `sig(paymentKeyHash)` native script, NOT at the wallet address. Being
  * at a script address makes them invisible to TxBuilder's fee/change coin selection, which cannot
  * spend a script UTxO, so a 50 ADA ref can never be pulled in as a fee input and destroyed (the
  * FeeTooSmallUTxO class of failures). They stay reclaimable if the bridge is ever decommissioned:
  * the wallet key alone satisfies the native script. A reference input only requires the UTxO to
  * exist, not for it to be at any particular address. Prints the resulting outpoints so they can go
  * into the bridge config.
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

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)

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
        if cfg.bridgeStateOneShotRef.forall(_.trim.isEmpty) then {
            Console.error(
              "bridge.bridge-state-one-shot-ref is not set — run deploy-bridge first"
            )
            break(1)
        }
        val bssRefInput =
            parseRef("bridge-state-one-shot-ref", cfg.bridgeStateOneShotRef.get)
        val bssOneShotRef = TxOutRef(TxId(bssRefInput.transactionId), bssRefInput.index)

        // Re-derive the 5 scripts the completion paths need (peg-in: peg_in, bridged_token,
        // completed_peg_ins; peg-out: peg_out; confirm: bridge_state) — same constructor invocations
        // DeployBridgeCommand uses, so the hashes line up exactly. (bridged_token is shared.)
        // Blueprint script() — must match DeployBridgeCommand and the watchtower exactly.
        val tmScript =
            TreasuryMovementContract.script(oraclePolicyId, configNftPolicy, configNftAsset)
        val tmNftPolicy = ByteString.fromArray(tmScript.scriptHash.bytes)
        // Rev 5.4: peg_in dropped its tm_nft_policy_id param; tmNftPolicy parameterizes bridge_state.
        val pegIn =
            PegInContract(blueprint, oraclePolicyId, configNftPolicy)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy)
        val cpi =
            CompletedPegInsContract(blueprint, configNftPolicy, cpiOneShotRef)
        val pegOut = PegOutContract(blueprint, configNftPolicy)
        val bss = BridgeStateContract(blueprint, tmNftPolicy, bssOneShotRef)

        Console.info("peg_in script hash", pegIn.policyId.toHex)
        Console.info("bridged_token script hash", bridgedToken.policyId.toHex)
        Console.info("completed_peg_ins script hash", cpi.policyId.toHex)
        Console.info("peg_out script hash", pegOut.policyId.toHex)
        Console.info("bridge_state script hash", bss.policyId.toHex)
        Console.info("treasury_movement script hash", tmScript.scriptHash.toHex)
        println()

        // --- the federation half ---
        //
        // `register_spo` would otherwise carry the ~6.7 kB registry script twice and miss the 16 kB
        // limit, and `apply-ban` carries spo_bans plus a fault verifier. Their scripts cannot be
        // rebuilt from a policy id, so this needs the federation one-shot — which the Config now
        // publishes at #12, alongside the ban schedule (an INPUT to the ban policy id) it already
        // published. Both come from the deployed Config, so this half is no longer conditional on
        // an operator having set a local key: it used to default to OFF, which meant a default
        // deployment published no registry reference script at all and every SPO deployed their
        // own copy.
        val federationScripts: List[(String, Script.PlutusV3)] = {
            val configAddress =
                Address(
                  network,
                  Credential.ScriptHash(ScriptHash.fromHex(cfg.configNftPolicyId))
                )
            val (_, deployed) = BridgeSweepSetup
                .loadConfig(
                  provider,
                  configAddress,
                  ScriptHash.fromHex(cfg.configNftPolicyId),
                  AssetName(configNftAsset),
                  timeout
                )
                .valueOr { err =>
                    Console.error(err); break(1)
                }
            val fedInput = deployed.federationOneShot
            // The local key is retired but still parsed, so a stale one is caught rather
            // than ignored: it used to be the only source, and silently preferring the
            // chain over a value someone deliberately typed would hide a real
            // disagreement about which bridge this is.
            config.bridge.federationOneShotRef.map(_.trim).filter(_.nonEmpty).foreach { refStr =>
                val local = parseRef("bridge.federation-one-shot-ref", refStr)
                val same = local.transactionId.bytes.sameElements(
                  fedInput.id.hash.bytes
                ) && BigInt(local.index) == fedInput.idx
                if !same then {
                    Console.error(
                      s"bridge.federation-one-shot-ref = $refStr disagrees with the " +
                          s"deployed Config #12 = ${fedInput.id.hash.toHex}#${fedInput.idx}. " +
                          "The Config is authoritative; unset the local key."
                    )
                    break(1)
                }
            }
            val federation = FederationScripts.derive(
              blueprint,
              fedInput.id.hash,
              fedInput.idx,
              configNftPolicy,
              (
                deployed.params.baseBanDurationMs,
                deployed.params.maxFaultsBeforePermanent,
                deployed.params.maxValidityWindowMs
              )
            )
            FederationScripts
                .verifyAgainstConfig(federation, deployed)
                .valueOr { err =>
                    Console.error(err); break(1)
                }
            Console.info("spos_registry script hash", federation.registry.policyId.toHex)
            Console.info("spo_bans script hash", federation.bans.policyId.toHex)
            Console.info("(verified against the deployed Config)", "#8 / #9 / #10 / #12")
            println()
            // treasury_info is NOT published: nothing ever spends it with the script
            // inlined at size — its spend paths are small, and the state UTxO is read as a
            // reference input everywhere else.
            ("spos_registry", federation.registry.script) ::
                ("spo_bans", federation.bans.script) ::
                FaultVerifierContract.Titles.zipWithIndex.map { case (title, i) =>
                    val label =
                        List("fault_round1", "fault_round2", "fault_equivocation")(i)
                    (
                      label,
                      FaultVerifierContract(
                        blueprint,
                        title,
                        ByteString.fromArray(federation.registry.policyId.bytes)
                      ).script
                    )
                }
        }

        if dryRun then {
            Console.success("Dry-run complete (computed hashes, not submitting)")
            break(0)
        }

        val signer = setup.signer

        // 50 ADA per output: generously above the minUTxO formula for a ~13 KB script-bearing
        // output. The excess comes back as change when the ref UTxO is ever spent.
        val baseAda = Coin(50_000_000L)
        // Refs are parked at the sponsor's native-script holding address, not the wallet address:
        // coin selection cannot spend a script UTxO, so a ref can never be consumed as a fee input.
        // Derived from the sponsor key, so discovery recomputes the same address without config.
        val refHoldingAddress =
            binocular.cli.CommandHelpers.refScriptHoldingAddress(network, sponsorAddress)
        def refOutput(script: Script.PlutusV3): TransactionOutput =
            TransactionOutput.Babbage(
              refHoldingAddress,
              Value(baseAda),
              datumOption = None,
              scriptRef = Some(ScriptRef(script))
            )

        def submitOne(label: String, output: TransactionOutput): Option[(String, Int)] = {
            Console.step(0, s"Publishing $label reference script")
            // BlockfrostProvider drops `scriptRef` from returned UTxOs, so coin selection can pick a
            // reference-script UTxO (e.g. a leftover deploy from a prior run) and under-estimate the
            // Conway reference-script fee → FeeTooSmallUTxO; spending it would also destroy a deployed
            // ref script. Exclude every ref-script UTxO at either scanned address from selection
            // and pass the filtered set to the sync `complete`. Recomputed per tx so refs published
            // earlier in THIS run are excluded too (their UTxOs are indexed before the next submit).
            // Same fix as OracleTransactions.buildOptimalUpdateTransaction's `excludeInputs`.
            val excludeInputs = binocular.cli.CommandHelpers.refScriptOutpoints(
              config,
              binocular.cli.CommandHelpers.refScriptScanAddresses(config, network, sponsorAddress)
            )
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
                            // Same convention as DeployBridgeCommand:200-215. Wait on the HOLDING
                            // address: that is where the ref output lands, and seeing it there
                            // proves the block was applied to the UTxO index (so the sponsor's
                            // change is visible too) and that the next iteration's exclusion
                            // and already-deployed scans will see this ref.
                            binocular.oracle.OracleTransactions
                                .waitForUtxoAtAddress(
                                  provider,
                                  refHoldingAddress,
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

        // Each entry: (label, script). Skip any whose script hash already has a reference UTxO at
        // the holding address or the sponsor wallet, so re-running (e.g. to add the peg-out side
        // after the peg-in side) doesn't re-publish — and waste ~50 ADA on — refs that already
        // exist. Scanning both means a pre-migration ref still at the wallet also counts as
        // deployed. Discovery is on-chain (BlockfrostProvider drops scriptRef, so scan
        // `reference_script_hash` directly), not config.
        val candidates = List(
          ("peg_in", pegIn.script),
          ("bridged_token", bridgedToken.script),
          ("completed_peg_ins", cpi.script),
          ("peg_out", pegOut.script),
          ("bridge_state", bss.script),
          ("treasury_movement", tmScript)
        ) ++ federationScripts
        val deployedHashes = binocular.cli.CommandHelpers
            .refScriptUtxosByHash(
              config,
              binocular.cli.CommandHelpers.refScriptScanAddresses(config, network, sponsorAddress)
            )
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
