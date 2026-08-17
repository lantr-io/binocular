package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.ledger.{AssetName, TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Mint a fresh bridge-state singleton against a LIVE bridge — the executable half of the
  * ConfigDatum field-3 swap, and the §Recovery replacement path.
  *
  * The singleton's `"BSS"` NFT can only be created by `bridge-state.ak`'s own mint branch, which
  * consumes a one-shot UTxO ([BSS-4]) and pays the token to the policy's own script address
  * ([BSS-5]). The validator takes `(tm_nft_policy_id, one_shot_input_ref)`; the TM script hash
  * comes from the live bridge (oracle hash + the deployed config NFT), so the only free parameter
  * is the one-shot, and a fresh one-shot yields a fresh policy with a fresh singleton.
  *
  * The bootstrap [[BridgeState]] datum is deliberately NOT pinned on-chain (spec §Why the bootstrap
  * datum is not pinned): the same mint path serves the FIRST deployment — zero roots and the
  * deployment anchor — and the §Recovery replacement — the current roots and the live tip. It is
  * operator-supplied here and observer-verified: the honest roots are a deterministic function of
  * chain history, so a wrong one is detectable, and being attested rather than folded it is
  * overwritten by the next honest Confirm.
  *
  * Migration order (the rest is the runbook's):
  *   1. run this command; it prints the new policy id and the one-shot ref;
  *   2. run `update-config --bridge-state-policy <policy>` (together with any dependent field
  *      swaps) — ONE transaction, all fields together;
  *   3. set `bridge.bridge-state-one-shot-ref` to the printed ref, so `confirm-tmtx` derives the
  *      same script;
  *   4. restart the watchtower.
  *
  * Between steps 1 and 2 nothing is live: the config still publishes the old policy, so every
  * reader still looks for the old singleton and the freshly minted one is simply idle.
  */
case class BootstrapBridgeStateCommand(
    oneShotRef: Option[String] = None,
    // Override the anchor (TXID:VOUT) / amount from bridge.initial-btc-treasury-*: a §Recovery
    // replacement anchors at the live tip, not at the deployment anchor.
    anchor: Option[String] = None,
    amountSat: Option[Long] = None,
    // Non-zero roots for a §Recovery replacement of a bridge with history. Hex, 32 bytes each.
    spiRoot: Option[String] = None,
    cpoRoot: Option[String] = None,
    // [DEP-2] escape hatch: skip the gettxout verification of the anchor. The datum is unpinned
    // on-chain, so a wrong outpoint or amount surfaces only at first-movement signing — weeks
    // later, as an invalid FROST signature. Only for a bridge whose Bitcoin node is unreachable
    // AND whose anchor was verified by hand.
    skipBtcCheck: Boolean = false,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Bootstrap Bridge-State Singleton (config field 3)")
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
        // as confirm-tmtx and deploy-bridge derive it, so all three agree on the singleton policy.
        val tmNftPolicy = CommandHelpers.tmNftPolicy(config, setup.script.scriptHash)
        Console.info("TM NFT policy", tmNftPolicy.toHex)

        // The operator-supplied bootstrap state (spec §Why the bootstrap datum is not pinned).
        def rootArg(flag: String, value: Option[String]): ByteString =
            value.map(_.trim).filter(_.nonEmpty) match {
                case None => BridgeBootstrap.EmptyRoot
                case Some(h)
                    if h.length == 64 && h.forall(c => "0123456789abcdefABCDEF".contains(c)) =>
                    ByteString.fromHex(h)
                case Some(h) =>
                    Console.error(s"$flag must be 64 hex chars (a 32-byte MPF root), got '$h'")
                    break(1)
            }
        val anchorDisplay = anchor
            .map(_.trim)
            .filter(_.nonEmpty)
            .orElse(Option(config.bridge.initialBtcTreasuryUtxo.trim).filter(_.nonEmpty))
            .getOrElse {
                Console.error(
                  "no anchor: pass --anchor TXID:VOUT or set bridge.initial-btc-treasury-utxo"
                )
                break(1)
            }
        val anchorOutpoint =
            try BridgeConfig.outpointFromDisplay(anchorDisplay)
            catch { case e: Exception => Console.error(s"anchor: ${e.getMessage}"); break(1) }
        val stateAmountSat = amountSat.getOrElse(config.bridge.initialBtcTreasuryAmountSat)
        val state = BridgeState(
          spiRoot = rootArg("--spi-root", spiRoot),
          cpoRoot = rootArg("--cpo-root", cpoRoot),
          treasuryUtxoId = anchorOutpoint,
          treasuryAmount = BigInt(stateAmountSat)
        )

        // [DEP-2]: verify the anchor outpoint and its satoshi amount against Bitcoin BEFORE the
        // bootstrap. The datum is deliberately unpinned on-chain, so this command is the last
        // place a typoed vout, a wrong amount, or an already-spent anchor fails loudly — after
        // it, the mistake surfaces only when the first TM's BIP-341 sighash commits to the wrong
        // prevout and every FROST signature the roster produces is invalid.
        if skipBtcCheck then
            Console.warn(
              "[DEP-2] --skip-btc-check: the anchor was NOT verified against Bitcoin. A wrong " +
                  "outpoint or amount makes every signature over the first TM invalid."
            )
        else {
            val Array(anchorTxid, anchorVoutStr) = anchorDisplay.split(':')
            val anchorVout = anchorVoutStr.toInt
            val rpc = new binocular.bitcoin.SimpleBitcoinRpc(config.bitcoinNode)
            val verified =
                try rpc.getTxOutValueSat(anchorTxid, anchorVout).await(timeout)
                catch {
                    case e: Exception =>
                        Console.error(
                          s"[DEP-2] cannot verify the anchor against Bitcoin (gettxout " +
                              s"$anchorDisplay failed: ${e.getMessage}). Fix the Bitcoin node " +
                              "configuration, or pass --skip-btc-check after verifying the " +
                              "anchor by hand."
                        )
                        break(1)
                }
            verified match {
                case None =>
                    Console.error(
                      s"[DEP-2] the anchor $anchorDisplay is not an unspent output on Bitcoin " +
                          "(spent, or never existed). A singleton anchored to it can never " +
                          "advance: no TM spending it can be built."
                    )
                    break(1)
                case Some(sat) if sat != stateAmountSat =>
                    Console.error(
                      s"[DEP-2] the anchor $anchorDisplay holds $sat sat on Bitcoin, but the " +
                          s"bootstrap datum would record $stateAmountSat sat. The first TM's " +
                          "sighash would commit to the wrong prevout amount and every FROST " +
                          "signature over it would be invalid. Pass --amount-sat $sat (or fix " +
                          "bridge.initial-btc-treasury-amount-sat)."
                    )
                    break(1)
                case Some(sat) =>
                    Console.info("[DEP-2] anchor verified", s"$anchorDisplay = $sat sat (unspent)")
            }
        }

        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }

        // An explicit --one-shot-ref must still be an UNSPENT wallet UTxO: the mint handler checks
        // the outref is among the tx inputs ([BSS-4]), so a spent or foreign ref can never satisfy
        // it.
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
                val excluded = CommandHelpers.refScriptOutpoints(
                  config,
                  CommandHelpers.refScriptScanAddresses(config, network, sponsorAddress)
                )
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

        val bssContract = BridgeStateContract(blueprint, tmNftPolicy, oneShot)
        val (bssAddress, bssValue, bssDatum) =
            BridgeBootstrap.bridgeStateOutput(bssContract, network, state)

        Console.info("one-shot", oneShotDisplay)
        Console.info("bridge-state policy", bssContract.policyId.toHex)
        Console.info("bridge-state asset", BridgeStateContract.assetName.toHex)
        Console.info("bridge-state address", bssAddress.encode.getOrElse("?"))
        Console.info("spi_root", state.spiRoot.toHex)
        Console.info("cpo_root", state.cpoRoot.toHex)
        Console.info("head (anchor)", anchorDisplay)
        Console.info("amount (sat)", state.treasuryAmount.toString)
        println()

        if dryRun then {
            Console.success("Dry-run complete (computed the singleton policy; not submitting)")
            printNextSteps(bssContract.policyId.toHex, oneShotDisplay)
            break(0)
        }

        Console.step(1, "Minting the bridge-state singleton NFT")
        val tx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(oneShotUtxo)
                    .mint(
                      bssContract.script,
                      Map(AssetName(BridgeStateContract.assetName) -> 1L),
                      Data.unit
                    )
                    .payTo(bssAddress, bssValue, bssDatum)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(setup.hdAccount.signerForUtxos)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building the singleton bootstrap tx: ${e.getMessage}")
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
                Console.success(s"Bridge-state singleton bootstrapped: $txHash")
                println()
                printNextSteps(bssContract.policyId.toHex, oneShotDisplay)
                0
            case other =>
                Console.error(s"Not confirmed: $other")
                1
        }
    }

    /** Print the exact follow-up commands, so the migration cannot be half-applied by guesswork. */
    private def printNextSteps(policyHex: String, oneShotDisplay: String): Unit = {
        Console.separator()
        Console.info("bridge-state-policy-id", policyHex)
        Console.info("bridge-state-one-shot-ref", oneShotDisplay)
        Console.info(
          "next 1",
          s"binocular update-config --bridge-state-policy $policyHex (plus any dependent field swaps)"
        )
        Console.info(
          "next 2",
          s"set binocular.bridge.bridge-state-one-shot-ref = $oneShotDisplay, then restart the " +
              "watchtower"
        )
        Console.separator()
    }
}
