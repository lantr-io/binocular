package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.{AssetName, Transaction, TransactionHash, TransactionInput, Utxo, Value}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.prelude.Option as POption

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** F3: deploy the ft-bifrost-bridge completion contracts on Cardano.
  *
  * Bootstraps the whole bridge in ONE tx that spends a single one-shot wallet UTxO and creates
  * every protocol UTxO at once:
  *   1. the **config NFT** (`config.ak`) carrying the [[ConfigDatum]] – the spine that records
  *      every cross-referenced script hash;
  *   2. the **completed-peg-ins MPF NFT** (`completed-peg-ins-merkle-tree.ak`) with an empty root;
  *   3. the **completed-peg-outs MPF NFT** (`completed-peg-outs-merkle-tree.ak`) with an empty
  *      root.
  *
  * Every one of those policies is parameterized by the SAME one-shot outref, so they get distinct
  * policy ids while all mint handlers see the single UTxO consumed in this tx. The bridged-token
  * (`bridged_token`) policy has no state UTxO – its hash is recorded in the config datum (index 0).
  * All hashes are computed deterministically from the one-shot + the live oracle policy (see
  * [[BifrostContracts]]). After deploy, set `binocular.bridge.{config-nft-*, bridged-token-*}` to
  * the printed values and re-mint the PegInRequests so the peg_in policy matches.
  *
  * Config layout (rev 5.4, eight fields — spec §Config datum): 0 `update_auth` (the binocular owner
  * key `oracle.owner-pkh`, which may Update/Retire the config per config.ak's spend handler), 1
  * `bridged_token_policy`, 2 `completed_peg_ins_policy`, 3 `bridge_state_policy`, 4
  * `tm_script_hash` (spec [CFG-2]: published for off-chain readers, no on-chain reader), 5/6
  * (peg_in / peg_out script hashes) and 7 `params` (the nested tunables record). The bridged-token
  * asset name is the [CFG-1] constant "fSAT", not a field. Minting a new config NFT changes the
  * bridged-token policy, so re-mint under this config. The cpi/cpo NFT asset names are the
  * constants "CPI"/"CPO". The TM validator is parameterized by (oracle hash, config NFT policy,
  * config NFT asset), so its address derives from this deploy's config NFT — no TM-control NFT
  * exists anymore; TM minting is permissionless, gated by chain linkage (see [[TmMintRedeemer]]).
  *
  * Derivation ORDER matters (peg-out trie v2, 2026-07). The completed-peg-outs validator is
  * parameterized by `(tm_nft_policy_id, one_shot_ref)`, so the chain is: one-shot -> config policy
  * -> TM script hash -> completed-peg-outs policy -> ConfigDatum field 3. The genesis config
  * therefore publishes the REWRITTEN trie validator's hash, and the trie UTxO it bootstraps is
  * spendable only inside a TM `Unconfirmed -> Confirmed` transition.
  *
  * INTERIM (rev 5.4, pre-singleton-migration): field 3 (`bridge_state_policy`) still carries the
  * completed-peg-outs trie policy this deploy bootstraps, because the interim TM Confirm flow
  * locates the trie through it. TODO(bridge-state migration): bootstrap `bridge-state.ak` and write
  * the real singleton policy here.
  */
case class DeployBridgeCommand(dryRun: Boolean = false) extends Command {

    // Config NFT asset name (arbitrary; recorded as config asset name).
    private val ConfigAssetName: ByteString = ByteString.fromString("BIFCFG")
    // Bridged-token asset name — the [CFG-1] protocol constant, not a config field.
    private val BridgedTokenAssetName: ByteString = ConfigDatum.BridgedTokenAssetName
    private val EmptyRoot: ByteString = BridgeBootstrap.EmptyRoot

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Deploy Bifrost Bridge Contracts (F3)")
        if dryRun then Console.warn("Dry-run mode — will compute hashes but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val sponsorAddress = setup.sponsorAddress
        val oraclePolicyId = setup.script.scriptHash

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)

        def refOf(u: Utxo): TxOutRef =
            TxOutRef(TxId(u.input.transactionId), u.input.index)

        // --- pick one clean pure-ADA one-shot UTxO (shared by config + cpi + cpo + TM-control) ---
        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }
        // A reference-script UTxO (CIP-33) is pure-lovelace with no native assets, so it looks
        // identical to a plain change UTxO to the filter below — but spending one DESTROYS a deployed
        // reference script, and (because BlockfrostProvider drops `scriptRef` on findUtxos) the tx
        // builder under-estimates the fee by the Conway reference-script surcharge → the mint tx is
        // rejected with FeeTooSmallUTxO. Exclude every ref-script UTxO — discovered on-chain via the
        // shared address-utxos `reference_script_hash` scan. The one-shot from a prior deploy is
        // excluded too, in case a stale (now-spent) copy still lingers in the wallet index.
        val staleOneShot: Option[TransactionInput] =
            config.bridge.completedPegInsOneShotRef.trim.split("#") match {
                case Array(h, i) if i.toIntOption.isDefined =>
                    Some(TransactionInput(TransactionHash.fromHex(h), i.toInt))
                case _ => None
            }
        val excludedInputs: Set[TransactionInput] =
            CommandHelpers.refScriptOutpoints(config, sponsorAddress.encode.getOrElse("")) ++
                staleOneShot

        val signer = setup.hdAccount.signerForUtxos

        // The whole bridge is bootstrapped in ONE tx that spends a single one-shot UTxO. Every
        // protocol NFT policy (config, cpi, cpo, tm-control) is parameterized by that same outref,
        // so they get distinct policy ids while all their mint handlers see the one UTxO consumed.
        // Selection rule shared with `bootstrap-completed-peg-outs`.
        val oneShotUtxo = BridgeBootstrap.pickOneShot(walletUtxos, excludedInputs).getOrElse {
            Console.error(
              "No clean pure-ADA wallet UTxO (>=5 ADA, excluding reference-script UTxOs) for the " +
                  "bridge one-shot; fund the sponsor wallet"
            )
            break(1)
        }
        val oneShotRef = refOf(oneShotUtxo)
        val configRef = oneShotRef
        val cpiRef = oneShotRef
        val cpoRef = oneShotRef

        // --- compute the deterministic hash chain ---
        val configContract =
            ConfigContract(blueprint, configRef.id.hash, configRef.idx, ConfigAssetName)
        val configPolicy = configContract.policyId

        val bridgedToken = BridgedTokenContract(blueprint, configPolicy, ConfigAssetName)
        val bridgedTokenPolicy = bridgedToken.policyId

        val cpiContract =
            CompletedPegInsContract(blueprint, configPolicy, ConfigAssetName, cpiRef)
        val cpiPolicy = cpiContract.policyId
        val cpiAssetName = CompletedPegInsContract.assetName

        // TM-NFT policy = the TreasuryMovementValidator script hash (oracle hash + the config NFT
        // minted in this same deploy tx). peg_in.ak references the Confirmed TM UTxO by this NFT
        // (its 4th param), so the peg_in hash depends on it.
        // Derived from the blueprint script() — the SAME path the watchtower/relay/confirm
        // use. The SIR-applied contract() hashes differently (params applied pre-optimization),
        // so using it here would split the system across two TM script hashes.
        val tmNftPolicy = ByteString.fromArray(
          TreasuryMovementContract
              .script(oraclePolicyId, configPolicy, ConfigAssetName)
              .scriptHash
              .bytes
        )

        // Rev 5.4: peg_in no longer takes the TM NFT policy — it reads the singleton through
        // Config field 3 at runtime. `tmNftPolicy` still parameterizes the trie below and is
        // published as Config field 4 (`tm_script_hash`, spec [CFG-2]).
        val pegIn = PegInContract(blueprint, oraclePolicyId, configPolicy, ConfigAssetName)
        val pegInWithdrawHash = pegIn.policyId

        // --- peg-out side (config index 6 = peg_out script hash) ---
        val pegOut = PegOutContract(blueprint, configPolicy, ConfigAssetName)
        val pegOutWithdrawHash = pegOut.policyId

        // Trie v2: the completed-peg-outs validator takes the TM NFT policy, not the config NFT
        // pair, so it MUST be derived after `tmNftPolicy` above. Its own policy is written into
        // config field 3 below, which is where the TM validator reads it back at Confirm — the
        // link closes at runtime, not at compile time, so the two parameterizations do not cycle.
        val cpoContract = CompletedPegOutsContract(blueprint, tmNftPolicy, cpoRef)
        val cpoPolicy = cpoContract.policyId
        val cpoAssetName = CompletedPegOutsContract.assetName

        // config Update/Retire authority = the binocular owner key (oracle.owner-pkh),
        // so the same operator that runs the oracle governs the bridge config.
        val updateAuthPkh = {
            val s = config.oracle.ownerPkh
            if s.length == 56 && s.forall(c => "0123456789abcdefABCDEF".contains(c)) then
                ByteString.fromHex(s)
            else {
                Console.error(
                  "oracle.owner-pkh must be a 28-byte (56 hex) pubkey hash for config update_auth " +
                      "(set ORACLE_OWNER_PKH or the owner-pkh in your preprod conf)"
                )
                break(1)
            }
        }

        val configDatum = ConfigDatum(
          // Governance: the binocular owner key (oracle.owner-pkh) may Update/Retire
          // the config (progressive decentralization rotates this via a later update).
          updateAuth = POption.Some(
            AuthorizationMethod.CardanoSignature(updateAuthPkh)
          ),
          bridgedTokenPolicy = bridgedTokenPolicy,
          completedPegInsPolicy = cpiPolicy,
          // INTERIM: the completed-peg-outs trie policy, because the pre-migration TM Confirm
          // locates the trie through this field. TODO(bridge-state migration): the bridge-state
          // singleton policy.
          bridgeStatePolicy = cpoPolicy,
          // spec [CFG-2]: published so off-chain readers can locate the TM address; no on-chain
          // reader.
          tmScriptHash = tmNftPolicy,
          pegInScriptHash = pegInWithdrawHash,
          pegOutScriptHash = pegOutWithdrawHash,
          // Operational-parameter tunables (config field 7, nested; off-chain readers only).
          params = ConfigParams(
            feeRateSatPerVb = BigInt(config.bridge.feeRateSatPerVb),
            perPegoutFee = BigInt(config.bridge.perPegoutFeeSat),
            minPegOutFbtc = BigInt(config.bridge.minPegOutSat),
            // Devnet-scale schedule defaults (spec §TM batches — governance replaces the record
            // wholesale, effective next epoch, so creation values only need to be sane).
            schedule = ScheduleParams(
              dkgR1Deadline = BigInt(3600),
              dkgR2Deadline = BigInt(7200),
              updateYDeadline = BigInt(10800),
              tmBatchInterval = BigInt(21600),
              signR1Window = BigInt(1800),
              signR2Window = BigInt(1800),
              leaderSlotT = BigInt(600),
              tmRecoveryWindow = BigInt(129600),
              finalTmCutoff = BigInt(345600),
              stabilityWindow = BigInt(129600)
            )
          )
        )

        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("config one-shot", s"${configRef.id.hash.toHex}#${configRef.idx}")
        Console.info("cpi one-shot", s"${cpiRef.id.hash.toHex}#${cpiRef.idx}")
        println()
        Console.info("config NFT policy", configPolicy.toHex)
        Console.info("config NFT asset", ConfigAssetName.toHex)
        Console.info("bridged_token policy", bridgedTokenPolicy.toHex)
        Console.info("bridged_token asset", BridgedTokenAssetName.toHex)
        Console.info("completed-peg-ins policy", cpiPolicy.toHex)
        Console.info("completed-peg-ins asset", cpiAssetName.toHex)
        Console.info("cpo one-shot", s"${cpoRef.id.hash.toHex}#${cpoRef.idx}")
        Console.info("completed-peg-outs policy", cpoPolicy.toHex)
        Console.info("completed-peg-outs asset", cpoAssetName.toHex)
        Console.info("peg_in withdraw hash", pegInWithdrawHash.toHex)
        Console.info("peg_out withdraw hash", pegOutWithdrawHash.toHex)
        Console.info("TM script hash (config field 4)", tmNftPolicy.toHex)
        println()

        if dryRun then {
            Console.success(
              "Dry-run complete (computed hashes + assembled config datum; not submitting)"
            )
            break(0)
        }

        // --- Single bootstrap tx: spend the one-shot, mint all three protocol NFTs, and create
        //     every protocol UTxO in one atomic tx. config.ak::mint checks self.outputs[0] is the
        //     config UTxO, so the config output MUST be first. cpi/cpo mint handlers each find
        //     their own output by script address and see the shared one-shot consumed. ---
        Console.step(1, "Bootstrapping bridge (config + cpi + cpo) in one tx")
        val configAsset = AssetName(ConfigAssetName)
        val cpiAsset = AssetName(cpiAssetName)
        val cpoAsset = AssetName(cpoAssetName)
        val configValue =
            Value.lovelace(2_000_000L) + Value.asset(configContract.policyId, configAsset, 1L)
        val cpiValue = Value.lovelace(2_000_000L) + Value.asset(cpiContract.policyId, cpiAsset, 1L)
        val cpiDatum = CompletedPegInsMerkleTreeDatum(EmptyRoot)
        // Shared with `bootstrap-completed-peg-outs`, so a bridge deployed here and a trie re-minted
        // during the field-3 migration produce a byte-identical trie output.
        val (cpoAddress, cpoValue, cpoDatum) =
            BridgeBootstrap.completedPegOutsOutput(cpoContract, network)
        val bootstrapTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(oneShotUtxo)
                    .mint(configContract.script, Map(configAsset -> 1L), Data.unit)
                    .mint(cpiContract.script, Map(cpiAsset -> 1L), Data.unit)
                    .mint(cpoContract.script, Map(cpoAsset -> 1L), Data.unit)
                    // config UTxO first (config.ak::mint reads self.outputs[0]).
                    .payTo(configContract.address(network), configValue, configDatum.toData)
                    .payTo(cpiContract.address(network), cpiValue, cpiDatum.toData)
                    .payTo(cpoAddress, cpoValue, cpoDatum)
                    // Register the peg_in / peg_out withdraw reward accounts here (deposit-less
                    // Shelley RegCert, no script execution) so completion txs can withdraw. Both
                    // hashes are fresh per deploy (they derive from this deploy's config policy), so
                    // this is always a first-time registration - safe in an atomic tx. These two
                    // are the ONLY reward accounts the protocol uses (the rev-5.1 verifier slots
                    // are gone from the rev-5.4 datum). `register-bridge-creds` (idempotent)
                    // re-runs the same two registrations if this tx is ever partial.
                    .registerStake(StakeAddress(network, StakePayload.Script(pegIn.policyId)))
                    .registerStake(StakeAddress(network, StakePayload.Script(pegOut.policyId)))
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building bootstrap tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }
        val bootstrapTxHash = submitAndConfirm(provider, bootstrapTx, timeout)
        Console.success(s"Bridge bootstrapped in one tx: $bootstrapTxHash")
        Console.info("config address", configContract.address(network).encode.getOrElse("?"))
        Console.info(
          "completed-peg-ins address",
          cpiContract.address(network).encode.getOrElse("?")
        )
        Console.info(
          "completed-peg-outs address",
          cpoContract.address(network).encode.getOrElse("?")
        )
        println()
        Console.separator()
        Console.success(
          "Bridge deployed. Set these in binocular.bridge and re-mint the PegInRequests:"
        )
        Console.info("config-nft-policy-id", configPolicy.toHex)
        Console.info("config-nft-asset-name", ConfigAssetName.toHex)
        Console.info("bridged-token-policy-id", bridgedTokenPolicy.toHex)
        Console.info("bridged-token-asset-name", BridgedTokenAssetName.toHex)
        Console.info("completed-peg-outs-policy-id", cpoPolicy.toHex)
        Console.info("completed-peg-outs-asset-name", cpoAssetName.toHex)
        // Both completed-peg one-shot refs are the shared bootstrap one-shot; deploy-script-refs
        // requires them set before it can publish the completed-peg-ins/outs reference scripts.
        Console.info(
          "completed-peg-ins-one-shot-ref",
          s"${configRef.id.hash.toHex}#${configRef.idx}"
        )
        Console.info(
          "completed-peg-outs-one-shot-ref",
          s"${configRef.id.hash.toHex}#${configRef.idx}"
        )
        Console.info("peg-out-withdraw-hash", pegOutWithdrawHash.toHex)
        Console.info(
          "next",
          "peg_in/peg_out reward accounts registered in the bootstrap tx (the only two the " +
              "protocol uses); re-run `register-bridge-creds` only if that tx was partial"
        )
        Console.info("tm-nft-policy", tmNftPolicy.toHex)
        Console.separator()
        0
    }

    private def submitAndConfirm(
        provider: BlockchainProvider,
        tx: Transaction,
        timeout: scala.concurrent.duration.Duration
    )(using ExecutionContext, boundary.Label[Int]): String = {
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
            case TransactionStatus.Confirmed => txHash
            case other                       => Console.error(s"Not confirmed: $other"); break(1)
        }
    }
}

object DeployBridgeCommand {
    // Confirmation polling budget. The `.await` window MUST exceed the poll's own budget
    // (`attempts * delayMs`); otherwise the await preempts the poll and throws a TimeoutException at
    // the same instant the poll would have observed confirmation — a spurious failure on a tx that
    // actually confirmed (observed on preprod with the old 60×2s poll under a 120s await). Generous
    // attempts also tolerate slow preprod block production.
    val ConfirmPollAttempts: Int = 90
    val ConfirmPollDelayMs: Int = 2000
    val confirmAwait: scala.concurrent.duration.FiniteDuration =
        scala.concurrent.duration.Duration(
          ConfirmPollAttempts.toLong * ConfirmPollDelayMs + 30_000,
          scala.concurrent.duration.MILLISECONDS
        )
}
