package binocular.cli.commands

import binocular.*
import binocular.bitcoin.SimpleBitcoinRpc
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
  *   3. the **bridge-state singleton** (`bridge-state.ak`, NFT asset `"BSS"`) carrying the
  *      bootstrap [[BridgeState]]: empty roots and the deployment anchor
  *      (`bridge.initial-btc-treasury-utxo` / `-amount-sat`).
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
  * bridged-token policy, so re-mint under this config. The cpi/singleton NFT asset names are the
  * constants "CPI"/"BSS". The TM validator is parameterized by (oracle hash, config NFT policy,
  * config NFT asset), so its address derives from this deploy's config NFT — no TM-control NFT
  * exists anymore; TM minting is permissionless, gated by chain linkage (see [[TmMintRedeemer]]).
  *
  * Derivation ORDER matters. The bridge_state validator is parameterized by
  * `(tm_nft_policy_id, one_shot_ref)`, so the chain is: one-shot -> config policy -> TM script hash
  * -> bridge_state policy -> ConfigDatum field 3. The genesis config therefore publishes the
  * singleton validator's hash, and the singleton it bootstraps is spendable only inside a TM
  * Confirm ([BSS-1]/[BSS-2]).
  */
case class DeployBridgeCommand(
    // [DEP-2] escape hatch, mirroring bootstrap-bridge-state: skip the gettxout verification of
    // the deployment anchor. Only for a deployer whose Bitcoin node is unreachable AND whose
    // anchor outpoint and amount were verified by hand.
    skipBtcCheck: Boolean = false,
    dryRun: Boolean = false
) extends Command {

    // Config NFT asset name (arbitrary; recorded as config asset name).
    private val ConfigAssetName: ByteString = ByteString.fromString("BIFCFG")
    // Bridged-token asset name — the [CFG-1] protocol constant, not a config field.
    private val BridgedTokenAssetName: ByteString = ConfigDatum.BridgedTokenAssetName
    // Asset name of the treasury_info state NFT. Chosen here and published as config #13 — it is
    // derivable from nothing, so publishing it is the only way an SPO avoids typing it.
    private val TreasuryInfoAssetName: ByteString = ByteString.fromString("TMTx")
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

        // --- pick one clean pure-ADA one-shot UTxO (shared by config + cpi + bridge-state) ---
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
        // protocol NFT policy (config, cpi, bridge_state) is parameterized by that same outref,
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
        val bssRef = oneShotRef

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

        // The bridge_state validator takes the TM NFT policy, not the config NFT pair, so it MUST
        // be derived after `tmNftPolicy` above. Its own policy is written into config field 3
        // below, which is where the TM validator (and every reader) finds the singleton back at
        // runtime — the link closes at runtime, not at compile time, so the two parameterizations
        // do not cycle ([PAR-1]).
        val bssContract = BridgeStateContract(blueprint, tmNftPolicy, bssRef)
        val bssPolicy = bssContract.policyId
        val bssAssetName = BridgeStateContract.assetName

        // The bootstrap BridgeState: zero roots + the deployment anchor (spec §Why the bootstrap
        // datum is not pinned — operator-supplied, observer-verified).
        val initialTreasuryDisplay = config.bridge.initialBtcTreasuryUtxo.trim
        val anchorOutpoint = {
            if initialTreasuryDisplay.isEmpty then {
                Console.error(
                  "bridge.initial-btc-treasury-utxo must be TXID:VOUT — the deployment anchor " +
                      "written into the singleton's bootstrap BridgeState"
                )
                break(1)
            }
            try BridgeConfig.outpointFromDisplay(initialTreasuryDisplay)
            catch {
                case e: Exception =>
                    Console.error(s"bridge.initial-btc-treasury-utxo: ${e.getMessage}"); break(1)
            }
        }
        val anchorAmountSat = config.bridge.initialBtcTreasuryAmountSat

        // [DEP-2]: verify the anchor against Bitcoin BEFORE writing it, exactly as
        // `bootstrap-bridge-state` does. Genesis is the case that needs it MOST, not least: these
        // two values have never been exercised by anything, they are hand-typed from a funding
        // transaction made outside all of this tooling, and the bootstrap datum is deliberately
        // unpinned on-chain (spec §Why the bootstrap datum is not pinned) — so nothing downstream
        // rejects a wrong one. The mistake surfaces at the first movement, as a BIP-341 sighash
        // committing to the wrong prevout amount and every FROST signature the roster produces
        // being invalid, with no error naming the cause.
        if skipBtcCheck then
            Console.warn(
              "[DEP-2] --skip-btc-check: the deployment anchor was NOT verified against Bitcoin. " +
                  "A wrong outpoint or amount makes every signature over the first TM invalid."
            )
        else {
            val Array(anchorTxid, anchorVoutStr) = initialTreasuryDisplay.split(':')
            val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
            val verified =
                try rpc.getTxOutValueSat(anchorTxid, anchorVoutStr.toInt).await(timeout)
                catch {
                    case e: Exception =>
                        Console.error(
                          s"[DEP-2] cannot verify the deployment anchor against Bitcoin (gettxout " +
                              s"$initialTreasuryDisplay failed: ${e.getMessage}). Fix the Bitcoin " +
                              "node configuration, or pass --skip-btc-check after verifying the " +
                              "anchor by hand."
                        )
                        break(1)
                }
            verified match {
                case None =>
                    Console.error(
                      s"[DEP-2] the deployment anchor $initialTreasuryDisplay is not an unspent " +
                          "output on Bitcoin (spent, or never existed). A bridge anchored to it " +
                          "can never make its first movement."
                    )
                    break(1)
                case Some(sat) if sat != anchorAmountSat =>
                    Console.error(
                      s"[DEP-2] the anchor $initialTreasuryDisplay holds $sat sat on Bitcoin, but " +
                          s"the bootstrap datum would record $anchorAmountSat sat. The first TM's " +
                          "sighash would commit to the wrong prevout amount and every FROST " +
                          s"signature over it would be invalid. Set " +
                          s"bridge.initial-btc-treasury-amount-sat = $sat."
                    )
                    break(1)
                case Some(sat) =>
                    Console.info(
                      "[DEP-2] anchor verified",
                      s"$initialTreasuryDisplay = $sat sat (unspent)"
                    )
            }
        }

        val initialState = BridgeState(
          spiRoot = EmptyRoot,
          cpoRoot = EmptyRoot,
          treasuryUtxoId = anchorOutpoint,
          treasuryAmount = BigInt(anchorAmountSat)
        )

        // --- federation side (config indices 7-13) ---
        //
        // The derivation chain is strictly ordered: one-shot -> spos_registry -> the three fault
        // verifiers -> spo_bans. Every one of these values is an INPUT to the policy id it
        // identifies, so an SPO handed them by email cannot check them — a wrong one derives a
        // well-formed address holding nothing. That is why genesis derives them here and publishes
        // the finished ids at #7-#13.
        //
        // A SECOND one-shot, distinct from the config/cpi/bridge-state one. It has to be: a
        // one-shot mint handler requires its outref to be SPENT in the minting transaction, and the
        // registry and ban roots cannot be minted in the config transaction — the scripts together
        // exceed the 16 kB max_tx_size. So the federation gets its own outref, spent by its own
        // transaction, which must confirm BEFORE the config exists: the Config NFT is the bridge's
        // identity, so minting the roots first is what makes "a bridge cannot exist without a ban
        // list" true by construction rather than by procedure.
        val federationUtxo = BridgeBootstrap
            .pickOneShot(walletUtxos, excludedInputs + oneShotUtxo.input)
            .getOrElse {
                Console.error(
                  "No SECOND clean pure-ADA wallet UTxO (>=5 ADA) for the federation one-shot. " +
                      "Genesis needs two: one for config/cpi/bridge-state and one for the " +
                      "registry + ban roots, which cannot share a transaction (the scripts " +
                      "together exceed the 16 kB max_tx_size). Fund the sponsor wallet with " +
                      "another UTxO"
                )
                break(1)
            }
        val federationRef = refOf(federationUtxo)
        val registryContract =
            SposRegistryContract(blueprint, federationRef.id.hash, federationRef.idx)
        val registryPolicy = ByteString.fromArray(registryContract.policyId.bytes)
        val faultPolicies = FaultVerifierContract.all(blueprint, registryPolicy)
        val banSchedule = config.bridge.banSchedule
        val spoBansContract = SpoBansContract(
          blueprint,
          registryPolicy,
          faultPolicies,
          baseBanDurationMs = BigInt(banSchedule.baseBanDurationMs),
          maxFaultsBeforePermanent = BigInt(banSchedule.maxFaultsBeforePermanent),
          maxValidityWindowMs = BigInt(banSchedule.maxValidityWindowMs),
          bootstrapTxId = federationRef.id.hash,
          bootstrapIndex = federationRef.idx
        )
        // Rev 5.4: treasury_info takes the registry policy alone — the TM-NFT parameter went with
        // the FederationReset branch that was its only reader.
        val treasuryInfoContract = TreasuryInfoContract(blueprint, registryPolicy)

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

        // Config #15 (spec [CFG-4]): the treasury's federation PUBLIC key, x-only. Validated here
        // rather than trusted, because nothing downstream can catch a bad one — no Cardano
        // validator checks a Bitcoin Taproot tree, and every SPO that reads it will build a
        // treasury address from it and sign against that address forever after.
        val yFederationPubkey = {
            val s = config.bridge.yFederationPubkey.trim
            if s.isEmpty then {
                Console.error(
                  "bridge.y-federation-pubkey is required: it is config #15, the federation key " +
                      "every SPO rebuilds the treasury Taproot tree from. Run `heimdall " +
                      "bootstrap-treasury` — it prints the x-only public key next to the treasury " +
                      "address. Publish the PUBLIC key, never the seed"
                )
                break(1)
            }
            if s.length != 64 || !s.forall(c => "0123456789abcdefABCDEF".contains(c)) then {
                // 66 hex is the compressed-SEC form; it is the likeliest wrong paste, and it
                // would build a different tree rather than fail.
                Console.error(
                  s"bridge.y-federation-pubkey must be 32 bytes (64 hex) x-only, got ${s.length} " +
                      "chars. A 66-char compressed key is not the x-only form — drop the 02/03 prefix"
                )
                break(1)
            }
            ByteString.fromHex(s)
        }
        if config.bridge.federationCsvBlocks <= 0 then {
            Console.error(
              s"bridge.federation-csv-blocks must be positive, got ${config.bridge.federationCsvBlocks}"
            )
            break(1)
        }

        val configDatum = ConfigDatum(
          // Governance: the binocular owner key (oracle.owner-pkh) may Update/Retire
          // the config (progressive decentralization rotates this via a later update).
          updateAuth = POption.Some(
            AuthorizationMethod.CardanoSignature(updateAuthPkh)
          ),
          bridgedTokenPolicy = bridgedTokenPolicy,
          completedPegInsPolicy = cpiPolicy,
          bridgeStatePolicy = bssPolicy,
          // spec [CFG-2]: published so off-chain readers can locate the TM address; no on-chain
          // reader.
          tmScriptHash = tmNftPolicy,
          pegInScriptHash = pegInWithdrawHash,
          pegOutScriptHash = pegOutWithdrawHash,
          // Federation identity (config #7-13; off-chain readers only, spec [CFG-3]). Publishing
          // these is what lets an SPO join this bridge with NO ban or registry configuration.
          spoBansPolicyId = ByteString.fromArray(spoBansContract.policyId.bytes),
          baseBanDurationMs = BigInt(banSchedule.baseBanDurationMs),
          maxFaultsBeforePermanent = BigInt(banSchedule.maxFaultsBeforePermanent),
          maxValidityWindowMs = BigInt(banSchedule.maxValidityWindowMs),
          sposRegistryPolicyId = registryPolicy,
          treasuryInfoPolicyId = ByteString.fromArray(treasuryInfoContract.policyId.bytes),
          treasuryInfoAssetName = TreasuryInfoAssetName,
          // Federation identity (config #15-16, spec [CFG-4]). The x-only PUBLIC key: the seed
          // never reaches this process, let alone the chain.
          yFederationPubkey = yFederationPubkey,
          federationCsvBlocks = BigInt(config.bridge.federationCsvBlocks),
          // Operational-parameter tunables (config field 14, nested; off-chain readers only).
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
        Console.info("bridge-state one-shot", s"${bssRef.id.hash.toHex}#${bssRef.idx}")
        Console.info("bridge-state policy (config field 3)", bssPolicy.toHex)
        Console.info("bridge-state asset", bssAssetName.toHex)
        Console.info("anchor (initial treasury)", config.bridge.initialBtcTreasuryUtxo)
        Console.info("anchor amount (sat)", config.bridge.initialBtcTreasuryAmountSat.toString)
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

        // --- Federation tx, FIRST: spend the federation one-shot and mint both linked-list
        //     roots. It cannot be merged into the bootstrap tx below — the two script sets
        //     together exceed the 16 kB max_tx_size — and it must PRECEDE it, because the config
        //     NFT is the bridge's identity: minting the roots first is what makes "a bridge cannot
        //     exist without a ban list" true by construction rather than by procedure. A crash
        //     between the two leaves an orphan registry and ban list, which costs a few ADA and is
        //     simply re-run; the reverse order would leave a live bridge whose published addresses
        //     hold nothing.
        //
        //     Neither mint handler constrains output ORDER (both locate their root by script
        //     address + asset via find_singleton_asset_output_at_script), unlike config.ak. ---
        Console.step(1, "Bootstrapping the federation (registry + ban roots) in one tx")
        val registryRootAsset = AssetName(SposRegistryContract.RootAssetName)
        val banRootAsset = AssetName(SpoBansContract.RootAssetName)
        val registryRootValue =
            Value.lovelace(2_000_000L) +
                Value.asset(registryContract.policyId, registryRootAsset, 1L)
        val banRootValue =
            Value.lovelace(2_000_000L) + Value.asset(spoBansContract.policyId, banRootAsset, 1L)
        val federationTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(federationUtxo)
                    .mint(
                      registryContract.script,
                      Map(registryRootAsset -> 1L),
                      FederationRoot.RegistryBootstrapRedeemer
                    )
                    .mint(
                      spoBansContract.script,
                      Map(banRootAsset -> 1L),
                      FederationRoot.banBootstrapRedeemer(federationRef.id.hash, federationRef.idx)
                    )
                    .payTo(
                      registryContract.address(network),
                      registryRootValue,
                      FederationRoot.Datum
                    )
                    .payTo(spoBansContract.address(network), banRootValue, FederationRoot.Datum)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building federation tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }
        // submitAndConfirm does not return until this is on chain, which is the ordering guarantee:
        // the config tx below is never built against a federation that failed to materialise.
        val federationTxHash = submitAndConfirm(provider, federationTx, timeout)
        Console.success(s"Federation bootstrapped: $federationTxHash")
        Console.info("registry address", registryContract.address(network).encode.getOrElse("?"))
        Console.info("ban list address", spoBansContract.address(network).encode.getOrElse("?"))
        println()

        // --- Single bootstrap tx: spend the one-shot, mint all three protocol NFTs, and create
        //     every protocol UTxO in one atomic tx. config.ak::mint checks self.outputs[0] is the
        //     config UTxO, so the config output MUST be first. cpi/bridge_state mint handlers find
        //     their own output by script address and see the shared one-shot consumed. ---
        Console.step(2, "Bootstrapping bridge (config + cpi + bridge-state) in one tx")
        val configAsset = AssetName(ConfigAssetName)
        val cpiAsset = AssetName(cpiAssetName)
        val bssAsset = AssetName(bssAssetName)
        val configValue =
            Value.lovelace(2_000_000L) + Value.asset(configContract.policyId, configAsset, 1L)
        val cpiValue = Value.lovelace(2_000_000L) + Value.asset(cpiContract.policyId, cpiAsset, 1L)
        val cpiDatum = CompletedPegInsMerkleTreeDatum(EmptyRoot)
        // Shared with `bootstrap-bridge-state`, so a bridge deployed here and a singleton re-minted
        // during a §Recovery replacement produce the same output shape.
        val (bssAddress, bssValue, bssDatum) =
            BridgeBootstrap.bridgeStateOutput(bssContract, network, initialState)
        val bootstrapTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(oneShotUtxo)
                    .mint(configContract.script, Map(configAsset -> 1L), Data.unit)
                    .mint(cpiContract.script, Map(cpiAsset -> 1L), Data.unit)
                    .mint(bssContract.script, Map(bssAsset -> 1L), Data.unit)
                    // config UTxO first (config.ak::mint reads self.outputs[0]).
                    .payTo(configContract.address(network), configValue, configDatum.toData)
                    .payTo(cpiContract.address(network), cpiValue, cpiDatum.toData)
                    .payTo(bssAddress, bssValue, bssDatum)
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
          "bridge-state address",
          bssContract.address(network).encode.getOrElse("?")
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
        Console.info("bridge-state-policy-id", bssPolicy.toHex)
        // Both one-shot refs are the shared bootstrap one-shot; deploy-script-refs and
        // confirm-tmtx require them set to re-derive the cpi / bridge_state scripts.
        Console.info(
          "completed-peg-ins-one-shot-ref",
          s"${configRef.id.hash.toHex}#${configRef.idx}"
        )
        Console.info(
          "bridge-state-one-shot-ref",
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
