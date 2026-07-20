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
  * Config layout (12 fields): index 0/1 (bridged-token policy + asset), 2/3 (completed-peg-ins /
  * completed-peg-outs MPF policies), 4/5 (peg_in / peg_out withdraw), 6 (peg-in close verifier,
  * Dummy28 placeholder), 7 (real BTC-tx-parsing produced verifier,
  * [[PegOutProducedVerifierContract]]), 8 (not-produced placeholder,
  * [[PegOutNotProducedVerifierContract]] – `Cancel`/refund is out of scope), 9 (min_stake), 10
  * (`update_auth` – the binocular owner key `oracle.owner-pkh`, which may Update/Retire the config
  * per config.ak's spend handler) and 11 (`initial_btc_treasury_utxo` – the 36-byte anchor outpoint
  * the FIRST Treasury Movement must spend, from `bridge.initial-btc-treasury-utxo`). Minting a new
  * config NFT changes the bridged-token policy, so re-mint under this config. The cpi/cpo NFT asset
  * names are the constants "CPI"/"CPO". The TM validator is parameterized by (oracle hash, config
  * NFT policy, config NFT asset), so its address derives from this deploy's config NFT — no
  * TM-control NFT exists anymore; TM minting is permissionless, gated by chain linkage (see
  * [[TmMintRedeemer]]).
  */
case class DeployBridgeCommand(dryRun: Boolean = false) extends Command {

    // Config NFT asset name (arbitrary; recorded as config asset name).
    private val ConfigAssetName: ByteString = ByteString.fromString("BIFCFG")
    // Bridged-token asset name.
    private val BridgedTokenAssetName: ByteString = ByteString.fromString("fSAT")
    private val Dummy28: ByteString = ByteString.fromArray(Array.fill[Byte](28)(0))
    private val EmptyRoot: ByteString = ByteString.fromArray(Array.fill[Byte](32)(0))

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

        // The initial Bitcoin treasury outpoint (config field 11): the FIRST TM must spend it.
        val initialTreasuryOutpoint =
            try
                if config.bridge.initialBtcTreasuryUtxo.trim.nonEmpty then
                    BridgeConfig.outpointFromDisplay(config.bridge.initialBtcTreasuryUtxo.trim)
                else if dryRun then {
                    Console.warn("bridge.initial-btc-treasury-utxo unset — zeros for dry-run")
                    ByteString.fromArray(Array.fill[Byte](36)(0))
                } else {
                    Console.error(
                      "Set bridge.initial-btc-treasury-utxo to the initial Bitcoin treasury " +
                          "outpoint as TXID:VOUT (display txid)"
                    )
                    break(1)
                }
            catch {
                case e: IllegalArgumentException =>
                    Console.error(s"bridge.initial-btc-treasury-utxo: ${e.getMessage}")
                    break(1)
            }

        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(
                      s"Loading bridge blueprint (${config.bridge.plutusJson}): ${e.getMessage}"
                    )
                    break(1)
            }

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

        def cleanCandidates(utxos: List[Utxo]): List[Utxo] =
            utxos
                .filter(u =>
                    u.output.value.assets.isEmpty && u.output.value.coin.value >= 5_000_000L
                )
                .filterNot(u => excludedInputs.contains(u.input))
                .sortBy(-_.output.value.coin.value)

        val signer = setup.hdAccount.signerForUtxos

        // The whole bridge is bootstrapped in ONE tx that spends a single one-shot UTxO. Every
        // protocol NFT policy (config, cpi, cpo, tm-control) is parameterized by that same outref,
        // so they get distinct policy ids while all their mint handlers see the one UTxO consumed.
        val oneShotUtxo = cleanCandidates(walletUtxos).headOption.getOrElse {
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

        val pegIn =
            PegInContract(blueprint, oraclePolicyId, configPolicy, ConfigAssetName, tmNftPolicy)
        val pegInWithdrawHash = pegIn.policyId

        // --- peg-out side (config indices 3 = completed-peg-outs MPF, 5 = peg_out withdraw,
        //     7 = produced verifier, 8 = not-produced verifier placeholder) ---
        val pegOut = PegOutContract(blueprint, oraclePolicyId, configPolicy, ConfigAssetName)
        val pegOutWithdrawHash = pegOut.policyId

        val cpoContract =
            CompletedPegOutsContract(blueprint, configPolicy, ConfigAssetName, cpoRef)
        val cpoPolicy = cpoContract.policyId
        val cpoAssetName = CompletedPegOutsContract.assetName

        val pegOutProducedVerifierHash =
            PegOutProducedVerifierContract.pinnedScript.scriptHash
        val pegOutNotProducedVerifierHash =
            PegOutNotProducedVerifierContract.pinnedScript.scriptHash

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
          bridgedTokenPolicyId = bridgedTokenPolicy,
          bridgedTokenAssetName = BridgedTokenAssetName,
          completedPegInsMerkleTreePolicyId = cpiPolicy,
          completedPegOutsMerkleTreePolicyId = cpoPolicy,
          pegInWithdrawScriptHash = pegInWithdrawHash,
          pegOutWithdrawScriptHash = pegOutWithdrawHash,
          // config[6] = peg-in CLOSE verifier. peg_in.ak's Cancel delegates the F4/F5 close checks
          // to a withdrawal from this script. Dummy28 until the F1–F6 failure-mode close verifier is
          // built + deployed; a Dummy28 hash has no reward account, so Cancel is cleanly
          // unsatisfiable in the meantime. Wiring the real verifier later is a config update only –
          // no peg_in recompile / PIR re-mint.
          pegInCloseVerifierScriptHash = Dummy28,
          legitTmAndPegOutProducedVerifierScriptHash = pegOutProducedVerifierHash,
          legitTmAndPegOutNotProducedVerifierScriptHash = pegOutNotProducedVerifierHash,
          minStake = BigInt(0),
          // Governance: the binocular owner key (oracle.owner-pkh) may Update/Retire
          // the config (progressive decentralization rotates this via a later update).
          updateAuth = POption.Some(
            AuthorizationMethod.CardanoSignature(updateAuthPkh)
          ),
          // The anchor outpoint the first Treasury Movement must spend (chain genesis).
          initialBtcTreasuryUtxo = initialTreasuryOutpoint
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
        Console.info("peg_out produced verifier", pegOutProducedVerifierHash.toHex)
        Console.info("peg_out not-produced verifier", pegOutNotProducedVerifierHash.toHex)
        Console.info("TM-NFT policy (peg_in param)", tmNftPolicy.toHex)
        Console.info("initial treasury outpoint", initialTreasuryOutpoint.toHex)
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
        val cpoValue = Value.lovelace(2_000_000L) + Value.asset(cpoContract.policyId, cpoAsset, 1L)
        val cpiDatum = CompletedPegInsMerkleTreeDatum(EmptyRoot)
        val cpoDatum = CompletedPegOutsMerkleTreeDatum(EmptyRoot)
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
                    .payTo(cpoContract.address(network), cpoValue, cpoDatum.toData)
                    // Register the peg_in / peg_out withdraw reward accounts here (deposit-less
                    // Shelley RegCert, no script execution) so completion txs can withdraw. Both
                    // hashes are fresh per deploy (they derive from this deploy's config policy), so
                    // this is always a first-time registration - safe in an atomic tx. The
                    // produced-verifier account is NOT registered here: its hash is constant across
                    // deploys, so on a redeploy it is already registered and would fail the whole tx;
                    // `register-bridge-creds` (idempotent) handles it before peg-out completion.
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
        Console.info("peg-out-produced-verifier-hash", pegOutProducedVerifierHash.toHex)
        Console.info(
          "next",
          "peg_in/peg_out reward accounts registered in the bootstrap tx; run " +
              "`register-bridge-creds` to register the produced verifier before peg-out completion"
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
