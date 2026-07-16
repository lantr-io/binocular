package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.ledger.{AssetName, Transaction, TransactionHash, Utxo, Value}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** F3: deploy the ft-bifrost-bridge completion contracts on Cardano.
  *
  * Mints, in two sequential txs, the two one-shot NFTs the peg-in completion path reads:
  *   1. the **config NFT** (`config.ak`) carrying the [[ConfigDatum]] — the spine that records
  *      every cross-referenced script hash;
  *   2. the **completed-peg-ins MPF NFT** (`completed-peg-ins-merkle-tree.ak`) with an empty root;
  *   3. the **completed-peg-outs MPF NFT** (`completed-peg-outs-merkle-tree.ak`) with an empty
  *      root.
  *
  * The fBTC (`bridged_token`) policy has no state UTxO — it's a mint policy whose hash is recorded
  * in the config datum (index 0). All hashes are computed deterministically from the chosen
  * one-shot wallet UTxOs + the live oracle policy (see [[BifrostContracts]]). After deploy, set
  * `binocular.bridge.{config-nft-*, bridged-token-*}` to the printed values and re-mint the
  * PegInRequests so the peg_in policy + owner_auth match.
  *
  * Peg-out wiring (this iteration): config indices 8/9 (completed-peg-outs MPF), 11 (peg_out
  * withdraw), 13 (real BTC-tx-parsing produced verifier, [[PegOutProducedVerifier]]) and 14
  * (not-produced placeholder, [[PegOutNotProducedVerifier]] — `Cancel`/refund is out of scope) are
  * now REAL, as are index 18 (`update_auth` — the sponsor's payment key, which may Update/Retire
  * the config per config.ak's spend handler) and index 19 (the swappable fBTC mint checker).
  * Minting a new config NFT changes the fBTC policy, so re-mint fBTC under this config. Treasury /
  * source-chain / block-header entries remain dummies (no path reads them yet).
  */
case class DeployBridgeCommand(authorizedMinter: Option[String] = None, dryRun: Boolean = false)
    extends Command {

    // TM-control NFT asset name (the control UTxO it tags holds the TmControlDatum).
    private val TmControlAssetName: ByteString = ByteString.fromString("TMCTRL")

    // Config NFT asset name (arbitrary; recorded as config asset name).
    private val ConfigAssetName: ByteString = ByteString.fromString("BIFCFG")
    // fBTC asset name.
    private val BridgedTokenAssetName: ByteString = ByteString.fromString("fBTC")
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
        val oraclePolicyId = ByteString.fromArray(setup.script.scriptHash.bytes)

        // The key authorized to mint TM NFTs (written into the TM-control datum).
        val minterHex =
            authorizedMinter.filter(_.nonEmpty).getOrElse(config.bridge.tmAuthorizedMinter)
        val authorizedMinterBS =
            if minterHex.length == 56 && minterHex.forall(c => "0123456789abcdefABCDEF".contains(c))
            then ByteString.fromHex(minterHex)
            else if dryRun then {
                Console.warn("authorized-minter unset/invalid — zeros for dry-run"); Dummy28
            } else {
                Console.error(
                  "Set --authorized-minter (or bridge.tm-authorized-minter) to the 28-byte (56 hex) " +
                      "authorized-minter pkh (e.g. `heimdall wallet-address` → payment key hash)"
                )
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
        def utxoKey(u: Utxo): String = { val r = refOf(u); s"${r.id.hash.toHex}#${r.idx}" }

        // --- pick 4 distinct pure-ADA one-shot UTxOs (config + completed-peg-ins + completed-peg-outs
        //     + TM-control) ---
        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }
        // A reference-script UTxO (CIP-33) is pure-lovelace with no native assets, so it looks
        // identical to a plain change UTxO to the filter below — but spending one DESTROYS a deployed
        // reference script, and (because BlockfrostProvider drops `scriptRef` on findUtxos) the tx
        // builder under-estimates the fee by the Conway reference-script surcharge → the mint tx is
        // rejected with FeeTooSmallUTxO. We must exclude every ref-script UTxO. The config-recorded
        // refs are not enough (a sponsor wallet accumulates duplicate CIP-33 outputs across runs), so
        // also query blockfrost's address-utxos endpoint directly for `reference_script_hash` — the
        // one piece of state the provider discards. Best-effort: on any query failure we still exclude
        // the config-known refs.
        val configKnownRefs: Set[String] = Set(
          config.bridge.pegInScriptRef,
          config.bridge.bridgedTokenScriptRef,
          config.bridge.completedPegInsScriptRef,
          config.bridge.completedPegInsOneShotRef
        ).iterator.map(_.trim.toLowerCase).filter(_.nonEmpty).toSet

        def queriedRefScriptOutrefs(): Set[String] = {
            val backendOk = config.cardano.backend.toLowerCase == "blockfrost" &&
                config.cardano.blockfrostProjectId.nonEmpty
            val addr = sponsorAddress.encode.getOrElse("")
            if !backendOk || addr.isEmpty then Set.empty
            else
                val base = config.cardano.network.toLowerCase match {
                    case "mainnet" => "https://cardano-mainnet.blockfrost.io/api/v0"
                    case "preview" => "https://cardano-preview.blockfrost.io/api/v0"
                    case _         => "https://cardano-preprod.blockfrost.io/api/v0"
                }
                try {
                    val client = java.net.http.HttpClient.newHttpClient()
                    def page(p: Int): Seq[ujson.Value] = {
                        val req = java.net.http.HttpRequest
                            .newBuilder()
                            .uri(
                              java.net.URI.create(s"$base/addresses/$addr/utxos?count=100&page=$p")
                            )
                            .header("project_id", config.cardano.blockfrostProjectId)
                            .GET()
                            .build()
                        val resp =
                            client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
                        if resp.statusCode() != 200 then Seq.empty
                        else ujson.read(resp.body()).arr.toSeq
                    }
                    def collect(p: Int, acc: Vector[ujson.Value]): Vector[ujson.Value] = {
                        val items = page(p)
                        val next = acc ++ items
                        if items.size < 100 then next else collect(p + 1, next)
                    }
                    collect(1, Vector.empty).iterator.flatMap { o =>
                        o.obj.get("reference_script_hash") match {
                            case Some(ujson.Str(_)) =>
                                Some(
                                  s"${o("tx_hash").str.toLowerCase}#${o("output_index").num.toInt}"
                                )
                            case _ => None
                        }
                    }.toSet
                } catch { case _: Exception => Set.empty }
        }

        val excludedRefs: Set[String] = configKnownRefs ++ queriedRefScriptOutrefs()
        def cleanCandidates(utxos: List[Utxo]): List[Utxo] =
            utxos
                .filter(u =>
                    u.output.value.assets.isEmpty && u.output.value.coin.value >= 5_000_000L
                )
                .filterNot(u => excludedRefs.contains(utxoKey(u)))
                .sortBy(-_.output.value.coin.value)

        val signer = setup.hdAccount.signerForUtxos

        // A cluttered sponsor wallet often locks its ADA in multi-asset / reference-script UTxOs,
        // leaving fewer than the 4 clean pure-ADA outputs the one-shots need. If short, split the
        // largest clean UTxO into fresh self-outputs first (skipped in dry-run). Spending one clean
        // UTxO yields `deficit` explicit outputs + a clean change output, so this always closes the
        // gap when at least one large clean UTxO exists.
        val MinOneShots = 4
        val initialCandidates = cleanCandidates(walletUtxos)
        val candidates =
            if initialCandidates.size >= MinOneShots || dryRun then initialCandidates
            else {
                val deficit = MinOneShots - initialCandidates.size
                Console.step(0, s"Preparing wallet: creating $deficit clean one-shot UTxO(s)")
                val funder = initialCandidates.headOption.getOrElse {
                    Console.error(
                      "No clean pure-ADA UTxO (>=5 ADA) available to split for the one-shots"
                    )
                    break(1)
                }
                val splitOut = Value.lovelace(10_000_000L)
                val splitTx =
                    try
                        (1 to deficit)
                            .foldLeft(TxBuilder(provider.cardanoInfo).spend(funder))((b, _) =>
                                b.payTo(sponsorAddress, splitOut)
                            )
                            .complete(provider, sponsorAddress)
                            .await(timeout)
                            .sign(signer)
                            .transaction
                    catch {
                        case e: Exception =>
                            Console.error(s"Building wallet-split tx: ${e.getMessage}"); break(1)
                    }
                val splitHash = submitAndConfirm(provider, splitTx, timeout)
                Console.success(s"Wallet split tx: $splitHash")
                OracleTransactions.waitForUtxoAtAddress(
                  provider,
                  sponsorAddress,
                  TransactionHash.fromHex(splitHash),
                  timeout
                ) match {
                    case Left(err) => Console.error(err); break(1)
                    case _         =>
                }
                val refreshed = provider.findUtxos(sponsorAddress).await(timeout) match {
                    case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
                    case Left(err)    => Console.error(s"Re-fetching wallet UTxOs: $err"); break(1)
                }
                cleanCandidates(refreshed)
            }
        val (configOneShot, cpiOneShot, cpoOneShot, tmControlOneShot) = candidates match {
            case a :: b :: c :: d :: _ => (a, b, c, d)
            case _ =>
                Console.error(
                  "Need >=4 pure-ADA wallet UTxOs (>=5 ADA each, excluding reference-script UTxOs) " +
                      "for the one-shots; wallet-split could not produce enough"
                )
                break(1)
        }
        val configRef = refOf(configOneShot)
        val cpiRef = refOf(cpiOneShot)
        val cpoRef = refOf(cpoOneShot)
        val tmControlRef = refOf(tmControlOneShot)

        // TM-control one-shot NFT — authenticates the control UTxO (TmControlDatum).
        val tmControlContract = OneShotMintContract.contract(tmControlRef)
        val tmControlPolicy = ByteString.fromArray(tmControlContract.script.scriptHash.bytes)

        // --- compute the deterministic hash chain ---
        val configContract =
            ConfigContract(blueprint, configRef.id.hash, configRef.idx, ConfigAssetName)
        val configPolicy = ByteString.fromArray(configContract.policyId.bytes)

        val bridgedToken = BridgedTokenContract(blueprint, configPolicy, ConfigAssetName)
        val bridgedTokenPolicy = ByteString.fromArray(bridgedToken.policyId.bytes)

        // The swappable fBTC mint checker (ConfigDatum index 19). bridged_token only requires a
        // withdrawal from this script; all mint/burn rules live here and rotate via config Update.
        val fbtcMintChecker = FbtcMintCheckerContract(blueprint, configPolicy, ConfigAssetName)
        val fbtcMintCheckerHash = ByteString.fromArray(fbtcMintChecker.scriptHash.bytes)

        val cpiContract =
            CompletedPegInsContract(blueprint, configPolicy, ConfigAssetName, cpiRef)
        val cpiPolicy = ByteString.fromArray(cpiContract.policyId.bytes)
        val cpiAssetName = CompletedPegInsContract.assetName(cpiRef)

        // TM-NFT policy = the TreasuryMovementValidator script hash (oracle hash + the TM-control
        // NFT minted in this same deploy tx). peg_in.ak references the Confirmed TM UTxO by this NFT
        // (its 4th param), so the peg_in hash depends on it.
        // Derived from the blueprint script() — the SAME path the watchtower/relay/confirm
        // use. The SIR-applied contract() hashes differently (params applied pre-optimization),
        // so using it here would split the system across two TM script hashes.
        val tmNftPolicy = ByteString.fromArray(
          TreasuryMovementContract
              .script(oraclePolicyId, tmControlPolicy, TmControlAssetName)
              .scriptHash
              .bytes
        )

        val pegIn =
            PegInContract(blueprint, oraclePolicyId, configPolicy, ConfigAssetName, tmNftPolicy)
        val pegInWithdrawHash = ByteString.fromArray(pegIn.policyId.bytes)

        // --- peg-out side (config indices 8/9 = completed-peg-outs MPF, 11 = peg_out withdraw,
        //     13 = produced verifier, 14 = not-produced verifier placeholder) ---
        val pegOut = PegOutContract(blueprint, oraclePolicyId, configPolicy, ConfigAssetName)
        val pegOutWithdrawHash = ByteString.fromArray(pegOut.policyId.bytes)

        val cpoContract =
            CompletedPegOutsContract(blueprint, configPolicy, ConfigAssetName, cpoRef)
        val cpoPolicy = ByteString.fromArray(cpoContract.policyId.bytes)
        val cpoAssetName = CompletedPegOutsContract.assetName(cpoRef)

        val pegOutProducedVerifierHash =
            ByteString.fromArray(PegOutProducedVerifierContract.pinnedScript.scriptHash.bytes)
        val pegOutNotProducedVerifierHash =
            ByteString.fromArray(PegOutNotProducedVerifierContract.pinnedScript.scriptHash.bytes)

        val configDatum = ConfigDatum(
          bridgedTokenPolicyId = bridgedTokenPolicy,
          bridgedTokenAssetName = BridgedTokenAssetName,
          sourceChainMerkleTreePolicyId = Dummy28,
          sourceChainMerkleTreeAssetName = ByteString.empty,
          blockHeaderMerkleTreePolicyId = Dummy28,
          blockHeaderMerkleTreeAssetName = ByteString.empty,
          completedPegInsMerkleTreePolicyId = cpiPolicy,
          completedPegInsMerkleTreeAssetName = cpiAssetName,
          completedPegOutsMerkleTreePolicyId = cpoPolicy,
          completedPegOutsMerkleTreeAssetName = cpoAssetName,
          pegInWithdrawScriptHash = pegInWithdrawHash,
          pegOutWithdrawScriptHash = pegOutWithdrawHash,
          // config[12] = peg-in CLOSE verifier (the slot the retired legit_TM_verifier vacated).
          // peg_in.ak's Cancel delegates the F4/F5 close checks to a withdrawal from this script.
          // Dummy28 until the F1–F6 failure-mode close verifier is built + deployed; a Dummy28 hash
          // has no reward account, so Cancel is cleanly unsatisfiable in the meantime. Wiring the
          // real verifier later is a config update only — no peg_in recompile / PIR re-mint.
          pegInCloseVerifierScriptHash = Dummy28,
          legitTmAndPegOutProducedVerifierScriptHash = pegOutProducedVerifierHash,
          legitTmAndPegOutNotProducedVerifierScriptHash = pegOutNotProducedVerifierHash,
          treasuryNftPolicyId = Dummy28,
          treasuryNftAssetName = ByteString.empty,
          minStake = BigInt(0),
          // Demo/testnet governance: the sponsor's payment key may Update/Retire the config
          // (progressive decentralization rotates this later via a config update).
          updateAuth = scalus.cardano.onchain.plutus.prelude.Option.Some(
            AuthorizationMethod.CardanoSignature(
              ByteString.fromArray(setup.hdAccount.paymentKeyHash.bytes)
            )
          ),
          bridgedTokenMintCheckerScriptHash = fbtcMintCheckerHash
        )

        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("config one-shot", s"${configRef.id.hash.toHex}#${configRef.idx}")
        Console.info("cpi one-shot", s"${cpiRef.id.hash.toHex}#${cpiRef.idx}")
        println()
        Console.info("config NFT policy", configPolicy.toHex)
        Console.info("config NFT asset", ConfigAssetName.toHex)
        Console.info("bridged_token (fBTC) policy", bridgedTokenPolicy.toHex)
        Console.info("bridged_token (fBTC) asset", BridgedTokenAssetName.toHex)
        Console.info("fbtc mint checker hash", fbtcMintCheckerHash.toHex)
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
        Console.info("TM-control one-shot", s"${tmControlRef.id.hash.toHex}#${tmControlRef.idx}")
        Console.info("TM-control NFT policy", tmControlPolicy.toHex)
        Console.info("TM-control NFT asset", TmControlAssetName.toHex)
        Console.info("authorized minter", authorizedMinterBS.toHex)
        println()

        if dryRun then {
            Console.success(
              "Dry-run complete (computed hashes + assembled config datum; not submitting)"
            )
            break(0)
        }

        // --- Tx 1: mint the config NFT, carrying the ConfigDatum, to the config script address ---
        Console.step(1, "Minting config NFT")
        val configAsset = AssetName(ConfigAssetName)
        val configValue =
            Value.lovelace(2_000_000L) + Value.asset(configContract.policyId, configAsset, 1L)
        val configTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(configOneShot)
                    .mint(configContract.script, Map(configAsset -> 1L), Data.unit)
                    .payTo(configContract.address(network), configValue, configDatum.toData)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            catch {
                case e: Exception => Console.error(s"Building config tx: ${e.getMessage}"); break(1)
            }
        val configTxHash = submitAndConfirm(provider, configTx, timeout)
        Console.success(s"Config NFT minted: $configTxHash")
        Console.info("config address", configContract.address(network).encode.getOrElse("?"))
        println()

        // Wait for the address-based UTxO index to reflect tx 1 before building tx 2, so tx 2's
        // fee/change selection doesn't pick tx 1's already-spent inputs (pollForConfirmation
        // checks tx status, not the address index). Same convention as InitOracleCommand.
        OracleTransactions.waitForUtxoAtAddress(
          provider,
          sponsorAddress,
          TransactionHash.fromHex(configTxHash),
          timeout
        ) match {
            case Left(err) => Console.error(err); break(1)
            case _         =>
        }

        // --- Tx 2: mint the completed-peg-ins NFT (empty MPF root) to its script address ---
        Console.step(2, "Minting completed-peg-ins MPF NFT")
        val cpiAsset = AssetName(cpiAssetName)
        val cpiValue = Value.lovelace(2_000_000L) + Value.asset(cpiContract.policyId, cpiAsset, 1L)
        val cpiDatum = CompletedPegInsMerkleTreeDatum(EmptyRoot)
        val cpiRedeemer = CompletedPegInsMintRedeemer(cpiRef)
        val cpiTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(cpiOneShot)
                    .mint(cpiContract.script, Map(cpiAsset -> 1L), cpiRedeemer.toData)
                    .payTo(cpiContract.address(network), cpiValue, cpiDatum.toData)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building completed-peg-ins tx: ${e.getMessage}"); break(1)
            }
        val cpiTxHash = submitAndConfirm(provider, cpiTx, timeout)
        Console.success(s"Completed-peg-ins NFT minted: $cpiTxHash")
        Console.info(
          "completed-peg-ins address",
          cpiContract.address(network).encode.getOrElse("?")
        )
        println()

        OracleTransactions.waitForUtxoAtAddress(
          provider,
          sponsorAddress,
          TransactionHash.fromHex(cpiTxHash),
          timeout
        ) match {
            case Left(err) => Console.error(err); break(1)
            case _         =>
        }

        // --- Tx 2b: mint the completed-peg-outs NFT (empty MPF root) to its script address ---
        Console.step(3, "Minting completed-peg-outs MPF NFT")
        val cpoAsset = AssetName(cpoAssetName)
        val cpoValue = Value.lovelace(2_000_000L) + Value.asset(cpoContract.policyId, cpoAsset, 1L)
        val cpoDatum = CompletedPegOutsMerkleTreeDatum(EmptyRoot)
        // The mint handler does not read config; configRefInputIndex is an inert positional field.
        val cpoRedeemer = CompletedPegOutsMintRedeemer(cpoRef, BigInt(0))
        val cpoTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(cpoOneShot)
                    .mint(cpoContract.script, Map(cpoAsset -> 1L), cpoRedeemer.toData)
                    .payTo(cpoContract.address(network), cpoValue, cpoDatum.toData)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building completed-peg-outs tx: ${e.getMessage}"); break(1)
            }
        val cpoTxHash = submitAndConfirm(provider, cpoTx, timeout)
        Console.success(s"Completed-peg-outs NFT minted: $cpoTxHash")
        Console.info(
          "completed-peg-outs address",
          cpoContract.address(network).encode.getOrElse("?")
        )
        println()

        OracleTransactions.waitForUtxoAtAddress(
          provider,
          sponsorAddress,
          TransactionHash.fromHex(cpoTxHash),
          timeout
        ) match {
            case Left(err) => Console.error(err); break(1)
            case _         =>
        }

        // --- Tx 3: mint the TM-control NFT to the (immutable, spend=False) config address, carrying
        //     the TmControlDatum that names the authorized TM-NFT minter. The one-shot policy makes
        //     the NFT unforgeable; config.ak's spend=False makes the control UTxO immutable. ---
        Console.step(4, "Minting TM-control NFT")
        val tmControlAsset = AssetName(TmControlAssetName)
        val tmControlValue =
            Value.lovelace(2_000_000L) + Value.asset(
              tmControlContract.script.scriptHash,
              tmControlAsset,
              1L
            )
        val tmControlDatum = TmControlDatum(authorizedMinterBS)
        val tmControlTx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(tmControlOneShot)
                    .mint(tmControlContract, Map(tmControlAsset -> 1L), Data.unit)
                    .payTo(configContract.address(network), tmControlValue, tmControlDatum.toData)
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building TM-control tx: ${e.getMessage}"); break(1)
            }
        val tmControlTxHash = submitAndConfirm(provider, tmControlTx, timeout)
        Console.success(s"TM-control NFT minted: $tmControlTxHash")
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
        Console.info("peg-out-withdraw-hash", pegOutWithdrawHash.toHex)
        Console.info("peg-out-produced-verifier-hash", pegOutProducedVerifierHash.toHex)
        Console.info("tm-control-nft-policy", tmControlPolicy.toHex)
        Console.info("tm-control-nft-name", TmControlAssetName.toHex)
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
