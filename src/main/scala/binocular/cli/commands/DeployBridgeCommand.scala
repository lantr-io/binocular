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
  *   2. the **completed-peg-ins MPF NFT** (`completed-peg-ins-merkle-tree.ak`) with an empty root.
  *
  * The fBTC (`bridged_token`) policy has no state UTxO — it's a mint policy whose hash is recorded
  * in the config datum (index 0). All hashes are computed deterministically from the two chosen
  * one-shot wallet UTxOs + the live oracle policy (see [[BifrostContracts]]). After deploy, set
  * `binocular.bridge.{config-nft-*, bridged-token-*}` to the printed values and re-mint the
  * PegInRequests so the peg_in policy + owner_auth match.
  *
  * Demo scope: peg-out / treasury / source-chain / block-header config entries are dummies (the
  * peg-in completion path doesn't read them).
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

        // --- pick two distinct pure-ADA one-shot UTxOs (config + completed-peg-ins) ---
        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }
        val pureAda = walletUtxos
            .filter(u => u.output.value.assets.isEmpty && u.output.value.coin.value >= 5_000_000L)
            .sortBy(-_.output.value.coin.value)
        val (configOneShot, cpiOneShot, tmControlOneShot) = pureAda match {
            case a :: b :: c :: _ => (a, b, c)
            case _ =>
                Console.error("Need >=3 pure-ADA wallet UTxOs (>=5 ADA each) for the one-shots");
                break(1)
        }

        def refOf(u: Utxo): TxOutRef =
            TxOutRef(TxId(u.input.transactionId), u.input.index)
        val configRef = refOf(configOneShot)
        val cpiRef = refOf(cpiOneShot)
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

        val cpiContract =
            CompletedPegInsContract(blueprint, configPolicy, ConfigAssetName, cpiRef)
        val cpiPolicy = ByteString.fromArray(cpiContract.policyId.bytes)
        val cpiAssetName = CompletedPegInsContract.assetName(cpiRef)

        // TM-NFT policy = the TreasuryMovementValidator script hash (oracle hash + the TM-control
        // NFT minted in this same deploy tx). peg_in.ak references the Confirmed TM UTxO by this NFT
        // (its 4th param), so the peg_in hash depends on it.
        val tmNftPolicy = ByteString.fromArray(
          TreasuryMovementContract
              .contract(oraclePolicyId, tmControlPolicy, TmControlAssetName)
              .script
              .scriptHash
              .bytes
        )

        val pegIn =
            PegInContract(blueprint, oraclePolicyId, configPolicy, ConfigAssetName, tmNftPolicy)
        val pegInWithdrawHash = ByteString.fromArray(pegIn.policyId.bytes)

        val configDatum = ConfigDatum(
          bridgedTokenPolicyId = bridgedTokenPolicy,
          bridgedTokenAssetName = BridgedTokenAssetName,
          sourceChainMerkleTreePolicyId = Dummy28,
          sourceChainMerkleTreeAssetName = ByteString.empty,
          blockHeaderMerkleTreePolicyId = Dummy28,
          blockHeaderMerkleTreeAssetName = ByteString.empty,
          completedPegInsMerkleTreePolicyId = cpiPolicy,
          completedPegInsMerkleTreeAssetName = cpiAssetName,
          completedPegOutsMerkleTreePolicyId = Dummy28,
          completedPegOutsMerkleTreeAssetName = ByteString.empty,
          pegInWithdrawScriptHash = pegInWithdrawHash,
          pegOutWithdrawScriptHash = Dummy28,
          // config[12] legit_TM_verifier — retired in B1 (the TM proof moved to confirm-tmtx and
          // peg_in.ak no longer delegates to a verifier withdraw); kept as a dummy for layout.
          legitTmAndPegInSpentVerifierScriptHash = Dummy28,
          legitTmAndPegOutProducedVerifierScriptHash = Dummy28,
          legitTmAndPegOutNotProducedVerifierScriptHash = Dummy28,
          treasuryNftPolicyId = Dummy28,
          treasuryNftAssetName = ByteString.empty,
          minStake = BigInt(0)
        )

        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("config one-shot", s"${configRef.id.hash.toHex}#${configRef.idx}")
        Console.info("cpi one-shot", s"${cpiRef.id.hash.toHex}#${cpiRef.idx}")
        println()
        Console.info("config NFT policy", configPolicy.toHex)
        Console.info("config NFT asset", ConfigAssetName.toHex)
        Console.info("bridged_token (fBTC) policy", bridgedTokenPolicy.toHex)
        Console.info("bridged_token (fBTC) asset", BridgedTokenAssetName.toHex)
        Console.info("completed-peg-ins policy", cpiPolicy.toHex)
        Console.info("completed-peg-ins asset", cpiAssetName.toHex)
        Console.info("peg_in withdraw hash", pegInWithdrawHash.toHex)
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

        val signer = setup.hdAccount.signerForUtxos

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

        // --- Tx 3: mint the TM-control NFT to the (immutable, spend=False) config address, carrying
        //     the TmControlDatum that names the authorized TM-NFT minter. The one-shot policy makes
        //     the NFT unforgeable; config.ak's spend=False makes the control UTxO immutable. ---
        Console.step(3, "Minting TM-control NFT")
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
            .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 60, delayMs = 2000)
            .await(timeout)
        status match {
            case TransactionStatus.Confirmed => txHash
            case other                       => Console.error(s"Not confirmed: $other"); break(1)
        }
    }
}
