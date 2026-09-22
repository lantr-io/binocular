package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console, DaemonExecution}

import org.bitcoins.core.protocol.BitcoinAddress

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash}
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** P3: create a PegOut request — lock fBTC + MIN_ADA at the `peg_out.ak` script address with a
  * [[PegOutDatum]], so the next Treasury Movement can pay the withdrawer's Bitcoin address.
  *
  * `peg_out.ak` is spend-/withdraw-only, so creation runs NO script — it's a plain pay-to-script
  * output. The withdrawer is the only party harmed by a malformed PegOut, so nothing is enforced
  * on-chain here. The datum records:
  *   - `source_chain_destination_address` — the raw Bitcoin scriptPubKey the TM must pay (derived
  *     from `--btc-address`; e.g. P2WPKH `0014…`, P2TR `5120…`). The on-chain produced verifier
  *     ([[PegOutProducedVerifier]]) later checks a TM output pays exactly this scriptPubKey.
  *   - `per_pegout_fee` — pinned from the live Config, not the local deployment defaults.
  *   - `created` — current Cardano tip time, not wall-clock time.
  *   - `owner_auth` — authority that can reclaim the fBTC if the TM excludes this peg-out
  *     (`Cancel`, out of scope this iteration). Defaults to the sponsor's payment key hash.
  *
  * The peg-out amount is the fBTC quantity locked in the value (no datum field). The TM pays that
  * amount minus the pinned fee. `--treasury-outpoint` remains a legacy display-only argument.
  */
case class PegOutRequestCommand(
    btcAddress: String,
    amountSat: Long,
    treasuryOutpoint: String,
    ownerPkh: Option[String] = None,
    minAda: Long = 2_000_000L,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Peg-Out Request (lock fBTC)")
        if dryRun then Console.warn("Dry-run mode — will assemble but not submit")
        println()

        given ec: ExecutionContext = DaemonExecution.ec
        val timeout = config.oracle.transactionTimeout.seconds

        def hexBytes(label: String, s: String, expectedChars: Option[Int]): ByteString = {
            val isHex = s.length % 2 == 0 && s.forall(c => "0123456789abcdefABCDEF".contains(c))
            if !isHex || expectedChars.exists(_ != s.length) then {
                val want = expectedChars.fold("even-length hex")(n => s"$n hex chars")
                Console.error(s"Invalid $label: expected $want, got '$s'"); break(1)
            }
            ByteString.fromHex(s)
        }

        if amountSat <= 0 then {
            Console.error("--amount must be > 0 (fBTC sat to lock)"); break(1)
        }

        // --- destination scriptPubKey from the Bitcoin address ---
        val destinationSpk: ByteString =
            try
                ByteString.fromArray(
                  BitcoinAddress.fromString(btcAddress).scriptPubKey.asmBytes.toArray
                )
            catch {
                case e: Exception =>
                    Console.error(s"Invalid --btc-address '$btcAddress': ${e.getMessage}")
                    break(1)
            }

        // --- treasury outpoint (display txid:vout) → 36-byte internal prev_txid(LE) ++ vout(LE) ---
        val treasuryUtxoId: ByteString = treasuryOutpoint.split(":") match {
            case Array(txid, voutStr)
                if txid.length == 64 && txid.forall(c => "0123456789abcdefABCDEF".contains(c))
                    && voutStr.toIntOption.exists(_ >= 0) =>
                val vout = voutStr.toInt
                val voutLE = Array[Byte](
                  (vout & 0xff).toByte,
                  ((vout >> 8) & 0xff).toByte,
                  ((vout >> 16) & 0xff).toByte,
                  ((vout >> 24) & 0xff).toByte
                )
                ByteString.fromArray(txid.hexToBytes.reverse) ++ ByteString.fromArray(voutLE)
            case _ =>
                Console.error(
                  s"Invalid --treasury-outpoint: expected TXID:VOUT, got '$treasuryOutpoint'"
                )
                break(1)
        }

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
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

        val configNftPolicy =
            hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        val configNftAsset =
            hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)
        val bridgedTokenAsset =
            AssetName(
              hexBytes("bridge.bridged-token-asset-name", config.bridge.bridgedTokenAssetName, None)
            )

        val pegOut = PegOutContract(blueprint, configNftPolicy)
        val pegOutAddress = pegOut.address(network)
        // fBTC policy = bridged_token derived from the config NFT (== bridge.bridged-token-policy-id).
        val fbtcPolicy = BridgedTokenContract(blueprint, configNftPolicy).policyId
        if fbtcPolicy.toHex != config.bridge.bridgedTokenPolicyId then
            Console.warn(
              s"derived fBTC policy ${fbtcPolicy.toHex} != bridge.bridged-token-policy-id " +
                  s"${config.bridge.bridgedTokenPolicyId} — check config"
            )

        // owner_auth — defaults to the sponsor's payment key hash (so the sponsor can later Cancel).
        val ownerHash =
            ownerPkh
                .map(hexBytes("--owner-pkh", _, Some(56)))
                .getOrElse(ByteString.fromArray(setup.hdAccount.paymentKeyHash.bytes))

        val liveConfig = BridgeSweepSetup
            .loadConfig(
              provider,
              Address(network, Credential.ScriptHash(ScriptHash.fromHex(configNftPolicy.toHex))),
              ScriptHash.fromHex(configNftPolicy.toHex),
              AssetName(configNftAsset),
              timeout
            )
            .valueOr { err =>
                Console.error(err); break(1)
            }
            ._2
        if liveConfig.pegOutScriptHash.toHex != pegOut.policyId.toHex ||
            liveConfig.bridgedTokenPolicy.toHex != fbtcPolicy.toHex
        then {
            Console.error("Derived peg-out scripts do not match the live Config"); break(1)
        }
        if amountSat < liveConfig.params.minPegOutFbtc then {
            Console.error(s"Amount is below the live minimum ${liveConfig.params.minPegOutFbtc}")
            break(1)
        }
        val tipSlot = provider.currentSlot.await(timeout)
        // Pin the live fee and chain time in the request. A later Config update must not change it.
        val datum = PegOutDatum(
          ownerAuth = AuthorizationMethod.CardanoSignature(ownerHash),
          sourceChainDestinationAddress = destinationSpk,
          perPegoutFee = liveConfig.params.perPegoutFee,
          created = BigInt(provider.cardanoInfo.slotConfig.slotToTime(tipSlot))
        )

        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("peg_out address", pegOutAddress.encode.getOrElse("?"))
        Console.info("fBTC policy/asset", s"${fbtcPolicy.toHex} / ${bridgedTokenAsset.bytes.toHex}")
        Console.info("destination scriptPubKey", destinationSpk.toHex)
        Console.info("treasury_utxo_id (36B)", treasuryUtxoId.toHex)
        Console.info("owner_auth pkh", ownerHash.toHex)
        Console.info("fBTC to lock (sat)", amountSat.toString)
        println()

        Console.step(1, "Reading sponsor funding")
        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }
        val excluded = CommandHelpers.refScriptOutpoints(
          config,
          CommandHelpers.refScriptScanAddresses(config, network, sponsorAddress)
        )
        Console.step(2, "Building peg-out lock tx")
        val tx =
            try
                PegOutRequestTx.build(
                  provider.cardanoInfo,
                  setup.hdAccount,
                  walletUtxos,
                  excluded,
                  pegOutAddress,
                  fbtcPolicy,
                  bridgedTokenAsset,
                  amountSat,
                  datum,
                  minAda
                )
            catch {
                case e: Exception =>
                    Console.error(s"Building tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }
        if dryRun then {
            Console.success(s"Dry-run complete: built ${tx.id.toHex}; not submitting")
            break(0)
        }
        val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
            case Right(h)  => h
            case Left(err) => Console.error(s"Submit: $err"); break(1)
        }

        // The PegOut UTxO is the output paying the peg_out address.
        val outIdx =
            tx.body.value.outputs.indexWhere(_.value.address == pegOutAddress)

        println()
        Console.separator()
        Console.tx("Peg-out request TX", txHash)
        Console.info("PegOut UTxO", s"$txHash#${if outIdx >= 0 then outIdx else 0}")
        Console.info("peg_out address", pegOutAddress.encode.getOrElse("?"))
        Console.info("locked fBTC (sat)", amountSat.toString)
        Console.separator()
        0
    }
}
