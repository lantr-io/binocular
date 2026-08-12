package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, LedgerToPlutusTranslation, Script, ScriptHash, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{fromData, toData}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** B1: build + submit the peg-in completion tx — mint `peg_in_amount` fBTC to `--recipient` and
  * record the peg-in in the completed-peg-ins MPF. See [[PegInCompleteTx]] for the tx shape and the
  * on-chain requirements (`peg_in.ak::withdraw(CompletePegIn)`).
  *
  * Rev 5.4: there is no `Confirmed` TM record any more ([OB-5]). This references the **bridge state
  * singleton** — the UTxO carrying the NFT `(bridge_state_policy, "BSS")`, where
  * `bridge_state_policy` is Config datum field 3 read at runtime ([CPI-10], [PAR-1]) — and proves
  * the sweep with the [CPI-9] MPF membership proof of `(peg_in_utxo_id -> sweeping_tm_input_0)`
  * against the singleton's `spi_root`. The proof and the proven value come from
  * [[SweptPegInsProofService]] ([OB-10]).
  *
  * Permissionless except for the depositor's BIP-322 signature, which is produced externally (e.g.
  * with `heimdall/.keys/alice.wif` via `sign-pegin-msg`) and passed via `--signature`. The command
  * prints the exact 32-byte message digest to sign: `sha2_256("BFR-mint-v1" ‖ peg_in_utxo_id ‖
  * serialiseData(recipient))` ([CPI-3], [OB-11]). The retired preimage also carried the TM txid;
  * the message no longer names a transaction, so the depositor MAY sign before the sweep.
  *
  * Preconditions (one-time setup): the peg_in withdraw reward cred is registered
  * (`register-bridge-creds`), the sweeping TM has been confirmed (so the singleton's `spi_root`
  * covers this deposit), and `--prior-pegin` is supplied for every earlier completion so the
  * completed-peg-ins MPF reconstructs to the on-chain root.
  */
case class PegInCompleteCommand(
    pirRef: String,
    recipient: String,
    signature: Option[String],
    priorPegins: List[String] = Nil,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Peg-In Complete (mint fBTC)")
        if dryRun then Console.warn("Dry-run mode — will assemble but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        def hexBytes(label: String, s: String, expectedChars: Option[Int]): ByteString = {
            val isHex = s.length % 2 == 0 && s.forall(c => "0123456789abcdefABCDEF".contains(c))
            if !isHex || expectedChars.exists(_ != s.length) then {
                val want = expectedChars.fold("even-length hex")(n => s"$n hex chars")
                Console.error(s"Invalid $label: expected $want, got '$s'"); break(1)
            }
            ByteString.fromHex(s)
        }
        def parseRef(label: String, s: String): TransactionInput = s.split("#") match {
            case Array(h, i) if i.toIntOption.isDefined =>
                TransactionInput(TransactionHash.fromHex(h), i.toInt)
            case _ => Console.error(s"Invalid $label: expected TX_HASH#INDEX, got '$s'"); break(1)
        }

        // Validate the signature's format up front if supplied, but don't *require* it yet: the
        // intended flow is `--dry-run` (no signature) to print the digest, sign it, then re-run with
        // --signature. Presence is enforced only for the real (non-dry-run) submit, below.
        val sigBytesOpt: Option[ByteString] = signature.map(hexBytes("signature", _, Some(128)))
        val pirInput = parseRef("--pir", pirRef)
        // Resolve the recipient all the way to its plutus form here, inside the guard, so a
        // bech32-valid but non-payment address (stake/Byron) fails cleanly rather than throwing an
        // uncaught exception later when getAddress runs.
        val (recipientLedger, recipientData) =
            try {
                val addr = Address.fromBech32(recipient)
                (addr, LedgerToPlutusTranslation.getAddress(addr).toData)
            } catch {
                case e: Exception =>
                    Console.error(
                      s"Invalid --recipient (must be a bech32 payment address): ${e.getMessage}"
                    )
                    break(1)
            }

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val oracleScriptHash = setup.script.scriptHash

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)

        // --- bridge config / scripts ---
        val configNftPolicy =
            hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        val configNftAsset =
            hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)
        val bridgedTokenAsset =
            AssetName(
              hexBytes("bridge.bridged-token-asset-name", config.bridge.bridgedTokenAssetName, None)
            )
        if config.bridge.completedPegInsOneShotRef.isEmpty then {
            Console.error(
              "Set binocular.bridge.completed-peg-ins-one-shot-ref (the cpi one-shot from deploy-bridge)"
            )
            break(1)
        }
        val cpiRefInput = parseRef(
          "bridge.completed-peg-ins-one-shot-ref",
          config.bridge.completedPegInsOneShotRef
        )
        val cpiRef = TxOutRef(TxId(cpiRefInput.transactionId), cpiRefInput.index)

        val oraclePolicyBS = ByteString.fromArray(oracleScriptHash.bytes)
        val pegIn =
            PegInContract(blueprint, oraclePolicyBS, configNftPolicy)
        val cpiContract =
            CompletedPegInsContract(blueprint, configNftPolicy, cpiRef)
        val cpiPolicy = cpiContract.policyId
        val cpiAsset = AssetName(CompletedPegInsContract.assetName)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy)

        Console.info("Peg-in policy", pegIn.policyId.toHex)
        Console.info("fBTC policy", bridgedToken.policyId.toHex)
        Console.info("completed-peg-ins policy", cpiPolicy.toHex)
        println()

        // --- locate the UTxOs ---
        def findWithAsset(addr: Address, pol: ScriptHash, an: AssetName): Option[Utxo] =
            provider.findUtxos(addr).await(timeout) match {
                case Right(us) =>
                    us.toList.collectFirst {
                        case (i, o) if o.value.hasAsset(pol, an) => Utxo(i, o)
                    }
                case Left(_) => None
            }

        Console.step(1, "Locating UTxOs (PIR, completed-peg-ins, config, bridge state singleton)")
        val pirUtxo = provider.findUtxos(pegIn.address(network)).await(timeout) match {
            case Right(us) =>
                us.toList
                    .collectFirst { case (i, o) if i == pirInput => Utxo(i, o) }
                    .getOrElse {
                        Console.error(s"PIR $pirRef not found at peg-in address"); break(1)
                    }
            case Left(err) => Console.error(s"Fetching peg-in UTxOs: $err"); break(1)
        }
        val datum = pirUtxo.output.inlineDatum
            .map(fromData[PegInDatum])
            .getOrElse { Console.error("PIR has no inline PegInDatum"); break(1) }

        val cpiUtxo = findWithAsset(cpiContract.address(network), cpiPolicy, cpiAsset)
            .getOrElse { Console.error("Completed-peg-ins MPF UTxO not found"); break(1) }
        val configAddr = Address(
          network,
          Credential.ScriptHash(ScriptHash.fromHex(config.bridge.configNftPolicyId))
        )
        val configUtxo = findWithAsset(
          configAddr,
          ScriptHash.fromHex(config.bridge.configNftPolicyId),
          AssetName(configNftAsset)
        )
            .getOrElse { Console.error("Config NFT UTxO not found"); break(1) }

        // --- the bridge state singleton ([CPI-10], [PAR-1]) ---
        // `bridge_state_policy` and `tm_script_hash` are read from the Config datum at runtime: no
        // reader may hard-code either ([PAR-1]). Typed decode, fields by name ([LIB-1]).
        val configDatum = configUtxo.output.inlineDatum
            .flatMap(d => Try(d.to[ConfigDatum]).toOption)
            .getOrElse {
                Console.error("Config datum does not decode as the rev-5.4 ConfigDatum")
                break(1)
            }
        val bridgeStatePolicy = ScriptHash.fromHex(configDatum.bridgeStatePolicy.toHex)
        val tmScriptHash = ScriptHash.fromHex(configDatum.tmScriptHash.toHex)
        val tmAddress = Address(network, Credential.ScriptHash(tmScriptHash))
        Console.info("bridge state policy (config)", bridgeStatePolicy.toHex)
        Console.info("TM validator (config)", tmScriptHash.toHex)

        val bridgeStateUtxo = findWithAsset(
          Address(network, Credential.ScriptHash(bridgeStatePolicy)),
          bridgeStatePolicy,
          AssetName(TreasuryMovementValidator.BridgeStateAssetName)
        ).getOrElse {
            Console.error(
              s"No UTxO carrying the (${bridgeStatePolicy.toHex}, \"BSS\") NFT — the bridge state " +
                  "singleton does not exist under the policy config field 3 publishes."
            )
            break(1)
        }
        // Decode defensively: anyone can park a UTxO with a poison inline datum at that address,
        // and fromData throws on a shape it cannot decode.
        val bridgeState = bridgeStateUtxo.output.inlineDatum
            .flatMap(d => Try(fromData[BridgeState](d)).toOption)
            .getOrElse {
                Console.error(
                  "The singleton UTxO's datum does not decode as the 4-field BridgeState — " +
                      "refusing to guess at a root ([LIB-1])."
                )
                break(1)
            }
        Console.info(
          "bridge state singleton",
          s"${bridgeStateUtxo.input.transactionId.toHex}#${bridgeStateUtxo.input.index}"
        )
        Console.info("  spi_root", bridgeState.spiRoot.toHex)

        // --- the [CPI-9] sweep proof ([OB-10]) ---
        // Membership of (peg_in_utxo_id -> sweeping_tm_input_0) in the singleton's spi_root, served
        // by the same reconciliation the `spi-proof` command and the REST endpoint use. It refuses
        // rather than guessing when the deposit is not in the CONFIRMED swept set ([SPI-6]).
        val tmAddressBech32 = tmAddress.encode.toOption.getOrElse {
            Console.error(s"Cannot encode the TM address for script hash ${tmScriptHash.toHex}")
            break(1)
        }
        val history = ProviderChainHistory.from(provider, timeout).valueOr { err =>
            Console.error(s"No chain-history backend for the swept-peg-ins proof: $err"); break(1)
        }
        val fetchRawTx = SweptPegInsProofService
            .cardanoFetcher(history, tmAddressBech32, msg => Console.info("spi", msg))
            .valueOr { err =>
                Console.error(s"Reading the TM history at $tmAddressBech32: $err"); break(1)
            }
        // The reconciled swept set, kept whole rather than thrown away after one proof: it is also
        // the only source of the completed-peg-ins VALUES (spec §The two deposit tries — both
        // tries map peg_in_utxo_id to the same sweeping_tm_input_0).
        val spiTrie = SweptPegInsProofService
            .confirmedTrie(bridgeState.spiRoot, bridgeState.treasuryUtxoId, fetchRawTx)
            .valueOr { err =>
                Console.error(err.message); break(1)
            }
        val spi = SweptPegInsProofService
            .proveFrom(spiTrie, datum.pegInUtxoId)
            .valueOr { err =>
                Console.error(err.message); break(1)
            }
        Console.info("sweeping TM input 0", spi.sweepingTmInput0.toHex)
        println()

        // --- completed-peg-ins MPF: reconstruct, verify root, produce proofs ---
        Console.step(2, "Reconstructing completed-peg-ins MPF + proofs")
        val cpiDatum = cpiUtxo.output.inlineDatum
            .map(fromData[CompletedPegInsMerkleTreeDatum])
            .getOrElse { Console.error("Completed-peg-ins UTxO has no datum"); break(1) }
        // Each entry's VALUE is its sweeping_tm_input_0, recovered from the reconciled swept set —
        // never the key. `peg-in.ak` inserts that value, so a key-as-value replay reproduces
        // neither the on-chain root nor the root the validator computes.
        val priorIds = priorPegins.map(k => hexBytes("--prior-pegin", k, None))
        val cpi = PegInCompleteTx
            .completedPegInsUpdate(priorIds, spiTrie, datum.pegInUtxoId)
            .valueOr { err =>
                Console.error(err); break(1)
            }
        if cpi.tree.rootHash != cpiDatum.root then {
            Console.error(
              s"Reconstructed completed-peg-ins root ${cpi.tree.rootHash.toHex} != on-chain ${cpiDatum.root.toHex}. " +
                  "Pass --prior-pegin <pegInUtxoId> for every earlier completion (in insertion order)."
            )
            break(1)
        }
        println()

        // --- signing message (recipientData was resolved up front, in the recipient guard) ---
        // [CPI-3] REVISED: sha2_256(mint_tag ‖ peg_in_utxo_id ‖ serialiseData(recipient)). The TM
        // txid is gone from the preimage — [CPI-9] proves the sweep instead.
        val msgDigest = BifrostMessages.completionDigest(datum.pegInUtxoId, recipientData)
        // BIP-322: the depositor signs the ASCII text below from their Taproot wallet
        // (signMessage(text, "bip322-simple")); peg_in.ak verifies it against the beacon output key.
        val signText = BifrostMessages.completionSignText(msgDigest)
        Console.info("Depositor signs (BIP-322 text)", signText)
        Console.info("  → in a wallet: signMessage(text, \"bip322-simple\")", "")
        Console.info("  digest (for sign-pegin-msg --message)", msgDigest.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (assembled proofs + redeemers; not building tx)")
            break(0)
        }

        val sigBytes = sigBytesOpt.getOrElse {
            Console.error(
              "--signature is required for a real run. Re-run with --dry-run to print the digest, " +
                  "sign it with `sign-pegin-msg`, then pass --signature <64-byte hex>."
            )
            break(1)
        }

        Console.step(3, "Building + submitting completion tx")
        // Look up any configured CIP-33 reference-script UTxOs and enrich them with the actual
        // script bytes (BlockfrostProvider's findUtxo returns scriptRef=None even when the
        // on-chain output carries one — its parseUtxoOutput skips the second /scripts/<h>/cbor
        // round-trip). We have the scripts locally already (we derive them every time from the
        // same blueprint + params the original deploy used), so we attach them directly. Each
        // empty config entry skips that ref → its script falls back to the witness set.
        // Discover the CIP-33 reference-script UTxOs by the script hash each carries, scanning the
        // sponsor wallet where deploy-script-refs publishes them — so the outpoints need not be
        // recorded in config. A script whose hash isn't found falls back to inlining it in the
        // witness set (only viable for small txs). The provider drops scriptRef on the fetched UTxO,
        // so re-attach the reconstructed script for the tx builder.
        val refScriptUtxos =
            CommandHelpers.refScriptUtxosByHash(config, setup.sponsorAddress.encode.getOrElse(""))
        def lookupRefUtxo(
            label: String,
            script: Script.PlutusV3
        ): Option[Utxo] =
            refScriptUtxos.get(script.scriptHash).map { ref =>
                provider.findUtxo(ref).await(timeout) match {
                    case Right(u) =>
                        val enrichedOutput = u.output match {
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
                        Utxo(u.input, enrichedOutput)
                    case Left(err) =>
                        Console.error(
                          s"Looking up $label ref (${ref.transactionId.toHex}#${ref.index}): $err"
                        )
                        break(1)
                }
            }
        val scriptRefs = PegInCompleteTx.ScriptRefs(
          pegIn = lookupRefUtxo("peg_in", pegIn.script),
          completedPegIns = lookupRefUtxo("completed_peg_ins", cpiContract.script),
          bridgedToken = lookupRefUtxo("bridged_token", bridgedToken.script)
        )

        val tx =
            try
                PegInCompleteTx
                    .build(
                      provider = provider,
                      sponsor = setup.hdAccount,
                      scripts = PegInCompleteTx.Scripts(
                        pegIn.script,
                        cpiContract.script,
                        bridgedToken.script
                      ),
                      scriptRefs = scriptRefs,
                      inputs =
                          PegInCompleteTx.Inputs(pirUtxo, cpiUtxo, configUtxo, bridgeStateUtxo),
                      datum = datum,
                      recipientAddress = recipientLedger,
                      recipientData = recipientData,
                      signature = sigBytes,
                      completedPegInsProof = cpi.insertProof,
                      completedPegInsNewRoot = cpi.newRoot,
                      sweepingTmInput0 = spi.sweepingTmInput0,
                      pegInSweptMembershipProof = spi.proof,
                      bridgedTokenPolicy = bridgedToken.policyId,
                      bridgedTokenAsset = bridgedTokenAsset,
                      completedPegInsPolicy = cpiPolicy,
                      completedPegInsAsset = cpiAsset
                    )
                    .await(timeout)
            catch {
                case e: Exception =>
                    Console.error(s"Building tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }

        val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
            case Right(h)  => h
            case Left(err) => Console.error(s"Submit: $err"); break(1)
        }
        val status = provider
            .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 60, delayMs = 2000)
            .await(timeout)
        status match {
            case TransactionStatus.Confirmed =>
            case other                       => Console.error(s"Not confirmed: $other"); break(1)
        }

        println()
        Console.separator()
        Console.tx("Peg-in complete TX", txHash)
        Console.info("fBTC minted (sat)", datum.pegInAmount.toString)
        Console.info("recipient", recipient)
        Console.info("new completed-peg-ins root", cpi.newRoot.toHex)
        Console.info("this peg_in_utxo_id (for next --prior-pegin)", datum.pegInUtxoId.toHex)
        Console.separator()
        0
    }
}
