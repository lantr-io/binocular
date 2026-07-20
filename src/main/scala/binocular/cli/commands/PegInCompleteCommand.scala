package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, LedgerToPlutusTranslation, Script, ScriptHash, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.utils.Hex.hexToBytes

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** B1: build + submit the peg-in completion tx — mint `peg_in_amount` fBTC to `--recipient` and
  * record the peg-in in the completed-peg-ins MPF. See [[PegInCompleteTx]] for the tx shape and the
  * on-chain requirements (`peg_in.ak::withdraw(CompletePegIn)`).
  *
  * Instead of re-proving the Treasury Movement, this references the **Confirmed TM UTxO** produced
  * by `confirm-tmtx` at the [[TreasuryMovementValidator]] address (authenticated by the TM NFT). It
  * locates the Confirmed UTxO whose `swept_peg_in_utxo_ids` contains this peg-in, reads `btc_txid`
  * from its datum, and binds the depositor signature + fBTC output to `--recipient`.
  *
  * Permissionless except for the depositor's BIP340 Schnorr signature, which is produced externally
  * (e.g. with `heimdall/.keys/alice.wif` via `sign-pegin-msg`) and passed via `--signature`. The
  * command prints the exact 32-byte message digest to sign (it binds the same recipient + TM +
  * peg-in).
  *
  * Preconditions (one-time setup): the peg_in withdraw reward cred is registered
  * (`register-bridge-creds`), the TM has been confirmed (`confirm-tmtx`), and `--prior-pegin` is
  * supplied for every earlier completion so the completed-peg-ins MPF reconstructs to the on-chain
  * root.
  */
case class PegInCompleteCommand(
    pirRef: String,
    tmTxId: String,
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

        hexBytes("BTC TM txid", tmTxId, Some(64))
        // The TM txid the depositor expects, in internal (LE) byte order — to cross-check against
        // the Confirmed datum's btc_txid (which `confirm-tmtx` stores LE).
        val expectedTmTxidLE = ByteString.fromArray(tmTxId.hexToBytes.reverse)
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

        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }

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

        // TM-NFT policy = TreasuryMovementValidator hash; both the peg_in 4th param and the marker
        // on the Confirmed TM UTxO this completion references.
        val tmNftPolicyBS = CommandHelpers.tmNftPolicy(config, oracleScriptHash)
        val tmNftPolicy = ScriptHash.fromHex(tmNftPolicyBS.toHex)
        val tmAddress = Address(network, Credential.ScriptHash(tmNftPolicy))

        val oraclePolicyBS = ByteString.fromArray(oracleScriptHash.bytes)
        val pegIn =
            PegInContract(blueprint, oraclePolicyBS, configNftPolicy, configNftAsset, tmNftPolicyBS)
        val cpiContract =
            CompletedPegInsContract(blueprint, configNftPolicy, configNftAsset, cpiRef)
        val cpiPolicy = cpiContract.policyId
        val cpiAsset = AssetName(CompletedPegInsContract.assetName)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy, configNftAsset)

        Console.info("Peg-in policy", pegIn.policyId.toHex)
        Console.info("fBTC policy", bridgedToken.policyId.toHex)
        Console.info("completed-peg-ins policy", cpiPolicy.toHex)
        Console.info("TM validator / NFT policy", tmNftPolicy.toHex)
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

        Console.step(1, "Locating UTxOs (PIR, completed-peg-ins, config, Confirmed TM)")
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

        // Find the Confirmed TM UTxO: carries the TM NFT and a `Confirmed` datum whose
        // swept_peg_in_utxo_ids includes this peg-in. This is the trust anchor — its btc_txid is
        // what the depositor signs over, and peg-in.ak reads the swept membership on-chain.
        // Decode defensively: anyone can park a UTxO carrying a (tmNftPolicy, "") token at the TM
        // address with a non-TmDatum / poison inline datum, and fromData throws on a shape it can't
        // decode. Guard each candidate with Try so one bad UTxO can't crash the command, and capture
        // the decoded btc_txid here so we never re-decode (no second .get on an Option).
        val confirmedHit: Option[(Utxo, ByteString)] =
            provider.findUtxos(tmAddress).await(timeout) match {
                case Left(err) => Console.error(s"Fetching TM UTxOs: $err"); break(1)
                case Right(us) =>
                    us.toList.iterator
                        .filter(_._2.value.hasAsset(tmNftPolicy, AssetName.empty))
                        .flatMap { case (i, o) =>
                            o.inlineDatum
                                .flatMap(d => scala.util.Try(fromData[TmDatum](d)).toOption)
                                .collect {
                                    case TmDatum.Confirmed(txid, swept, _, _, _)
                                        if swept.toScalaList.contains(datum.pegInUtxoId) =>
                                        (Utxo(i, o), txid)
                                }
                        }
                        .nextOption()
            }
        val (confirmedTmUtxo, btcTxidLE) = confirmedHit.getOrElse {
            Console.error(
              s"No Confirmed TM UTxO at $tmAddress sweeps peg-in ${datum.pegInUtxoId.toHex}. " +
                  "Run `confirm-tmtx` for the TM first."
            )
            break(1)
        }
        if btcTxidLE != expectedTmTxidLE then {
            Console.error(
              s"Confirmed TM btc_txid ${btcTxidLE.reverse.toHex} != --tm $tmTxId. " +
                  "Pass the txid of the TM that swept this peg-in."
            )
            break(1)
        }
        Console.info(
          "Confirmed TM UTxO",
          s"${confirmedTmUtxo.input.transactionId.toHex}#${confirmedTmUtxo.input.index}"
        )
        println()

        // --- completed-peg-ins MPF: reconstruct, verify root, produce proofs ---
        Console.step(2, "Reconstructing completed-peg-ins MPF + proofs")
        val cpiDatum = cpiUtxo.output.inlineDatum
            .map(fromData[CompletedPegInsMerkleTreeDatum])
            .getOrElse { Console.error("Completed-peg-ins UTxO has no datum"); break(1) }
        var tree = OffChainMPF.empty
        priorPegins.foreach { k =>
            val kb = hexBytes("--prior-pegin", k, None); tree = tree.insert(kb, kb)
        }
        if tree.rootHash != cpiDatum.root then {
            Console.error(
              s"Reconstructed completed-peg-ins root ${tree.rootHash.toHex} != on-chain ${cpiDatum.root.toHex}. " +
                  "Pass --prior-pegin <pegInUtxoId> for every earlier completion (in insertion order)."
            )
            break(1)
        }
        // proveNonMembership throws if the key is already present, so check first and give a clean
        // message (already completed, or the current id was mistakenly passed as --prior-pegin).
        if tree.get(datum.pegInUtxoId).isDefined then {
            Console.error(
              s"Peg-in ${datum.pegInUtxoId.toHex} is already in the completed-peg-ins tree — " +
                  "already completed, or its id was passed as --prior-pegin."
            )
            break(1)
        }
        val cpiProof = tree.proveNonMembership(datum.pegInUtxoId)
        val cpiNewRoot = tree.insert(datum.pegInUtxoId, datum.pegInUtxoId).rootHash
        println()

        // --- signing message (recipientData was resolved up front, in the recipient guard) ---
        // btc_txid is taken from the Confirmed datum (authoritative, internal/LE byte order).
        val msgPreimage = Builtins.appendByteString(
          BifrostMessages.mintTag,
          Builtins.appendByteString(
            btcTxidLE,
            Builtins.appendByteString(datum.pegInUtxoId, Builtins.serialiseData(recipientData))
          )
        )
        val msgDigest = Builtins.sha2_256(msgPreimage)
        // BIP-322: the depositor signs the ASCII text below from their Taproot wallet
        // (signMessage(text, "bip322-simple")); peg_in.ak verifies it against the beacon output key.
        val signText = BifrostMessages.mintTextPrefix + msgDigest.toHex
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
                          PegInCompleteTx.Inputs(pirUtxo, cpiUtxo, configUtxo, confirmedTmUtxo),
                      datum = datum,
                      recipientAddress = recipientLedger,
                      recipientData = recipientData,
                      signature = sigBytes,
                      completedPegInsProof = cpiProof,
                      completedPegInsNewRoot = cpiNewRoot,
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
        Console.info("new completed-peg-ins root", cpiNewRoot.toHex)
        Console.info("this peg_in_utxo_id (for next --prior-pegin)", datum.pegInUtxoId.toHex)
        Console.separator()
        0
    }
}
