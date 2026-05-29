package binocular.cli.commands

import binocular.*
import binocular.bitcoin.SimpleBitcoinRpc
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash, TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.fromData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** P5: complete a peg-out — burn the locked fBTC and record the peg-out in the completed-peg-outs
  * MPF, satisfying `peg_out.ak::withdraw(CompletePegOut)`. See [[PegOutCompleteTx]] for the tx
  * shape.
  *
  * The TM is proved INLINE against the Binocular oracle (block-inclusion + tx-merkle proof via
  * [[TmProofBundle]]) — no Confirmed-TM-UTxO reference. Permissionless except `owner_auth`, which
  * the PegOut was locked with as `CardanoSignature(owner pkh)`; the owner (the sponsor, here)
  * signs.
  *
  * Preconditions: the oracle's `confirmed_blocks_root` already contains the TM's block (≥100 confs
  * + 200 min); the peg_out + produced-verifier reward creds are registered
  * (`register-bridge-creds`); and `--prior-pegout` is supplied for every earlier completion so the
  * completed-peg-outs MPF reconstructs to the on-chain root.
  */
case class PegOutCompleteCommand(
    pegOutRef: String,
    tmTxId: String,
    priorPegouts: List[String] = Nil,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Peg-Out Complete (burn fBTC)")
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
        val pegOutInput = parseRef("--pegout", pegOutRef)

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val oracleScriptHash = setup.script.scriptHash
        val oraclePolicyBS = ByteString.fromArray(oracleScriptHash.bytes)

        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }

        val configNftPolicy =
            hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        val configNftAsset =
            hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)
        val bridgedTokenAsset =
            AssetName(
              hexBytes("bridge.bridged-token-asset-name", config.bridge.bridgedTokenAssetName, None)
            )
        if config.bridge.completedPegOutsOneShotRef.forall(_.trim.isEmpty) then {
            Console.error(
              "Set binocular.bridge.completed-peg-outs-one-shot-ref (the cpo one-shot from deploy-bridge)"
            )
            break(1)
        }
        val cpoRefInput =
            parseRef(
              "bridge.completed-peg-outs-one-shot-ref",
              config.bridge.completedPegOutsOneShotRef.get
            )
        val cpoOneShot = TxOutRef(TxId(cpoRefInput.transactionId), cpoRefInput.index)

        val pegOut = PegOutContract(blueprint, oraclePolicyBS, configNftPolicy, configNftAsset)
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy, configNftAsset)
        val cpoContract =
            CompletedPegOutsContract(blueprint, configNftPolicy, configNftAsset, cpoOneShot)
        val cpoPolicy = cpoContract.policyId
        val cpoAsset = AssetName(CompletedPegOutsContract.assetName(cpoOneShot))
        val producedVerifier = PegOutProducedVerifierContract.compiled.script
        val producedVerifierHash = producedVerifier.scriptHash

        Console.info("peg_out withdraw hash", pegOut.policyId.toHex)
        Console.info("fBTC policy", bridgedToken.policyId.toHex)
        Console.info("completed-peg-outs policy", cpoPolicy.toHex)
        Console.info("produced verifier hash", producedVerifierHash.toHex)
        println()

        // --- locate UTxOs ---
        def findWithAsset(addr: Address, pol: ScriptHash, an: AssetName): Option[Utxo] =
            provider.findUtxos(addr).await(timeout) match {
                case Right(us) =>
                    us.toList.collectFirst {
                        case (i, o) if o.value.hasAsset(pol, an) => Utxo(i, o)
                    }
                case Left(_) => None
            }

        Console.step(1, "Locating UTxOs (PegOut, completed-peg-outs, config, oracle)")
        val pegOutUtxo = provider.findUtxos(pegOut.address(network)).await(timeout) match {
            case Right(us) =>
                us.toList
                    .collectFirst { case (i, o) if i == pegOutInput => Utxo(i, o) }
                    .getOrElse {
                        Console.error(s"PegOut $pegOutRef not found at peg_out address"); break(1)
                    }
            case Left(err) => Console.error(s"Fetching peg_out UTxOs: $err"); break(1)
        }
        val datum = pegOutUtxo.output.inlineDatum
            .map(fromData[PegOutDatum])
            .getOrElse { Console.error("PegOut UTxO has no inline PegOutDatum"); break(1) }

        val pegOutAmount = pegOutUtxo.output.value.asset(bridgedToken.policyId, bridgedTokenAsset)
        if pegOutAmount <= 0 then {
            Console.error(s"PegOut UTxO holds no fBTC (${bridgedToken.policyId.toHex})"); break(1)
        }

        val cpoUtxo = findWithAsset(cpoContract.address(network), cpoPolicy, cpoAsset)
            .getOrElse { Console.error("Completed-peg-outs MPF UTxO not found"); break(1) }
        val configAddr =
            Address(
              network,
              Credential.ScriptHash(ScriptHash.fromHex(config.bridge.configNftPolicyId))
            )
        val configUtxo = findWithAsset(
          configAddr,
          ScriptHash.fromHex(config.bridge.configNftPolicyId),
          AssetName(configNftAsset)
        ).getOrElse { Console.error("Config NFT UTxO not found"); break(1) }

        val oracleUtxo =
            try CommandHelpers.findOracleUtxo(provider, oracleScriptHash).await(timeout)
            catch { case e: Exception => Console.error(e.getMessage); break(1) }
        val chainState = CommandHelpers
            .parseChainState(oracleUtxo)
            .getOrElse { Console.error("Oracle UTxO has no valid ChainState datum"); break(1) }
        println()

        // --- TM inclusion proof against the oracle ---
        Console.step(2, s"Building TM inclusion proof for $tmTxId")
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val confirmedMpf = CommandHelpers
            .reconstructMpf(rpc, chainState, config.oracle.startHeight)
            .valueOr { err =>
                Console.error(s"Rebuilding confirmed-blocks MPF: $err"); break(1)
            }
        // The on-chain peg_out.ak proves block-inclusion against the oracle's confirmedBlocksRoot.
        // If our reconstructed mirror's root differs, the inclusion proof is valid only against the
        // mirror and will FAIL on-chain. Surface a mismatch loudly.
        Console.info("reconstructed MPF root", confirmedMpf.rootHash.toHex)
        Console.info("oracle confirmedBlocksRoot", chainState.confirmedBlocksRoot.toHex)
        if confirmedMpf.rootHash != chainState.confirmedBlocksRoot then
            Console.warn(
              "MPF ROOT MISMATCH — reconstructed mirror != oracle root; block-inclusion proof will fail on-chain."
            )
        val tmBundle = TmProofBundle.produce(rpc, confirmedMpf, tmTxId).await(timeout) match {
            case Right(b) => b
            case Left(TmProofBundle.TxNotConfirmed(t)) =>
                Console.error(s"TM $t not confirmed on Bitcoin"); break(1)
            case Left(TmProofBundle.TxNotInBlock(t, bh)) =>
                Console.error(s"TM $t not found in block $bh"); break(1)
            case Left(TmProofBundle.BlockNotConfirmedByOracle(t, bh)) =>
                Console.error(
                  s"TM $t's block $bh is not yet in the oracle confirmed-blocks root — " +
                      "advance the oracle past the TM block (+100 confs) and retry."
                ); break(1)
        }
        Console.info("TM in block @ tx index", tmBundle.txIndex.toString)
        println()

        // peg_out_utxo_id: the Cardano PegOut outpoint (tx_hash(32) ++ vout(4, LE)) — a unique MPF key.
        val voutLE = {
            val v = pegOutInput.index
            Array[Byte](
              (v & 0xff).toByte,
              ((v >> 8) & 0xff).toByte,
              ((v >> 16) & 0xff).toByte,
              ((v >> 24) & 0xff).toByte
            )
        }
        val pegOutUtxoId =
            ByteString.fromHex(pegOutInput.transactionId.toHex) ++ ByteString.fromArray(voutLE)

        // --- completed-peg-outs MPF: reconstruct, verify root, exclusion proof + new root ---
        Console.step(3, "Reconstructing completed-peg-outs MPF + proofs")
        val cpoDatum = cpoUtxo.output.inlineDatum
            .map(fromData[CompletedPegOutsMerkleTreeDatum])
            .getOrElse { Console.error("Completed-peg-outs UTxO has no datum"); break(1) }
        var tree = OffChainMPF.empty
        priorPegouts.foreach { k =>
            val kb = hexBytes("--prior-pegout", k, None); tree = tree.insert(kb, kb)
        }
        if tree.rootHash != cpoDatum.root then {
            Console.error(
              s"Reconstructed completed-peg-outs root ${tree.rootHash.toHex} != on-chain ${cpoDatum.root.toHex}. " +
                  "Pass --prior-pegout <peg_out_utxo_id> for every earlier completion (insertion order)."
            )
            break(1)
        }
        if tree.get(pegOutUtxoId).isDefined then {
            Console.error(s"Peg-out ${pegOutUtxoId.toHex} already completed (in the MPF).");
            break(1)
        }
        val cpoProof = tree.proveNonMembership(pegOutUtxoId)
        val cpoNewRoot = tree.insert(pegOutUtxoId, pegOutUtxoId).rootHash
        println()

        val pegOutInfo = InputCompletePegOut(
          blockHeader = tmBundle.blockHeader,
          blockHeaderInSourceChainInclusionProof = tmBundle.mpfHeaderInclusionProof,
          treasuryMovementRawTx = tmBundle.rawTxStripped,
          treasuryMovementTxIndex = BigInt(tmBundle.txIndex),
          treasuryMovementTxInclusionProof = ScalusList.from(tmBundle.txInBlockMerklePath),
          pegOutUtxoId = pegOutUtxoId,
          pegOutInCompletedPegOutsExclusionProof = cpoProof
        )

        // The produced-verifier withdrawal redeemer — the bare list peg_out.ak cross-checks against
        // the datum + computed amount, and PegOutProducedVerifier independently validates the raw TM.
        val verifierRedeemer: Data = Data.List(
          ScalusList(
            Data.B(datum.sourceChainTreasuryUtxoId),
            Data.B(datum.sourceChainDestinationAddress),
            Data.B(pegOutUtxoId),
            Data.I(BigInt(pegOutAmount)),
            Data.B(tmBundle.rawTxStripped)
          )
        )

        Console.info("fBTC to burn (sat)", pegOutAmount.toString)
        Console.info("peg_out_utxo_id", pegOutUtxoId.toHex)
        Console.info("destination scriptPubKey", datum.sourceChainDestinationAddress.toHex)
        Console.info("new completed-peg-outs root", cpoNewRoot.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (assembled proofs + redeemers; not building tx)")
            break(0)
        }

        Console.step(4, "Building + submitting completion tx")
        def lookupRefUtxo(
            label: String,
            refStr: String,
            script: scalus.cardano.ledger.Script.PlutusV3
        ): Option[Utxo] =
            if refStr.isEmpty then None
            else {
                val ref = parseRef(s"bridge.$label", refStr)
                provider.findUtxo(ref).await(timeout) match {
                    case Right(u) =>
                        val enriched = u.output match {
                            case b: scalus.cardano.ledger.TransactionOutput.Babbage =>
                                b.copy(scriptRef = Some(scalus.cardano.ledger.ScriptRef(script)))
                            case s: scalus.cardano.ledger.TransactionOutput.Shelley =>
                                scalus.cardano.ledger.TransactionOutput.Babbage(
                                  s.address,
                                  s.value,
                                  datumOption = None,
                                  scriptRef = Some(scalus.cardano.ledger.ScriptRef(script))
                                )
                        }
                        Some(Utxo(u.input, enriched))
                    case Left(err) => Console.error(s"Looking up $label ($refStr): $err"); break(1)
                }
            }
        val scriptRefs = PegOutCompleteTx.ScriptRefs(
          pegOut =
              lookupRefUtxo(
                "peg-out-script-ref",
                config.bridge.pegOutScriptRef.getOrElse(""),
                pegOut.script
              ),
          completedPegOuts = lookupRefUtxo(
            "completed-peg-outs-script-ref",
            config.bridge.completedPegOutsScriptRef.getOrElse(""),
            cpoContract.script
          ),
          bridgedToken = lookupRefUtxo(
            "bridged-token-script-ref",
            config.bridge.bridgedTokenScriptRef,
            bridgedToken.script
          )
        )

        // Diagnostic: PEGOUT_DEBUG_BLUEPRINT=<verbose-trace plutus.json> registers a `?`-instrumented
        // peg_out (same params, different hash) under the deployed hash so scalus replays it on
        // failure and emits per-condition traces. No effect on the submitted tx.
        val debugPegOut =
            sys.env.get("PEGOUT_DEBUG_BLUEPRINT").map { p =>
                Console.info("debug peg_out blueprint", p)
                PegOutContract(
                  BifrostBlueprint.fromFile(p),
                  oraclePolicyBS,
                  configNftPolicy,
                  configNftAsset
                ).script
            }

        val tx =
            try
                PegOutCompleteTx
                    .build(
                      provider = provider,
                      sponsor = setup.hdAccount,
                      scripts = PegOutCompleteTx.Scripts(
                        pegOut.script,
                        cpoContract.script,
                        bridgedToken.script,
                        producedVerifier
                      ),
                      scriptRefs = scriptRefs,
                      inputs = PegOutCompleteTx.Inputs(pegOutUtxo, cpoUtxo, configUtxo, oracleUtxo),
                      pegOutInfo = pegOutInfo,
                      completedPegOutsInclusionProof = cpoProof,
                      completedPegOutsNewRoot = cpoNewRoot,
                      verifierRedeemer = verifierRedeemer,
                      pegOutAmount = pegOutAmount,
                      bridgedTokenPolicy = bridgedToken.policyId,
                      bridgedTokenAsset = bridgedTokenAsset,
                      completedPegOutsPolicy = cpoPolicy,
                      completedPegOutsAsset = cpoAsset,
                      pegOutHash = pegOut.policyId,
                      producedVerifierHash = producedVerifierHash,
                      debugPegOut = debugPegOut
                    )
                    .await(timeout)
            catch {
                case e: Exception =>
                    Console.error(s"Building tx: ${e.getMessage}")
                    var c: Throwable = e
                    while c != null do {
                        c match {
                            case pe: scalus.cardano.ledger.PlutusScriptEvaluationException =>
                                Console.error(s"FAILED SCRIPT HASH: ${pe.failedScriptHash.toHex}")
                                Console.error(s"  peg_out=${pegOut.policyId.toHex}")
                                Console.error(s"  completed_peg_outs=${cpoPolicy.toHex}")
                                Console.error(s"  bridged_token=${bridgedToken.policyId.toHex}")
                                Console.error(s"  produced_verifier=${producedVerifierHash.toHex}")
                                Console.error(s"  logs (${pe.logs.length}):")
                                pe.logs.foreach(l => Console.error(s"    | $l"))
                            case _ =>
                        }
                        c = c.getCause
                    }
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
        Console.tx("Peg-out complete TX", txHash)
        Console.info("fBTC burned (sat)", pegOutAmount.toString)
        Console.info("peg_out_utxo_id (for next --prior-pegout)", pegOutUtxoId.toHex)
        Console.separator()
        0
    }
}
