package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.Address
import scalus.cardano.ledger.{
    AssetName,
    Credential,
    LedgerToPlutusTranslation,
    ScriptHash,
    TransactionHash,
    TransactionInput,
    Utxo
}
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

/** F4: build + submit the peg-in completion tx — mint `peg_in_amount` fBTC to `--recipient` and
  * record the peg-in in the completed-peg-ins MPF. See [[PegInCompleteTx]] for the tx shape and the
  * on-chain requirements (`peg_in.ak::withdraw(CompletePegIn)` + the 3 rewarding scripts).
  *
  * Permissionless except for the depositor's BIP340 Schnorr signature, which is produced externally
  * (e.g. with `heimdall/.keys/alice.wif`) and passed via `--signature`. The command prints the exact
  * 32-byte message digest to sign so the off-chain signer binds to the same recipient + TM + peg-in.
  *
  * Preconditions (one-time setup): the 3 withdraw reward creds are registered (`register-bridge-creds`),
  * and the PegInRequest's `source_chain_treasury_utxo_id` is the TM's input-0 outpoint (else the
  * `legit_TM_verifier` fails). `--prior-pegin` must be supplied for every earlier completion so the
  * completed-peg-ins MPF reconstructs to the on-chain root.
  */
case class PegInCompleteCommand(
    pirRef: String,
    tmTxId: String,
    recipient: String,
    signature: String,
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
        val sigBytes = hexBytes("signature", signature, Some(128))
        val pirInput = parseRef("--pir", pirRef)
        val recipientLedger =
            try Address.fromBech32(recipient)
            catch { case e: Exception => Console.error(s"Invalid --recipient: ${e.getMessage}"); break(1) }

        val setup = CommandHelpers.setupOracle(config).valueOr { err => Console.error(err); break(1) }
        val provider = setup.provider
        val network = setup.network
        val oraclePolicyId = setup.script.scriptHash

        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch { case e: Exception => Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1) }

        // --- bridge config / scripts ---
        val configNftPolicy = hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        val configNftAsset = hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)
        val bridgedTokenPolicyBS =
            hexBytes("bridge.bridged-token-policy-id", config.bridge.bridgedTokenPolicyId, Some(56))
        val bridgedTokenAsset =
            AssetName(hexBytes("bridge.bridged-token-asset-name", config.bridge.bridgedTokenAssetName, None))
        if config.bridge.completedPegInsOneShotRef.isEmpty then {
            Console.error("Set binocular.bridge.completed-peg-ins-one-shot-ref (the cpi one-shot from deploy-bridge)")
            break(1)
        }
        val cpiRefInput = parseRef("bridge.completed-peg-ins-one-shot-ref", config.bridge.completedPegInsOneShotRef)
        val cpiRef = TxOutRef(TxId(cpiRefInput.transactionId), cpiRefInput.index)

        val oraclePolicyBS = ByteString.fromArray(oraclePolicyId.bytes)
        val pegIn = PegInContract(blueprint, oraclePolicyBS, configNftPolicy, configNftAsset)
        val cpiContract = CompletedPegInsContract(blueprint, configNftPolicy, configNftAsset, cpiRef)
        val cpiPolicy = cpiContract.policyId
        val cpiAsset = AssetName(CompletedPegInsContract.assetName(cpiRef))
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy, configNftAsset)
        val ownerAuth = PegInDepositorAuthContract
            .makeContract(
              PegInDepositorAuthParams(
                pegInScriptHash = ByteString.fromArray(pegIn.policyId.bytes),
                bridgedTokenPolicyId = bridgedTokenPolicyBS,
                bridgedTokenAssetName = bridgedTokenAsset.bytes
              )
            )
            .script
        val verifier = PegInVerifierContract.contract.script

        Console.info("Peg-in policy", pegIn.policyId.toHex)
        Console.info("fBTC policy", bridgedToken.policyId.toHex)
        Console.info("completed-peg-ins policy", cpiPolicy.toHex)
        println()

        // --- locate the UTxOs ---
        def findWithAsset(addr: Address, pol: ScriptHash, an: AssetName): Option[Utxo] =
            provider.findUtxos(addr).await(timeout) match {
                case Right(us) =>
                    us.toList.collectFirst { case (i, o) if o.value.hasAsset(pol, an) => Utxo(i, o) }
                case Left(_) => None
            }

        Console.step(1, "Locating UTxOs (PIR, completed-peg-ins, config, oracle)")
        val pirUtxo = provider.findUtxos(pegIn.address(network)).await(timeout) match {
            case Right(us) => us.toList.collectFirst { case (i, o) if i == pirInput => Utxo(i, o) }
                    .getOrElse { Console.error(s"PIR $pirRef not found at peg-in address"); break(1) }
            case Left(err) => Console.error(s"Fetching peg-in UTxOs: $err"); break(1)
        }
        val datum = pirUtxo.output.inlineDatum.map(fromData[PegInDatum])
            .getOrElse { Console.error("PIR has no inline PegInDatum"); break(1) }
        if datum.sourceChainTreasuryUtxoId.length != 36 then
            Console.warn(
              s"PIR source_chain_treasury_utxo_id is ${datum.sourceChainTreasuryUtxoId.length} bytes " +
                  "(expected 36 = TM input-0 outpoint). legit_TM_verifier will reject this unless the " +
                  "PIR was minted with the real treasury outpoint."
            )

        val cpiUtxo = findWithAsset(cpiContract.address(network), cpiPolicy, cpiAsset)
            .getOrElse { Console.error("Completed-peg-ins MPF UTxO not found"); break(1) }
        val configAddr = Address(network, Credential.ScriptHash(ScriptHash.fromHex(config.bridge.configNftPolicyId)))
        val configUtxo = findWithAsset(configAddr, ScriptHash.fromHex(config.bridge.configNftPolicyId), AssetName(configNftAsset))
            .getOrElse { Console.error("Config NFT UTxO not found"); break(1) }
        val oracleUtxo =
            try CommandHelpers.findOracleUtxo(provider, oraclePolicyId).await(timeout)
            catch { case e: Exception => Console.error(e.getMessage); break(1) }
        val chainState = CommandHelpers.parseChainState(oracleUtxo)
            .getOrElse { Console.error("Oracle UTxO has no valid ChainState"); break(1) }
        println()

        // --- TM inclusion proof (reuse the oracle confirmed-blocks MPF) ---
        Console.step(2, s"Building TM inclusion proof for $tmTxId")
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val obMpf = CommandHelpers.reconstructMpf(rpc, chainState, config.oracle.startHeight)
            .valueOr { err => Console.error(s"Rebuilding confirmed-blocks MPF: $err"); break(1) }
        val tm = TmProofBundle.produce(rpc, obMpf, tmTxId).await(timeout) match {
            case Right(b)  => b
            case Left(err) => Console.error(s"TM proof: $err"); break(1)
        }
        Console.info("TM in block at index", tm.txIndex)
        println()

        // --- completed-peg-ins MPF: reconstruct, verify root, produce proofs ---
        Console.step(3, "Reconstructing completed-peg-ins MPF + proofs")
        val cpiDatum = cpiUtxo.output.inlineDatum.map(fromData[CompletedPegInsMerkleTreeDatum])
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
        val cpiProof = tree.proveNonMembership(datum.pegInUtxoId)
        val cpiNewRoot = tree.insert(datum.pegInUtxoId, datum.pegInUtxoId).rootHash
        println()

        // --- recipient binding + signing message ---
        val recipientData = LedgerToPlutusTranslation.getAddress(recipientLedger).toData
        // Internal (LE) txid, matching the peg_in_utxo_id convention.
        val tmTxidLE = ByteString.fromArray(tmTxId.hexToBytes.reverse)
        val msgPreimage = Builtins.appendByteString(
          PegInDepositorAuthValidator.mintTag,
          Builtins.appendByteString(
            tmTxidLE,
            Builtins.appendByteString(datum.pegInUtxoId, Builtins.serialiseData(recipientData))
          )
        )
        val msgDigest = Builtins.sha2_256(msgPreimage)
        Console.info("Depositor signs (sha2_256 digest)", msgDigest.toHex)
        Console.info("  preimage", msgPreimage.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (assembled proofs + redeemers; not building tx)")
            break(0)
        }

        Console.step(4, "Building + submitting completion tx")
        val tx =
            try
                PegInCompleteTx
                    .build(
                      provider = provider,
                      sponsor = setup.hdAccount,
                      scripts = PegInCompleteTx.Scripts(pegIn.script, cpiContract.script, bridgedToken.script, ownerAuth, verifier),
                      inputs = PegInCompleteTx.Inputs(pirUtxo, cpiUtxo, oracleUtxo, configUtxo),
                      datum = datum,
                      recipientAddress = recipientLedger,
                      recipientData = recipientData,
                      tmProof = PegInCompleteTx.TmProof(
                        tm.blockHeader,
                        tm.mpfHeaderInclusionProof,
                        tm.rawTxFull,
                        BigInt(tm.txIndex),
                        scalus.cardano.onchain.plutus.prelude.List.from(tm.txInBlockMerklePath)
                      ),
                      completedPegInsProof = cpiProof,
                      completedPegInsNewRoot = cpiNewRoot,
                      treasuryMovementBtcTxid = tmTxidLE,
                      signature = sigBytes,
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
