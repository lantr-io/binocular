package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Confirm posted Treasury Movement (TM) transactions on Cardano — the validated `Unconfirmed ->
  * Confirmed` transition guarded on-chain by [[TreasuryMovementValidator]].
  *
  * Polls the TM validator address for `Unconfirmed` UTxOs (datum `Constr(0, [signed_btc_tx])`, as
  * posted by heimdall's `publish.rs` or `create-tmtx`). For each, once the TM is confirmed on
  * Bitcoin and the block is in the Binocular oracle's confirmed-blocks root, it builds the
  * inclusion proof and submits the Confirm tx: spend the `Unconfirmed` UTxO, reference the oracle,
  * and recreate it with the `Confirmed` datum
  * `{ btc_txid, swept_peg_in_utxo_ids, fulfilled_peg_outs }` that the validator re-parses and
  * verifies on-chain.
  *
  * Unlike the old always-ok scaffold, the datum flip is now only accepted if the Bitcoin
  * confirmation is *proven* against the oracle.
  */
case class ConfirmTmtxCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular TM Confirm (validated)")
        if dryRun then Console.warn("Dry-run mode — will check once and not submit")
        println()
        runConfirm(config)
    }

    private def runConfirm(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val pollInterval = config.relay.pollInterval
        val retryInterval = config.relay.retryInterval
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider: BlockchainProvider = setup.provider
        val network = setup.network
        val hdAccount = setup.hdAccount
        val oraclePolicyId = setup.script.scriptHash
        val oracleScriptHashBS = ByteString.fromArray(oraclePolicyId.bytes)

        // The TM UTxO lives at the validator address (parameterized by the oracle script hash + the
        // TM-control NFT that authenticates the authorized-minter datum).
        val tmScript = TreasuryMovementContract.contract(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.tmControlNftPolicy),
          ByteString.fromHex(config.bridge.tmControlNftName)
        )
        val tmAddress = Address(network, Credential.ScriptHash(tmScript.script.scriptHash))
        // The TM NFT: policy = the validator's own script hash, empty asset name (minted by the
        // validator's mint branch). Only UTxOs carrying it are genuine TM UTxOs.
        val tmNftPolicy = tmScript.script.scriptHash
        val tmNftAsset = AssetName.empty

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        try {
            val info = rpc.getBlockchainInfo().await(30.seconds)
            Console.info("Bitcoin", s"${config.bitcoinNode.url} (${info.chain})")
        } catch {
            case e: Exception => Console.error(s"Bitcoin RPC: ${e.getMessage}"); break(1)
        }
        Console.info("Cardano", config.cardano.network)
        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("TM validator", tmScript.script.scriptHash.toHex)
        Console.info("TM address", tmAddress.encode.getOrElse("?"))
        Console.separator()
        println()

        // utxoRef -> Cardano tx hash (or "dry-run"); avoids reprocessing within a run.
        val processed = scala.collection.mutable.Map[String, String]()

        while true do {
            try {
                // Re-read the oracle each cycle: its confirmed-blocks root advances as Bitcoin does.
                val oracleUtxo =
                    CommandHelpers.findOracleUtxo(provider, oraclePolicyId).await(timeout)
                val chainState = CommandHelpers
                    .parseChainState(oracleUtxo)
                    .getOrElse {
                        Console.logWarn("Oracle UTxO has no valid ChainState");
                        throw new RuntimeException("no chainstate")
                    }
                val obMpf = CommandHelpers
                    .reconstructMpf(rpc, chainState, config.oracle.startHeight)
                    .valueOr { err =>
                        Console.logWarn(s"Rebuilding confirmed-blocks MPF: $err");
                        throw new RuntimeException(err)
                    }

                provider.findUtxos(tmAddress).await(timeout) match {
                    case Left(err) => Console.logWarn(s"UTxO query: $err")
                    case Right(utxos) =>
                        val unconfirmed = utxos.toList
                            .collect { case (in, out) =>
                                out.inlineDatum match
                                    case Some(Data.Constr(0, args))
                                        if args.nonEmpty && out.value.hasAsset(
                                          tmNftPolicy,
                                          tmNftAsset
                                        ) =>
                                        args.head match
                                            case Data.B(tx) => Some((Utxo(in, out), tx))
                                            case _          => None
                                    case _ => None
                            }
                            .flatten
                            .filterNot { case (u, _) =>
                                processed.contains(
                                  s"${u.input.transactionId.toHex}#${u.input.index}"
                                )
                            }

                        if unconfirmed.isEmpty then
                            Console.logInPlace(
                              s"Polling... ${utxos.size} UTxO(s) at TM address, ${processed.size} processed"
                            )
                        else
                            Console.log(s"Found ${unconfirmed.size} Unconfirmed TM UTxO(s)")
                            for (utxo, signedBtcTx) <- unconfirmed do
                                confirmOne(
                                  provider,
                                  hdAccount,
                                  tmScript,
                                  tmAddress,
                                  oracleUtxo,
                                  obMpf,
                                  rpc,
                                  utxo,
                                  signedBtcTx,
                                  timeout,
                                  processed
                                )
                }

                if dryRun then break(0)
                Thread.sleep(pollInterval * 1000L)
            } catch {
                case e: Exception =>
                    Console.logError(s"Error: ${e.getMessage} — retrying in ${retryInterval}s")
                    if dryRun then break(1)
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0
    }

    /** Build + submit the Confirm tx for one Unconfirmed UTxO (or report why it's not ready). */
    private def confirmOne(
        provider: BlockchainProvider,
        hdAccount: scalus.cardano.wallet.hd.HdAccount,
        tmScript: scalus.uplc.PlutusV3[Data => Unit],
        tmAddress: Address,
        oracleUtxo: Utxo,
        obMpf: scalus.crypto.trie.MerklePatriciaForestry,
        rpc: SimpleBitcoinRpc,
        utxo: Utxo,
        signedBtcTx: ByteString,
        timeout: Duration,
        processed: scala.collection.mutable.Map[String, String]
    )(using ExecutionContext): Unit = {
        val utxoRef = s"${utxo.input.transactionId.toHex}#${utxo.input.index}"
        // Parse the (attacker-placeable) datum bytes defensively: getTxHash/allInputOutpoints/
        // allOutputs recurse on a tx-declared count, so a crafted UTxO at the TM address could
        // StackOverflow/OOM. A parse failure is deterministic → mark the UTxO skipped so it neither
        // crashes the watchtower nor is retried forever. RPC errors stay outside this guard (the
        // outer loop retries those).
        val parsed: Option[(ByteString, ScalusList[ByteString], ScalusList[PegOutEntry])] =
            try
                Some(
                  (
                    BitcoinHelpers.getTxHash(signedBtcTx), // internal (LE) — the Confirmed btc_txid
                    TreasuryMovementValidator.allInputOutpoints(signedBtcTx),
                    TreasuryMovementValidator.allOutputs(signedBtcTx)
                  )
                )
            catch {
                case t: Throwable =>
                    Console.logError(
                      s"  $utxoRef: malformed/poison TM bytes — skipping (${t.getClass.getSimpleName})"
                    )
                    processed(utxoRef) = "skip:malformed"
                    None
            }

        parsed.foreach { case (txid, swept, fulfilled) =>
            val displayTxid = txid.reverse.toHex
            Console.log(s"  $utxoRef: TM btc txid=$displayTxid")

            TmProofBundle.produce(rpc, obMpf, displayTxid).await(timeout) match {
                case Left(err) =>
                    Console.log(s"    not ready: $err")
                case Right(tm) =>
                    val redeemer: Data = TmConfirmRedeemer(
                      txIndex = BigInt(tm.txIndex),
                      txMerkleProof = ScalusList.from(tm.txInBlockMerklePath.toList),
                      blockMpfProof = tm.mpfHeaderInclusionProof,
                      blockHeader = binocular.oracle.BlockHeader(tm.blockHeader)
                    ).toData
                    val confirmed: Data =
                        (TmDatum.Confirmed(txid, swept, fulfilled): TmDatum).toData

                    if dryRun then {
                        Console.logSuccess(
                          s"    [dry-run] would Confirm $utxoRef (TM in block at index ${tm.txIndex})"
                        )
                        processed(utxoRef) = "dry-run"
                    } else
                        TreasuryMovementTx.buildAndSubmitConfirm(
                          provider,
                          hdAccount,
                          tmScript,
                          tmAddress,
                          utxo,
                          oracleUtxo,
                          redeemer,
                          confirmed,
                          timeout
                        ) match {
                            case Right(hash) =>
                                Console.logSuccess(s"    Confirmed: Cardano tx=$hash")
                                processed(utxoRef) = hash
                            case Left(err) =>
                                Console.logError(s"    Confirm failed: $err — will retry")
                        }
            }
        }
    }
}
