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
        given ec: ExecutionContext = binocular.cli.DaemonExecution.ec
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
        val tmScript = TreasuryMovementContract.script(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.tmControlNftPolicy),
          ByteString.fromHex(config.bridge.tmControlNftName)
        )
        val tmAddress = Address(network, Credential.ScriptHash(tmScript.scriptHash))
        // The TM NFT: policy = the validator's own script hash, empty asset name (minted by the
        // validator's mint branch). Only UTxOs carrying it are genuine TM UTxOs.
        val tmNftPolicy = tmScript.scriptHash
        val tmNftAsset = AssetName.empty

        // Operator-declared dead TMs (relay.skip-btc-txids): match on the display (big-endian) btc
        // txid, lower-cased so config casing doesn't matter.
        val skipBtcTxids: Set[String] = config.relay.skipBtcTxids.map(_.toLowerCase).toSet
        if skipBtcTxids.nonEmpty then Console.info("Skip btc txids", skipBtcTxids.mkString(", "))

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        try {
            val info = rpc.getBlockchainInfo().await(30.seconds)
            Console.info("Bitcoin", s"${config.bitcoinNode.url} (${info.chain})")
        } catch {
            case e: Exception => Console.error(s"Bitcoin RPC: ${e.getMessage}"); break(1)
        }
        Console.info("Cardano", config.cardano.network)
        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("TM validator", tmScript.scriptHash.toHex)
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
                                  skipBtcTxids,
                                  processed
                                )
                }

                if dryRun then break(0)
                Thread.sleep(pollInterval * 1000L)
            } catch {
                case e: boundary.Break[?] =>
                    // Control-flow escape (the dry-run `break(0)` above), not an operational error:
                    // `boundary.break` throws a `Break` that extends RuntimeException, so without
                    // this guard the generic handler swallows it, logs a spurious "Error: null",
                    // and exits via `break(1)`. Re-throw so `--dry-run` unwinds cleanly.
                    throw e
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
        tmScript: scalus.cardano.ledger.Script.PlutusV3,
        tmAddress: Address,
        oracleUtxo: Utxo,
        obMpf: scalus.crypto.trie.MerklePatriciaForestry,
        rpc: SimpleBitcoinRpc,
        utxo: Utxo,
        signedBtcTx: ByteString,
        timeout: Duration,
        skipBtcTxids: Set[String],
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

            if skipBtcTxids.contains(displayTxid.toLowerCase) then
                Console.logWarn(s"    $utxoRef: skipped (relay.skip-btc-txids)")
                processed(utxoRef) = "skip:config"
            else {

                // Proof construction fetches the TM's signed BTC tx from the node. If that tx is not
                // fetchable — e.g. a superseded handoff whose input was already spent by a competing
                // TM, so it can never be mined — the RPC throws ("No such mempool or blockchain
                // transaction"). Catch it per-UTxO and skip THIS TM rather than letting the exception
                // abort the whole confirm batch: other Unconfirmed TMs (real, on-chain deposits) must
                // still be processed. Mark it skipped so a permanently-dead TM isn't retried forever.
                val proofResult =
                    try TmProofBundle.produce(rpc, obMpf, displayTxid).await(timeout)
                    catch {
                        case t: Throwable =>
                            processed(utxoRef) = "skip:btc-tx-unavailable"
                            Left(
                              s"BTC tx $displayTxid not on this node (${t.getMessage}) — skipping"
                            )
                    }
                proofResult match {
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
                            Console.logSuccess(s"    [dry-run] would confirm  spent=$utxoRef")
                            confirmSummaryLines(displayTxid, tm, swept, fulfilled, cardanoTx = None)
                                .foreach(l => Console.log(s"    $l"))
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
                                    Console.logSuccess(s"    TM confirmed  spent=$utxoRef")
                                    confirmSummaryLines(
                                      displayTxid,
                                      tm,
                                      swept,
                                      fulfilled,
                                      cardanoTx = Some(hash)
                                    ).foreach(l => Console.log(s"    $l"))
                                    processed(utxoRef) = hash
                                case Left(err) =>
                                    Console.logError(s"    Confirm failed: $err — will retry")
                            }
                }
            }
        }
    }

    /** Max per-entry `pegin=`/`pegout=` lines to emit; a bigger sweep gets a `*_omitted=N` line
      * instead (never a silent cap).
      */
    private val MaxEntryLines = 20

    /** Render a Bitcoin outpoint (36 bytes = 32-byte txid in internal/LE order + 4-byte LE vout) as
      * the human `displaytxid:vout` form.
      */
    private def outpointDisplay(op: ByteString): String = {
        val hex = op.toHex // 72 hex chars
        val txid = hex.substring(0, 64).grouped(2).toList.reverse.mkString // LE -> display (BE)
        val vout = BigInt(hex.substring(64, 72).grouped(2).toList.reverse.mkString, 16)
        s"$txid:$vout"
    }

    /** Grep-able detail lines for a confirmed (or would-confirm) TM: every line carries the
      * `confirm-tm` anchor and `key=value` fields, so `journalctl | grep confirm-tm` yields the
      * whole event and individual facts are machine-extractable. Peg-ins are listed before
      * peg-outs. `cardanoTx` is `None` for the dry-run preview (no submitted tx yet).
      *
      * The recreated Confirmed-TM UTxO is the Confirm tx's first output (`payTo(tmAddress, …)` in
      * [[TreasuryMovementTx.buildAndSubmitConfirm]]), hence `new_state=<hash>#0`.
      */
    private def confirmSummaryLines(
        displayTxid: String,
        tm: TmProofBundle,
        swept: ScalusList[ByteString],
        fulfilled: ScalusList[PegOutEntry],
        cardanoTx: Option[String]
    ): Seq[String] = {
        val pegins = swept.asScala.toIndexedSeq
        val pegouts = fulfilled.asScala.toIndexedSeq
        val satTotal = pegouts.map(_.amount).sum
        val lines = scala.collection.mutable.ListBuffer.empty[String]
        cardanoTx.foreach(h => lines += s"confirm-tm cardano_tx=$h  new_state=$h#0")
        lines += s"confirm-tm btc_tx=$displayTxid  block=${tm.blockHeight}  index=${tm.txIndex}"
        lines += s"confirm-tm pegins=${pegins.size}"
        pegins.take(MaxEntryLines).zipWithIndex.foreach { case (op, i) =>
            lines += s"confirm-tm pegin=$i  outpoint=${outpointDisplay(op)}"
        }
        if pegins.size > MaxEntryLines then
            lines += s"confirm-tm pegins_omitted=${pegins.size - MaxEntryLines}"
        lines += s"confirm-tm pegouts=${pegouts.size}  sat_total=$satTotal"
        pegouts.take(MaxEntryLines).zipWithIndex.foreach { case (e, j) =>
            lines += s"confirm-tm pegout=$j  sat=${e.amount}  spk=${e.scriptPubKey.toHex}"
        }
        if pegouts.size > MaxEntryLines then
            lines += s"confirm-tm pegouts_omitted=${pegouts.size - MaxEntryLines}"
        lines.toList
    }
}
