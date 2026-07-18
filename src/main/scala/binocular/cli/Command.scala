package binocular.cli

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Credential, Script, ScriptHash, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.fromData
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Try
import scalus.utils.await

/** Base trait for all CLI commands
  *
  * Each command implements this trait and provides its execution logic. Commands receive a
  * BinocularConfig loaded at startup.
  */
trait Command {

    /** Execute the command
      *
      * @param config
      *   The loaded BinocularConfig
      * @return
      *   Exit code (0 for success, non-zero for error)
      */
    def execute(config: BinocularConfig): Int
}

/** Common setup context for commands that interact with the oracle on-chain */
case class OracleSetup(
    params: BitcoinValidatorParams,
    compiled: PlutusV3[Data => Unit],
    hdAccount: HdAccount,
    provider: BlockchainProvider,
    network: Network
) {

    /** Deployable oracle script: blueprint-loaded, UPLC-applied — the form whose hash matches
      * on-chain deployments. `compiled` (SIR-applied) is kept for CEK evaluation/debugging only.
      */
    lazy val script: Script.PlutusV3 = BitcoinContract.script(params)
    lazy val scriptAddress =
        Address(network, Credential.ScriptHash(script.scriptHash))
    lazy val scriptAddressBech32 = scriptAddress.encode.get
    lazy val signer = hdAccount.signerForUtxos
    lazy val sponsorAddress = hdAccount.baseAddress(network)
}

/** Represents a validated oracle UTxO with its parsed ChainState */
case class ValidOracleUtxo(
    utxo: Utxo,
    chainState: ChainState
) {
    def txHash: String = utxo.input.transactionId.toHex
    def outputIndex: Int = utxo.input.index
    def utxoRef: String = s"$txHash:$outputIndex"
}

/** Helper utilities for commands */
object CommandHelpers {

    /** Signature of the last equal-work fork reported by [[detectReorgAndComputePath]], so a
      * transient tie (two blocks at the same height with equal chainwork) is logged once per
      * distinct fork instead of on every poll. Cleared when the oracle tip matches Bitcoin's
      * canonical chain again. See the equal-work-sibling branch in `detectReorgAndComputePath`.
      */
    private val lastEqualWorkFork =
        new java.util.concurrent.atomic.AtomicReference[String]("")

    /** Print BitcoinValidatorParams as an aligned summary via Console.info, suitable for use in
      * command startup banners. Field order matches BitcoinValidatorParams declaration so the
      * on-chain parameter set is easy to scan at a glance.
      */
    def printParams(params: BitcoinValidatorParams): Unit = {
        def humanDuration(seconds: BigInt): String = {
            val s = seconds.toLong
            if s < 60 then s"${s}s"
            else if s < 3600 then f"${s / 60.0}%.1fm"
            else if s < 86400 then f"${s / 3600.0}%.2fh"
            else f"${s / 86400.0}%.2fd"
        }
        Console.info(
          "oneShotTxOutRef",
          s"${params.oneShotTxOutRef.id.hash.toHex}#${params.oneShotTxOutRef.idx}"
        )
        Console.info("owner", params.owner.hash.toHex)
        Console.info("powLimit", f"0x${params.powLimit.toString(16)}")
        Console.info("allowMinDifficultyBlocks", params.allowMinDifficultyBlocks)
        Console.info("maturationConfirmations", params.maturationConfirmations)
        Console.info(
          "challengeAging",
          s"${params.challengeAging}s (${humanDuration(params.challengeAging)})"
        )
        Console.info(
          "closureTimeout",
          s"${params.closureTimeout}s (${humanDuration(params.closureTimeout)})"
        )
        Console.info("maxBlocksInForkTree", params.maxBlocksInForkTree)
        Console.info("testingMode", params.testingMode)
    }

    /** Parse UTxO reference string (TX_HASH:OUTPUT_INDEX) */
    def parseUtxo(utxo: String): Either[String, (String, Int)] = {
        val parts = utxo.split(":")
        if parts.length != 2 then {
            Left(s"Invalid UTxO format. Expected: <TX_HASH>:<OUTPUT_INDEX>")
        } else {
            parts(1).toIntOption match {
                case Some(index) => Right((parts(0), index))
                case None        => Left(s"Invalid output index: ${parts(1)}")
            }
        }
    }

    /** Check if a UTxO is an oracle UTxO (has inline datum, no reference script) */
    def isOracleUtxo(utxo: Utxo): Boolean =
        utxo.output.inlineDatum.isDefined && utxo.output.scriptRef.isEmpty

    /** Try to parse ChainState from UTxO's inline datum */
    def parseChainState(utxo: Utxo): Option[ChainState] =
        utxo.output.inlineDatum.flatMap { data =>
            Try {
                fromData[ChainState](data)
            }.toOption
        }

    /** Check if ChainState is valid (has 11 sorted timestamps) */
    def isValidChainState(chainState: ChainState): Boolean = {
        val timestamps = chainState.ctx.timestamps.toScalaList
        timestamps.size >= 11 && timestamps.sliding(2).forall {
            case Seq(a, b) => a >= b
            case _         => true
        }
    }

    /** Try to get a valid oracle UTxO from a raw UTxO */
    def tryValidateOracleUtxo(utxo: Utxo): Option[ValidOracleUtxo] =
        if !isOracleUtxo(utxo) then None
        else
            parseChainState(utxo).filter(isValidChainState).map { chainState =>
                ValidOracleUtxo(utxo, chainState)
            }

    /** Filter list of UTxOs to only valid oracle UTxOs */
    def filterValidOracleUtxos(utxos: List[Utxo]): List[ValidOracleUtxo] =
        utxos.flatMap(tryValidateOracleUtxo)

    /** Find the oracle UTxO by looking for the NFT with the given policy ID.
      *
      * The NFT policy ID equals the script hash. The script address is derived from the policy ID
      * and the provider's network. Returns a failed Future if zero or multiple found.
      */
    def findOracleUtxo(
        provider: BlockchainProvider,
        nftPolicyId: ScriptHash
    )(using ExecutionContext): Future[Utxo] = {
        val scriptAddress =
            Address(
              provider.cardanoInfo.network,
              scalus.cardano.ledger.Credential.ScriptHash(nftPolicyId)
            )
        provider.findUtxos(scriptAddress).flatMap {
            case Left(err) =>
                Future.failed(new RuntimeException(s"Error fetching UTxOs: $err"))
            case Right(utxos) =>
                val matches = utxos.collect {
                    case (input, output) if output.value.hasAsset(nftPolicyId, AssetName.empty) =>
                        Utxo(input, output)
                }.toList
                matches match {
                    case List(utxo) => Future.successful(utxo)
                    case Nil =>
                        Future.failed(
                          new RuntimeException(
                            s"No oracle UTxO with NFT $nftPolicyId found"
                          )
                        )
                    case multiple =>
                        Future.failed(
                          new RuntimeException(
                            s"Multiple oracle UTxOs (${multiple.size}) with NFT $nftPolicyId found"
                          )
                        )
                }
        }
    }

    /** Parse Blockfrost `/addresses/{addr}/utxos` items into the `(reference_script_hash ->
      * outpoint)` pairs of the CIP-33 reference-script UTxOs among them. Items without a
      * `reference_script_hash` (plain wallet UTxOs) are dropped. Duplicate UTxOs carrying the same
      * script hash are all kept, so callers can either dedup by hash (resolution) or take every
      * outpoint (fee exclusion). Pure — the network fetch lives in [[fetchAddressUtxos]].
      */
    def parseRefScriptOutpoints(
        items: Seq[ujson.Value]
    ): Seq[(ScriptHash, TransactionInput)] =
        items.flatMap { o =>
            o.obj.get("reference_script_hash") match {
                case Some(ujson.Str(hash)) =>
                    Some(
                      ScriptHash.fromHex(hash) ->
                          TransactionInput(
                            TransactionHash.fromHex(o("tx_hash").str),
                            o("output_index").num.toInt
                          )
                    )
                case _ => None
            }
        }

    /** Fetch every UTxO JSON object at `addressBech32` from Blockfrost, following pagination.
      * Best-effort: returns empty on a non-blockfrost backend or any query failure.
      */
    def fetchAddressUtxos(config: BinocularConfig, addressBech32: String): Seq[ujson.Value] = {
        if config.cardano.backend.toLowerCase != "blockfrost"
            || config.cardano.blockfrostProjectId.isEmpty
            || addressBech32.isEmpty
        then Seq.empty
        else {
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
                          java.net.URI.create(
                            s"$base/addresses/$addressBech32/utxos?count=100&page=$p"
                          )
                        )
                        .header("project_id", config.cardano.blockfrostProjectId)
                        .GET()
                        .build()
                    val resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
                    if resp.statusCode() != 200 then Seq.empty
                    else ujson.read(resp.body()).arr.toSeq
                }
                def collect(p: Int, acc: Vector[ujson.Value]): Vector[ujson.Value] = {
                    val items = page(p)
                    val next = acc ++ items
                    if items.size < 100 then next else collect(p + 1, next)
                }
                collect(1, Vector.empty)
            } catch { case _: Exception => Seq.empty }
        }
    }

    /** Query blockfrost for EVERY reference-script (CIP-33) UTxO at `addressBech32` and return
      * their outpoints. Catches duplicate/leftover ref-script UTxOs from earlier deploy-script-refs
      * runs (the config-known set alone left the daemon still hitting FeeTooSmallUTxO on an
      * un-recorded ref UTxO). Needed because BlockfrostProvider drops `scriptRef`, so the only way
      * to spot a ref-script UTxO is the `reference_script_hash` field of the raw address-utxos
      * JSON. Best-effort: returns empty on a non-blockfrost backend or any query failure.
      */
    def refScriptOutpoints(
        config: BinocularConfig,
        addressBech32: String
    ): Set[TransactionInput] =
        parseRefScriptOutpoints(fetchAddressUtxos(config, addressBech32)).map(_._2).toSet

    /** Resolve the CIP-33 reference-script UTxOs at `addressBech32` keyed by the script hash each
      * carries — the discovery path that lets the completion transactions attach their heavy
      * scripts by reference without the outpoints being recorded in config. A script hash present
      * on more than one UTxO (leftover from a re-run) collapses to the last seen; any of them is a
      * valid reference input. Best-effort: empty on a non-blockfrost backend or query failure.
      */
    def refScriptUtxosByHash(
        config: BinocularConfig,
        addressBech32: String
    ): Map[ScriptHash, TransactionInput] =
        parseRefScriptOutpoints(fetchAddressUtxos(config, addressBech32)).toMap

    /** Derive the TM-NFT policy id = the binocular [[TreasuryMovementValidator]] script hash,
      * parameterized by the oracle script hash + the TM-control NFT `(policy, name)` from config.
      * This is the policy `peg_in.ak` requires on the referenced Confirmed TM UTxO (its 4th param,
      * `tm_nft_policy_id`), so the peg_in script hash now depends on it — every site that builds
      * [[PegInContract]] must apply the same value.
      */
    def tmNftPolicy(config: BinocularConfig, oracleScriptHash: ScriptHash): ByteString = {
        val tmScript = TreasuryMovementContract.script(
          ByteString.fromArray(oracleScriptHash.bytes),
          ByteString.fromHex(config.bridge.tmControlNftPolicy),
          ByteString.fromHex(config.bridge.tmControlNftName)
        )
        ByteString.fromArray(tmScript.scriptHash.bytes)
    }

    /** Set up all the common oracle infrastructure (params, wallet, provider, compiled contract).
      *
      * Returns Left(error) on any setup failure.
      */
    def setupOracle(
        config: BinocularConfig
    )(using ExecutionContext): Either[String, OracleSetup] = {
        for {
            params <- config.oracle.toBitcoinValidatorParams(config.bitcoinNode.bitcoinNetwork)
            hdAccount <- config.wallet.createHdAccount()
            provider <- config.cardano.createBlockchainProvider()
        } yield {
            val compiled = BitcoinContract.makeContract(params)
            OracleSetup(
              params,
              compiled,
              hdAccount,
              provider,
              config.cardano.scalusNetwork
            )
        }
    }

    /** Find existing reference script UTxO at the script address.
      *
      * The provider does not populate scriptRef when listing UTxOs, so we identify the reference
      * script UTxO as the one at the script address that does NOT carry the oracle NFT (the NFT
      * policy ID equals the script hash). We then reconstruct the output with the known script.
      */
    def findReferenceScriptUtxo(
        provider: BlockchainProvider,
        scriptAddress: Address,
        script: scalus.cardano.ledger.Script,
        timeout: Duration
    )(using ExecutionContext): Option[Utxo] = {
        val nftPolicyId = script.scriptHash
        provider.findUtxos(scriptAddress).await(timeout) match {
            case Right(utxos) =>
                utxos.toList
                    .find { case (_, output) =>
                        !output.value.hasAsset(nftPolicyId, AssetName.empty)
                    }
                    .map { case (input, output) =>
                        Utxo(
                          input,
                          TransactionOutput(
                            address = output.address,
                            value = output.value,
                            datumOption = output.datumOption,
                            scriptRef = Some(ScriptRef(script))
                          )
                        )
                    }
            case Left(_) => None
        }
    }

    /** Reconstruct off-chain MPF from Bitcoin RPC by re-inserting all confirmed block hashes.
      *
      * The range->root walk lives in [[binocular.oracle.BitcoinChainState.mpfForRange]] so this
      * rebuild and oracle init share one implementation and can never diverge.
      */
    def rebuildMpf(
        rpc: BitcoinRpc,
        startHeight: Long,
        endHeight: Long,
        expectedRoot: ByteString
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        try {
            val rebuilt =
                BitcoinChainState.mpfForRange(rpc, startHeight, endHeight).await(120.seconds)
            if rebuilt.rootHash != expectedRoot then
                Left(
                  s"Rebuilt MPF root does not match on-chain confirmedBlocksRoot. " +
                      s"Expected: ${expectedRoot.toHex}, got: ${rebuilt.rootHash.toHex}. " +
                      s"The oracle's confirmed history (${startHeight}..${endHeight}) commits " +
                      s"to block hashes that differ from this bitcoind's current canonical " +
                      s"chain in that range. Likely cause: a reorg below the oracle's confirmed " +
                      s"tip orphaned one or more committed blocks. The on-chain root is a " +
                      s"hash commitment, so the exact divergence height cannot be recovered " +
                      s"from chain state alone – manual recovery required (re-init the oracle " +
                      s"from a current canonical height)."
                )
            else Right(rebuilt)
        } catch {
            case e: Exception => Left(s"Error rebuilding MPF: ${e.getMessage}")
        }
    }

    /** Reconstruct the off-chain MPF by walking the oracle's committed chain *by hash* from its
      * confirmed tip backwards via `previousblockhash`, inserting `count` block hashes.
      *
      * Unlike [[rebuildMpf]] (which walks canonical headers by height), this reproduces the exact
      * set the oracle committed even when a shallow reorg has orphaned one or more committed blocks
      * near the tip — bitcoind still serves orphaned blocks by hash. The result is verified against
      * `expectedRoot`, so a mismatch (committed blocks genuinely unavailable, i.e. an unrecoverable
      * deep reorg) is reported as `Left` rather than silently producing a wrong proof.
      */
    def rebuildMpfFromTip(
        rpc: SimpleBitcoinRpc,
        tipHashLE: ByteString,
        count: Long,
        expectedRoot: ByteString
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        // bitcoind identifies blocks by big-endian display hash; the oracle stores little-endian.
        def displayHex(le: ByteString): String = le.toHex.grouped(2).toList.reverse.mkString
        def loop(curDisplayHex: String, remaining: Long, mpf: OffChainMPF): Future[OffChainMPF] =
            if remaining <= 0 then Future.successful(mpf)
            else
                for {
                    hdr <- rpc.getBlockHeader(curDisplayHex)
                    blockHashLE = ByteString.fromArray(curDisplayHex.hexToBytes.reverse)
                    updated = mpf.insert(blockHashLE, blockHashLE)
                    result <- hdr.previousblockhash match {
                        case Some(prev) => loop(prev, remaining - 1, updated)
                        case scala.None => Future.successful(updated)
                    }
                } yield result
        try {
            val rebuilt = loop(displayHex(tipHashLE), count, OffChainMPF.empty).await(120.seconds)
            if rebuilt.rootHash != expectedRoot then
                Left(
                  s"By-hash MPF reconstruction from the oracle tip does not match the on-chain " +
                      s"confirmedBlocksRoot (expected ${expectedRoot.toHex}, got " +
                      s"${rebuilt.rootHash.toHex}). One or more committed blocks are no longer " +
                      s"retrievable from this bitcoind — manual recovery required."
                )
            else Right(rebuilt)
        } catch {
            case e: Exception => Left(s"Error rebuilding MPF from tip: ${e.getMessage}")
        }
    }

    /** Bitcoin reorged below the oracle's confirmed tip.
      *
      * The oracle's confirmed-state hash at `confirmedHeight` no longer matches the canonical
      * chain. The protocol cannot auto-recover (the on-chain MPF root commits to the orphaned
      * history), so the syncer must halt and the operator must re-init.
      *
      * `deepestConfirmedAncestor`, when present, is the deepest height ≤ `confirmedHeight` whose
      * canonical hash is still present in the off-chain MPF — i.e., the depth at which the
      * orphaning starts. `None` means no canonical hash was found in the MPF within the searched
      * window (reorg is at least `searchedDepth` blocks deep, or beyond the oracle's known
      * history).
      */
    final class DeepReorgException(
        val confirmedHeight: Long,
        val oracleHash: ByteString,
        val canonicalHash: ByteString,
        val deepestConfirmedAncestor: Option[(Long, ByteString)],
        val searchedDepth: Option[Long]
    ) extends RuntimeException(
          DeepReorgException.format(
            confirmedHeight,
            oracleHash,
            canonicalHash,
            deepestConfirmedAncestor,
            searchedDepth
          )
        )

    object DeepReorgException {
        private[cli] def format(
            confirmedHeight: Long,
            oracleHash: ByteString,
            canonicalHash: ByteString,
            deepestConfirmedAncestor: Option[(Long, ByteString)],
            searchedDepth: Option[Long]
        ): String = {
            val ancestorLine = deepestConfirmedAncestor match {
                case Some((h, hash)) =>
                    val orphaned = confirmedHeight - h
                    s"deepest confirmed ancestor still on canonical chain: height $h " +
                        s"(${hash.toHex}); $orphaned confirmed block(s) orphaned"
                case None =>
                    searchedDepth match {
                        case Some(n) =>
                            s"no canonical block within last $n heights matches the " +
                                s"off-chain MPF — reorg deeper than searched window or beyond " +
                                s"oracle's known confirmed history"
                        case None =>
                            "off-chain MPF not consulted (deep reorg detected before MPF " +
                                "reconstruction)"
                    }
            }
            "Deep reorg: oracle's confirmed tip is no longer on Bitcoin's canonical chain. " +
                s"Confirmed height $confirmedHeight: oracle=${oracleHash.toHex}, " +
                s"canonical=${canonicalHash.toHex}. " + ancestorLine + ". " +
                "Manual recovery required (re-init the oracle from a current canonical height)."
        }
    }

    /** Walk down from `confirmedHeight - 1` to `max(0, confirmedHeight - maxLookback)`, returning
      * the deepest height whose canonical hash is present in the off-chain MPF. Bounded so a hard
      * stop happens quickly on misconfigured chains where no ancestor is reachable.
      */
    def findDeepestConfirmedAncestor(
        rpc: BitcoinRpc,
        mpf: OffChainMPF,
        confirmedHeight: Long,
        maxLookback: Long
    )(using ExecutionContext): Option[(Long, ByteString)] = {
        val lowerBound = math.max(0L, confirmedHeight - maxLookback)
        var h = confirmedHeight - 1
        var found: Option[(Long, ByteString)] = scala.None
        while found.isEmpty && h >= lowerBound do {
            val hashHex = rpc.getBlockHash(h.toInt).await(30.seconds)
            val hash = ByteString.fromArray(hashHex.hexToBytes.reverse)
            if mpf.get(hash).isDefined then found = Some((h, hash))
            else h -= 1
        }
        found
    }

    /** Format chainwork compactly in scientific notation, e.g. `1.28e11`. */
    private def fmtWork(cw: BigInt): String =
        if cw <= 0 then cw.toString
        else {
            val s = cw.toString
            val exp = s.length - 1
            if exp <= 3 then s else s"${s.head}.${s.slice(1, 3)}e$exp"
        }

    /** Gather and log enriched reorg diagnostics (depth, timing, winning pool(s), chainwork,
      * `getchaintips` cross-check). Entirely best-effort and failure-isolated: any RPC/parse error
      * is swallowed with a one-line note so observability never aborts reorg recovery. Does not
      * affect the computed `(parentPath, startHeight)`.
      */
    private def emitReorgDiagnostics(
        rpc: BitcoinRpc,
        forkTree: ForkTree,
        oracleTipHash: ByteString,
        highestKnown: Long,
        ancestorHeight: Long,
        ancestorHash: ByteString
    )(using ExecutionContext): Unit =
        Try {
            val orphanedTipTime =
                forkTree.toBlockList.find(_.hash == oracleTipHash).map(_.timestamp.toLong)
            val nowEpoch = System.currentTimeMillis() / 1000
            val report = ReorgDiagnostics.gather(
              rpc,
              ancestorHeight = ancestorHeight,
              ancestorHashLe = ancestorHash.toHex,
              tipHeight = highestKnown,
              orphanedTipHashLe = oracleTipHash.toHex,
              orphanedTipTime = orphanedTipTime,
              nowEpoch = nowEpoch
            )
            renderReorgReport(report)
        }.recover { case e =>
            Console.logWarn(s"(reorg diagnostics unavailable: ${e.getMessage})")
        }

    private[cli] def renderReorgReport(r: ReorgDiagnostics.ReorgReport): Unit = {
        def abbr(h: String): String =
            if h.length > 18 then s"${h.take(10)}…${h.takeRight(6)}" else h
        def winnerLine(w: ReorgDiagnostics.WinnerBlock): String = {
            val md = if w.minDifficulty then " · min-difficulty block" else ""
            s"#${w.height} ${abbr(w.hashLe)}  ${TimeFmt.utc(w.time)} UTC · ${w.pool.display}$md"
        }

        Console.logWarn(
          s"Reorg depth: ${r.depth} block(s) — fork tip ${r.tipHeight} → common ancestor ${r.ancestorHeight}"
        )

        val orphanedTime = r.orphanedTipTime.map(t => s"  ${TimeFmt.utc(t)} UTC").getOrElse("")
        Console.info("orphaned tip", s"${abbr(r.orphanedTipHashLe)}$orphanedTime")

        if r.winners.nonEmpty then {
            if r.winners.size <= 6 then
                r.winners.sortBy(_.height).foreach(w => Console.info("canonical", winnerLine(w)))
            else {
                Console.info("canonical first", winnerLine(r.winners.minBy(_.height)))
                Console.info("canonical tip", winnerLine(r.winners.maxBy(_.height)))
            }
            val countDesc =
                if r.winnersCapped then s"${r.winners.size} of ${r.depth} blocks fetched"
                else s"${r.depth} block(s)"
            Console.info(
              "winners",
              s"$countDesc · pools: ${r.winnerPools.mkString(", ")}"
            )
            val tip = r.winners.maxBy(_.height)
            Console.info(
              "reorg seen",
              s"~${TimeFmt.humanDuration(r.nowEpoch - tip.time)} after the winning block"
            )
        }

        r.workAdvantageOverOrphan.foreach { w =>
            Console.info("work advantage", s"+${fmtWork(w)} over orphaned tip")
        }
        r.canonicalWorkGain.foreach { w =>
            Console.info("canonical work", s"+${fmtWork(w)} since ancestor")
        }

        r.activeTip.foreach { t =>
            Console.info("bitcoind tip", s"height ${t.height} ${abbr(t.hash)}")
        }
        r.competingForkTips.take(3).foreach { t =>
            Console.info(
              "bitcoind fork",
              s"height ${t.height} branchlen ${t.branchlen} (${t.status})"
            )
        }

        // Greppable single-line summary for log scraping.
        val winnerPool = r.winners.maxByOption(_.height).map(_.pool.display).getOrElse("?")
        val minDiff = r.winners.exists(_.minDifficulty)
        Console.log(
          s"""REORG depth=${r.depth} ancestor=${r.ancestorHeight} tip=${r.tipHeight} """ +
              s"""winner_pool="$winnerPool" min_diff=$minDiff"""
        )
    }

    /** Detect a Bitcoin reorg and compute the correct parentPath and block range.
      *
      * Compares the oracle's fork tree tip against Bitcoin's canonical chain. If they diverge,
      * walks backwards from the oracle tip to find the common ancestor, then returns the correct
      * parentPath and start height for submitting the canonical chain's blocks.
      *
      * If the divergence reaches the confirmed tip itself — i.e., no fork-tree block is on
      * canonical and `ctx.lastBlockHash` does not match canonical at `confirmedHeight` — the
      * confirmed history has been orphaned. The protocol cannot auto-recover from that, so this
      * throws [[DeepReorgException]] with diagnostic info pinpointing where the orphaning starts.
      *
      * @return
      *   `(parentPath, startHeight)` — either the tip path with `highestKnown + 1` (no reorg) or
      *   the path to the common ancestor with `ancestor + 1` (reorg detected).
      */
    def detectReorgAndComputePath(
        rpc: BitcoinRpc,
        chainState: ChainState,
        mpf: OffChainMPF,
        deepReorgLookback: Long = 2016
    )(using ExecutionContext): (scalus.cardano.onchain.plutus.prelude.List[BigInt], Long) = {
        import scalus.cardano.onchain.plutus.prelude.List as ScalusList
        val confirmedHeight = chainState.ctx.height.toLong
        val forkTree = chainState.forkTree

        if forkTree == ForkTree.End then
            // Empty fork tree — parent is confirmed tip, start from confirmed + 1
            (ScalusList.Nil, confirmedHeight + 1)
        else
            val highestKnown = forkTree.highestHeight(chainState.ctx.height).toLong

            // Get the hash of the best chain tip in the fork tree
            forkTree.bestChainTipHash match
                case scala.None =>
                    (forkTree.findTipPath, highestKnown + 1)
                case scala.Some(oracleTipHash) =>
                    // Get Bitcoin's canonical block hash at the same height
                    val canonicalHashHex =
                        rpc.getBlockHash(highestKnown.toInt).await(30.seconds)
                    val canonicalHash =
                        ByteString.fromArray(canonicalHashHex.hexToBytes.reverse)

                    if oracleTipHash == canonicalHash then
                        // No reorg — oracle tip matches Bitcoin canonical chain
                        lastEqualWorkFork.set("")
                        (forkTree.findTipPath, highestKnown + 1)
                    else if forkTree.existsHash(canonicalHash) then
                        // Equal-work sibling: bitcoind's canonical tip at `highestKnown` is already
                        // in our fork tree, just not selected as best. Our deterministic tie-break
                        // (equal chainwork → left branch wins, see ForkTree.bestChainTipHash) simply
                        // differs from bitcoind's first-seen choice. This is a transient natural
                        // fork, NOT an actionable reorg: there is nothing above `highestKnown` to
                        // submit until Bitcoin extends one branch, at which point the heavier branch
                        // is fetched and selected normally. Return the path to the canonical sibling
                        // so any higher block builds on it. Log once per distinct fork (not every
                        // poll) — a persistent tie would otherwise spam the full reorg report.
                        val forkSig =
                            s"$highestKnown:${canonicalHash.toHex}:${oracleTipHash.toHex}"
                        if lastEqualWorkFork.getAndSet(forkSig) != forkSig then
                            Console.logWarn(
                              s"Equal-work fork at height $highestKnown: oracle best chose " +
                                  s"${oracleTipHash.toHex}, bitcoind canonical is " +
                                  s"${canonicalHash.toHex}. Both are in the fork tree with equal " +
                                  s"chainwork — waiting for Bitcoin to extend one branch. No action needed."
                            )
                        val siblingPath =
                            forkTree.findPathToHash(canonicalHash).getOrElse(forkTree.findTipPath)
                        (siblingPath, highestKnown + 1)
                    else
                        // Genuine reorg — canonical tip is not in our tree. Walk back to the ancestor.
                        lastEqualWorkFork.set("")
                        Console.logWarn(
                          s"Reorg detected at height $highestKnown: " +
                              s"oracle=${oracleTipHash.toHex}, canonical=${canonicalHash.toHex}"
                        )

                        // The equal-work-sibling case (canonical block already in the tree) is
                        // handled by the branch above, so here the canonical tip is genuinely
                        // absent. Start the walk at highestKnown anyway; the first iteration simply
                        // confirms the canonical tip is missing before stepping back.
                        var searchHeight = highestKnown
                        var ancestorFound = false
                        var ancestorHeight = confirmedHeight
                        var ancestorHash: ByteString = chainState.ctx.lastBlockHash

                        while !ancestorFound && searchHeight > confirmedHeight do {
                            val hashHex =
                                rpc.getBlockHash(searchHeight.toInt).await(30.seconds)
                            val hash =
                                ByteString.fromArray(hashHex.hexToBytes.reverse)

                            if forkTree.existsHash(hash) then {
                                ancestorFound = true
                                ancestorHeight = searchHeight
                                ancestorHash = hash
                            } else searchHeight -= 1
                        }

                        val parentPath = if ancestorHeight == confirmedHeight then {
                            // Fork tree had no block on canonical. Verify the confirmed tip is
                            // itself canonical before returning Nil — otherwise the next fetched
                            // header's prevBlockHash will not match ctx.lastBlockHash and the
                            // syncer would loop forever on "Parent hash mismatch".
                            val confirmedCanonicalHex =
                                rpc.getBlockHash(confirmedHeight.toInt).await(30.seconds)
                            val confirmedCanonical =
                                ByteString.fromArray(confirmedCanonicalHex.hexToBytes.reverse)
                            if chainState.ctx.lastBlockHash != confirmedCanonical then {
                                val deepest = findDeepestConfirmedAncestor(
                                  rpc,
                                  mpf,
                                  confirmedHeight,
                                  deepReorgLookback
                                )
                                deepest match {
                                    case Some((h, _)) =>
                                        Console.logWarn(
                                          s"Reorg depth: ~${highestKnown - h} block(s), " +
                                              s"${confirmedHeight - h} reaching into CONFIRMED " +
                                              s"history (deepest canonical ancestor $h) — " +
                                              s"auto-recovery impossible, manual re-init required"
                                        )
                                    case None =>
                                        Console.logWarn(
                                          s"Reorg depth: ≥ ${highestKnown - confirmedHeight} fork " +
                                              s"block(s) plus an unknown number into CONFIRMED " +
                                              s"history (no canonical ancestor within last " +
                                              s"$deepReorgLookback heights) — manual re-init required"
                                        )
                                }
                                throw new DeepReorgException(
                                  confirmedHeight = confirmedHeight,
                                  oracleHash = chainState.ctx.lastBlockHash,
                                  canonicalHash = confirmedCanonical,
                                  deepestConfirmedAncestor = deepest,
                                  searchedDepth = Some(deepReorgLookback)
                                )
                            }
                            Console.logWarn(
                              s"Common ancestor is confirmed tip at height $confirmedHeight"
                            )
                            ScalusList.Nil
                        } else {
                            Console.logWarn(
                              s"Common ancestor found at height $ancestorHeight"
                            )
                            forkTree.findPathToHash(ancestorHash).getOrElse {
                                throw new RuntimeException(
                                  s"Bug: existsHash found $ancestorHeight but findPathToHash failed"
                                )
                            }
                        }

                        emitReorgDiagnostics(
                          rpc,
                          forkTree,
                          oracleTipHash,
                          highestKnown,
                          ancestorHeight,
                          ancestorHash
                        )

                        (parentPath, ancestorHeight + 1)
    }

    /** Reconstruct off-chain MPF, checking first if the state has only a single block.
      *
      * Throws [[DeepReorgException]] when the oracle's stored hash at `confirmedHeight` does not
      * match this bitcoind's canonical hash at the same height. That guarantees the rebuild would
      * fail, and surfaces the same diagnostic that [[detectReorgAndComputePath]] uses in the
      * steady-state loop.
      */
    def reconstructMpf(
        rpc: SimpleBitcoinRpc,
        chainState: ChainState,
        startHeight: Option[Long]
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        val confirmedHeight = chainState.ctx.height.toLong
        val confirmedCanonicalHex =
            rpc.getBlockHash(confirmedHeight.toInt).await(30.seconds)
        val confirmedCanonical =
            ByteString.fromArray(confirmedCanonicalHex.hexToBytes.reverse)
        val tip = chainState.ctx.lastBlockHash
        val tipIsCanonical = tip == confirmedCanonical

        val initialMpf = OffChainMPF.empty.insert(tip, tip)
        if initialMpf.rootHash == chainState.confirmedBlocksRoot then Right(initialMpf)
        else
            startHeight match {
                case scala.None =>
                    if tipIsCanonical then
                        Left("Previous promotions detected but ORACLE_START_HEIGHT not configured")
                    else
                        throw new DeepReorgException(
                          confirmedHeight = confirmedHeight,
                          oracleHash = tip,
                          canonicalHash = confirmedCanonical,
                          deepestConfirmedAncestor = scala.None,
                          searchedDepth = scala.None
                        )
                case Some(h) if tipIsCanonical =>
                    // Tip is canonical-by-height: fast rebuild from canonical headers.
                    rebuildMpf(rpc, h, confirmedHeight, chainState.confirmedBlocksRoot)
                case Some(h) =>
                    // Tip is NOT canonical-by-height: a reorg orphaned one or more committed
                    // blocks. The oracle's committed chain is still walkable by hash (bitcoind
                    // retains orphans for a shallow reorg). Reconstruct from the tip and verify
                    // against the on-chain root; only a genuine deep reorg (committed blocks
                    // unavailable / root mismatch) is fatal.
                    rebuildMpfFromTip(
                      rpc,
                      tip,
                      confirmedHeight - h + 1,
                      chainState.confirmedBlocksRoot
                    ) match {
                        case Right(m) => Right(m)
                        case Left(_) =>
                            throw new DeepReorgException(
                              confirmedHeight = confirmedHeight,
                              oracleHash = tip,
                              canonicalHash = confirmedCanonical,
                              deepestConfirmedAncestor = scala.None,
                              searchedDepth = scala.None
                            )
                    }
            }
    }
}
