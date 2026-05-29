package binocular.cli

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, ScriptHash, ScriptRef, TransactionOutput, Utxo, Value}
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
    lazy val script = compiled.script
    lazy val scriptAddress = compiled.address(network)
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

    /** Derive the TM-NFT policy id = the binocular [[TreasuryMovementValidator]] script hash,
      * parameterized by the oracle script hash + the TM-control NFT `(policy, name)` from config.
      * This is the policy `peg_in.ak` requires on the referenced Confirmed TM UTxO (its 4th param,
      * `tm_nft_policy_id`), so the peg_in script hash now depends on it — every site that builds
      * [[PegInContract]] must apply the same value.
      */
    /** The bridge's deployed reference-script (CIP-33) UTxO outpoints, from config. These must be
      * kept OUT of oracle/update fee selection: BlockfrostProvider drops their `scriptRef`, so the
      * tx builder under-estimates the fee by the Conway reference-script surcharge →
      * FeeTooSmallUTxO. Pass to `buildOptimalUpdateTransaction(excludeInputs = …)`. Empty entries
      * (no bridge deployed) are skipped.
      */
    def bridgeRefOutpoints(config: BinocularConfig): Set[scalus.cardano.ledger.TransactionInput] = {
        (Set(
          config.bridge.pegInScriptRef,
          config.bridge.bridgedTokenScriptRef,
          config.bridge.completedPegInsScriptRef
        ) ++ config.bridge.pegOutScriptRef ++ config.bridge.completedPegOutsScriptRef).iterator
            .map(_.trim)
            .filter(_.nonEmpty)
            .flatMap { s =>
                s.split("#") match {
                    case Array(h, i) if i.toIntOption.isDefined =>
                        Some(
                          scalus.cardano.ledger.TransactionInput(
                            scalus.cardano.ledger.TransactionHash.fromHex(h),
                            i.toInt
                          )
                        )
                    case _ => None
                }
            }
            .toSet
    }

    /** Query blockfrost for EVERY reference-script (CIP-33) UTxO at `addressBech32` and return their
      * outpoints. More complete than [[bridgeRefOutpoints]]: it catches duplicate/leftover ref-script
      * UTxOs from earlier deploy-script-refs runs that aren't recorded in config (the config-known set
      * alone left the daemon still hitting FeeTooSmallUTxO on an un-recorded ref UTxO). Needed because
      * BlockfrostProvider drops `scriptRef`, so the only way to spot a ref-script UTxO is the
      * `reference_script_hash` field of the raw address-utxos JSON. Best-effort: returns empty on a
      * non-blockfrost backend or any query failure.
      */
    def refScriptOutpoints(
        config: BinocularConfig,
        addressBech32: String
    ): Set[scalus.cardano.ledger.TransactionInput] = {
        if config.cardano.backend.toLowerCase != "blockfrost"
            || config.cardano.blockfrostProjectId.isEmpty
            || addressBech32.isEmpty
        then Set.empty
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
                        .uri(java.net.URI.create(s"$base/addresses/$addressBech32/utxos?count=100&page=$p"))
                        .header("project_id", config.cardano.blockfrostProjectId)
                        .GET()
                        .build()
                    val resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
                    if resp.statusCode() != 200 then Seq.empty else ujson.read(resp.body()).arr.toSeq
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
                              scalus.cardano.ledger.TransactionInput(
                                scalus.cardano.ledger.TransactionHash.fromHex(o("tx_hash").str),
                                o("output_index").num.toInt
                              )
                            )
                        case _ => None
                    }
                }.toSet
            } catch { case _: Exception => Set.empty }
        }
    }

    def tmNftPolicy(config: BinocularConfig, oracleScriptHash: ScriptHash): ByteString = {
        val tmScript = TreasuryMovementContract.contract(
          ByteString.fromArray(oracleScriptHash.bytes),
          ByteString.fromHex(config.bridge.tmControlNftPolicy),
          ByteString.fromHex(config.bridge.tmControlNftName)
        )
        ByteString.fromArray(tmScript.script.scriptHash.bytes)
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

    /** Reconstruct off-chain MPF from Bitcoin RPC by re-inserting all confirmed block hashes. */
    def rebuildMpf(
        rpc: SimpleBitcoinRpc,
        startHeight: Long,
        endHeight: Long,
        expectedRoot: ByteString
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        def loop(heights: List[Long], mpf: OffChainMPF): Future[OffChainMPF] = {
            heights match {
                case Nil => Future.successful(mpf)
                case h :: tail =>
                    for {
                        hashHex <- rpc.getBlockHash(h.toInt)
                        blockHash = ByteString.fromArray(hashHex.hexToBytes.reverse)
                        updatedMpf = mpf.insert(blockHash, blockHash)
                        result <- loop(tail, updatedMpf)
                    } yield result
            }
        }
        val heights = (startHeight to endHeight).toList
        try {
            val rebuilt = loop(heights, OffChainMPF.empty).await(120.seconds)
            if rebuilt.rootHash != expectedRoot then
                Left(
                  s"Rebuilt MPF root does not match on-chain confirmedBlocksRoot. " +
                      s"Expected: ${expectedRoot.toHex}, got: ${rebuilt.rootHash.toHex}. " +
                      s"The oracle's confirmed history (${startHeight}..${endHeight}) commits " +
                      s"to block hashes that differ from this bitcoind's current canonical " +
                      s"chain in that range. Likely cause: a reorg below the oracle's confirmed " +
                      s"tip orphaned one or more committed blocks. The on-chain root is a " +
                      s"hash commitment, so the exact divergence height cannot be recovered " +
                      s"from chain state alone — manual recovery required (re-init the oracle " +
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
                        (forkTree.findTipPath, highestKnown + 1)
                    else
                        // Reorg detected! Walk backwards to find common ancestor
                        Console.logWarn(
                          s"Reorg detected at height $highestKnown: " +
                              s"oracle=${oracleTipHash.toHex}, canonical=${canonicalHash.toHex}"
                        )

                        // Start at highestKnown, not highestKnown - 1: the canonical block at
                        // highestKnown differs from the oracle's *best* tip there, but may
                        // already be present in the tree on a non-best branch (equal-chainwork
                        // fork). Missing that case caused us to re-fetch and re-submit a block
                        // that's already in the tree.
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
