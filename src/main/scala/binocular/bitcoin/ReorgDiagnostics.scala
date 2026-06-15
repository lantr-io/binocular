package binocular.bitcoin

import binocular.*

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import scalus.utils.await

/** Off-chain (JVM-only) reorg diagnostics: enriches a detected Bitcoin reorg with depth, timing,
  * the winning chain's mining pool(s), and a `getchaintips` cross-check.
  *
  * Deliberately NOT a Scalus `@Compile` object — it uses `java.time`, RPC, and string parsing that
  * have no on-chain meaning. Every field is best-effort: a missing block, an RPC timeout, or an
  * unparseable coinbase degrades the affected field to `None`/empty rather than failing the report.
  * Callers should additionally wrap the whole gather in a `Try` so observability can never abort
  * reorg recovery.
  */
object ReorgDiagnostics {

    /** testnet/testnet4 minimum-difficulty target (the "20-minute rule" floor). A winning block at
      * this difficulty is the most common cause of shallow testnet4 reorgs.
      */
    private val MinDifficultyBits = "1d00ffff"

    /** Cap on how many winning blocks we enumerate, so a deep reorg can't issue unbounded RPC. */
    private val MaxWinners = 25

    /** Known coinbase-tag substrings → friendly pool name. Matched case-insensitively against the
      * printable ASCII recovered from the coinbase scriptSig. Best-effort and non-exhaustive;
      * testnet4 miners frequently leave the coinbase untagged or reuse mainnet software tags.
      */
    private val PoolTags: Seq[(String, String)] = Seq(
      "mara_pool" -> "MARA Pool",
      "mara" -> "MARA Pool",
      "ckpool" -> "CKPool",
      "f2pool" -> "F2Pool",
      "viabtc" -> "ViaBTC",
      "antpool" -> "AntPool",
      "foundryusa" -> "Foundry USA",
      "foundry" -> "Foundry USA",
      "spiderpool" -> "SpiderPool",
      "secpool" -> "SECPOOL",
      "ocean.xyz" -> "OCEAN",
      "binance" -> "Binance Pool",
      "luxor" -> "Luxor",
      "braiins" -> "Braiins Pool",
      "slush" -> "Braiins Pool",
      "sbicrypto" -> "SBI Crypto",
      "poolin" -> "Poolin",
      "btc.com" -> "BTC.com",
      "btccom" -> "BTC.com",
      "nicehash" -> "NiceHash",
      "ultimus" -> "Ultimus",
      "bitfarms" -> "Bitfarms",
      "rawpool" -> "Rawpool",
      "whitepool" -> "WhitePool"
    )

    /** Identified miner of a coinbase. `name` is set when a known pool tag matched; otherwise
      * `rawTag` carries the longest readable ASCII run found (if any).
      */
    case class PoolTag(name: Option[String], rawTag: Option[String]) {
        def display: String =
            name.orElse(rawTag.map(t => s""""$t"""")).getOrElse("untagged (likely solo)")
    }

    /** A block on the winning (canonical) chain that replaced an orphaned block. */
    case class WinnerBlock(
        height: Long,
        hashLe: String, // little-endian (oracle/verify display order)
        time: Long,
        pool: PoolTag,
        minDifficulty: Boolean
    )

    case class ReorgReport(
        depth: Long,
        tipHeight: Long,
        ancestorHeight: Long,
        ancestorHashLe: String,
        orphanedTipHashLe: String,
        orphanedTipTime: Option[Long],
        winners: Seq[WinnerBlock],
        winnersCapped: Boolean,
        canonicalWorkGain: Option[BigInt], // chainwork(tip) − chainwork(ancestor)
        workAdvantageOverOrphan: Option[BigInt], // chainwork(tip) − chainwork(orphaned tip)
        activeTip: Option[ChainTip],
        competingForkTips: Seq[ChainTip],
        nowEpoch: Long
    ) {

        /** Distinct pool display names across the winning chain, in first-seen order. */
        def winnerPools: Seq[String] = winners.map(_.pool.display).distinct
    }

    // ---- pure helpers (unit-tested) -------------------------------------------------

    private def isPrintable(b: Int): Boolean = b >= 0x20 && b <= 0x7e

    /** Recover printable ASCII runs (length ≥ 4) from a coinbase scriptSig hex, after stripping the
      * leading BIP34 block-height push. Returns the runs in order.
      */
    def readableRuns(scriptSigHex: String): Seq[String] = {
        val bytes = Try(hexBytes(scriptSigHex)).getOrElse(Array.emptyByteArray)
        if bytes.isEmpty then Seq.empty
        else {
            // First byte is the push length of the serialized block height (BIP34); skip it.
            val pushLen = bytes(0) & 0xff
            val start = math.min(bytes.length, 1 + pushLen)
            val rest = bytes.drop(start)
            val runs = scala.collection.mutable.ListBuffer.empty[String]
            val cur = new StringBuilder
            def flush(): Unit = {
                if cur.length >= 4 then runs += cur.toString.trim
                cur.setLength(0)
            }
            rest.foreach { raw =>
                val b = raw & 0xff
                if isPrintable(b) then cur.append(b.toChar) else flush()
            }
            flush()
            runs.filter(_.nonEmpty).toList
        }
    }

    /** Identify the mining pool from a coinbase scriptSig hex. */
    def identifyPool(scriptSigHex: String): PoolTag = {
        val runs = readableRuns(scriptSigHex)
        val joined = runs.mkString(" ").toLowerCase
        val matched = PoolTags.collectFirst { case (tag, name) if joined.contains(tag) => name }
        matched match {
            case Some(name) => PoolTag(Some(name), None)
            case None       =>
                // Show the longest readable run (containing at least one letter) as a hint.
                val hint = runs
                    .filter(_.exists(_.isLetter))
                    .sortBy(-_.length)
                    .headOption
                PoolTag(None, hint)
        }
    }

    private def hexBytes(hex: String): Array[Byte] = {
        val clean = if hex.length % 2 == 0 then hex else "0" + hex
        clean.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray
    }

    /** Reverse a hex string byte-wise (big-endian ↔ little-endian display order). */
    private def reverseHex(hex: String): String =
        hexBytes(hex).reverse.map(b => f"${b & 0xff}%02x").mkString

    private def parseChainwork(h: BlockHeaderInfo): Option[BigInt] =
        h.chainwork.flatMap(cw => Try(BigInt(cw, 16)).toOption)

    // ---- RPC gather -----------------------------------------------------------------

    /** Gather a [[ReorgReport]]. Inputs come from the oracle/fork-tree side (little-endian hashes,
      * as the oracle stores them); winning-chain facts are fetched from bitcoind.
      *
      * @param orphanedTipHashLe
      *   the oracle's best fork-tip hash (little-endian `.toHex`)
      * @param orphanedTipTime
      *   the orphaned tip's Bitcoin timestamp, sourced from the fork tree (bitcoind may never have
      *   seen the orphaned block)
      */
    def gather(
        rpc: BitcoinRpc,
        ancestorHeight: Long,
        ancestorHashLe: String,
        tipHeight: Long,
        orphanedTipHashLe: String,
        orphanedTipTime: Option[Long],
        nowEpoch: Long
    )(using ExecutionContext): ReorgReport = {
        val depth = tipHeight - ancestorHeight
        val firstWinner = ancestorHeight + 1
        val lastWinner = math.min(tipHeight, firstWinner + MaxWinners - 1)
        val winnersCapped = lastWinner < tipHeight

        val winners: Seq[WinnerBlock] = (firstWinner to lastWinner).flatMap { h =>
            Try {
                val beHash = rpc.getBlockHash(h.toInt).await(30.seconds)
                val block = rpc.getBlock(beHash).await(30.seconds)
                val coinbaseSig =
                    block.tx.headOption.flatMap(_.coinbase).getOrElse("")
                WinnerBlock(
                  height = h,
                  hashLe = reverseHex(beHash),
                  time = block.time,
                  pool = identifyPool(coinbaseSig),
                  minDifficulty = block.bits.equalsIgnoreCase(MinDifficultyBits)
                )
            }.toOption
        }

        // Chainwork comparison (best-effort): how much more work the winning chain carried.
        val tipWork = winners.lastOption.flatMap { w =>
            Try(
              parseChainwork(rpc.getBlockHeader(reverseHex(w.hashLe)).await(30.seconds))
            ).toOption.flatten
        }
        val ancestorWork =
            Try(
              parseChainwork(rpc.getBlockHeader(reverseHex(ancestorHashLe)).await(30.seconds))
            ).toOption.flatten
        val orphanWork =
            Try(
              parseChainwork(rpc.getBlockHeader(reverseHex(orphanedTipHashLe)).await(30.seconds))
            ).toOption.flatten

        val canonicalWorkGain =
            for { t <- tipWork; a <- ancestorWork } yield t - a
        val workAdvantageOverOrphan =
            for { t <- tipWork; o <- orphanWork } yield t - o

        // getchaintips cross-check: confirm the active tip and surface any competing fork bitcoind
        // itself recorded in the affected range.
        // testnet4's getchaintips is littered with dozens of stale tips from the min-difficulty
        // rule across all history. Restrict to forks whose tip falls inside THIS reorg's height
        // window so the cross-check corroborates the event at hand rather than the whole chain.
        val tips: Seq[ChainTip] = Try(rpc.getChainTips().await(30.seconds)).getOrElse(Seq.empty)
        val activeTip = tips.find(_.status == "active")
        val competingForkTips = tips
            .filter { t =>
                t.status != "active" && t.branchlen > 0 &&
                t.height > ancestorHeight && t.height <= tipHeight
            }
            .sortBy(t => (-t.height, -t.branchlen))

        ReorgReport(
          depth = depth,
          tipHeight = tipHeight,
          ancestorHeight = ancestorHeight,
          ancestorHashLe = ancestorHashLe,
          orphanedTipHashLe = orphanedTipHashLe,
          orphanedTipTime = orphanedTipTime,
          winners = winners,
          winnersCapped = winnersCapped,
          canonicalWorkGain = canonicalWorkGain,
          workAdvantageOverOrphan = workAdvantageOverOrphan,
          activeTip = activeTip,
          competingForkTips = competingForkTips,
          nowEpoch = nowEpoch
        )
    }
}
