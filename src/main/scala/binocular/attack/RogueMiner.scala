package binocular.attack

import binocular.bitcoin.BitcoinHelpers
import binocular.bitcoin.BitcoinHelpers.*
import binocular.oracle.*
import scalus.uplc.builtin.ByteString
import java.security.MessageDigest
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** Produces valid-proof-of-work block headers that commit to fabricated transactions. Each block is
  * mined to extend a parent [[TraversalCtx]].
  *
  * Performance: the nonce search is a tight `byte[]` loop over a reused `MessageDigest("SHA-256")`,
  * which the JVM routes to the CPU's hardware SHA-256 instruction (ARMv8 crypto ext / x86 SHA-NI).
  * Measured ~12.8 MH/s/thread on an Apple M3 Max, ~124 MH/s across cores → a testnet4
  * min-difficulty block (≈2^32 hashes) in ~24-34s multi-threaded. A pure-Java midstate
  * implementation was measured 5.7x slower (it forfeits the hardware intrinsic), and the Vector API
  * cannot emit the SHA instructions, so neither is used. Throughput is not the bottleneck anyway:
  * the min-difficulty timestamp rule caps Eve at ~1 block / 20 min regardless. See the spec's
  * "Mining performance" note.
  */
object RogueMiner {

    private val cores = Runtime.getRuntime.availableProcessors()

    /** A mined rogue block plus the traversal context after applying it, mining cost detail, and
      * the fabricated peg-in commitment it carries.
      */
    case class MinedBlock(
        header: BlockHeader,
        summary: BlockSummary,
        ctxAfter: TraversalCtx,
        height: BigInt,
        nonce: Long,
        bits: ByteString,
        target: BigInt,
        blockProof: BigInt,
        parentHash: ByteString,
        miningMillis: Long,
        hashesTried: Long,
        commitment: RogueCommitment
    )

    /** Median-time-past of the parent context — mirrors `validateBlock`: sort the newest-11
      * timestamps ascending and take the middle (index 5).
      */
    private def medianTimePast(ctx: TraversalCtx): BigInt =
        BitcoinValidator
            .insertionSort(ctx.timestamps.take(BitcoinHelpers.MedianTimeSpan))
            .at(5) // index 5 = median of 11 timestamps

    /** Write `value` as `len` little-endian bytes into `buf` at `offset`. */
    private def putLE(buf: Array[Byte], offset: Int, len: Int, value: Long): Unit = {
        var v = value
        var i = 0
        while i < len do {
            buf(offset + i) = (v & 0xff).toByte
            v >>= 8
            i += 1
        }
    }

    /** Assemble an 80-byte header (same field layout as buildRawHeader). `prev`, `merkleRoot` and
      * `bits` are placed verbatim (already internal byte order).
      */
    private def buildHeaderBytes(
        version: Long,
        prev: ByteString,
        merkleRoot: ByteString,
        timestamp: Long,
        bits: ByteString,
        nonce: Long
    ): ByteString = {
        val buf = new Array[Byte](80)
        putLE(buf, 0, 4, version)
        System.arraycopy(prev.bytes, 0, buf, 4, 32)
        System.arraycopy(merkleRoot.bytes, 0, buf, 36, 32)
        putLE(buf, 68, 4, timestamp)
        System.arraycopy(bits.bytes, 0, buf, 72, 4)
        putLE(buf, 76, 4, nonce)
        ByteString.fromArray(buf)
    }

    /** 32-byte little-endian representation of `target` (index 0 = least significant), matching how
      * `byteStringToInteger(false, _)` reads a hash.
      */
    private def targetLE(target: BigInt): Array[Byte] = {
        val b = new Array[Byte](32)
        var t = target
        var i = 0
        while i < 32 do { b(i) = (t & 0xff).toByte; t >>= 8; i += 1 }
        b
    }

    /** True iff the raw 32-byte double-SHA output `h`, read little-endian, is `<= target` (given as
      * a 32-byte LE array). No BigInt allocation. Mirrors `validateBlock`'s
      * `byteStringToInteger(false, hash) <= target`.
      */
    private def hashLeTarget(h: Array[Byte], tLE: Array[Byte]): Boolean = {
        var i = 31 // scan MSB-first: index 31 is most significant in the LE representation
        while i >= 0 do {
            val hi = h(i) & 0xff
            val ti = tLE(i) & 0xff
            if hi < ti then return true
            if hi > ti then return false
            i -= 1
        }
        true
    }

    /** Parallel nonce search over `[0, 2^32)`. `template` is the 80-byte header with everything set
      * except the 4 nonce bytes (76..79). Returns the winning nonce, or -1 if the whole space is
      * exhausted (caller then bumps timestamp).
      */
    private def searchNonce(template: Array[Byte], tLE: Array[Byte], threads: Int): Long = {
        require(threads > 0, "threads must be positive")
        val found = new AtomicLong(-1L)
        val failure = new AtomicReference[Throwable](null)
        val ts = (0 until threads).map { tid =>
            val th = new Thread(() => {
                val md = MessageDigest.getInstance("SHA-256")
                val buf = template.clone()
                var n = tid.toLong
                val step = threads.toLong
                while found.get() < 0L && n <= 0xffffffffL do {
                    buf(76) = (n & 0xff).toByte
                    buf(77) = ((n >>> 8) & 0xff).toByte
                    buf(78) = ((n >>> 16) & 0xff).toByte
                    buf(79) = ((n >>> 24) & 0xff).toByte
                    val h1 = md.digest(buf)
                    val h2 = md.digest(h1)
                    if hashLeTarget(h2, tLE) then found.compareAndSet(-1L, n)
                    n += step
                }
            })
            th.setDaemon(true)
            th.setUncaughtExceptionHandler((_, ex) => failure.compareAndSet(null, ex))
            th.start()
            th
        }
        ts.foreach(_.join())
        Option(failure.get()).foreach(ex => throw new RuntimeException("nonce search failed", ex))
        found.get()
    }

    /** Mine one rogue block on top of `parentCtx`.
      *
      * @param now
      *   current wall-clock time (seconds); used for addedTimeDelta and as the +2h future-time
      *   ceiling.
      * @param blockSpacing
      *   desired timestamp gap after the parent (>1200 enables the testnet/regtest min-difficulty
      *   rule).
      * @param threads
      *   nonce-search parallelism (default = CPU count). In regtest the first nonce wins, so this
      *   is irrelevant to tests.
      */
    def mineBlock(
        parentCtx: TraversalCtx,
        now: BigInt,
        blockSpacing: BigInt,
        params: BitcoinValidatorParams,
        threads: Int = cores
    ): MinedBlock = {
        val parentTs = parentCtx.timestamps.head
        val mtp = medianTimePast(parentCtx)
        // To get the testnet4 min-difficulty (powLimit) target, this must be STRICTLY >
        // parentTs + 2*TargetBlockTime (1200s); callers pass blockSpacing > 1200 (CLI default 1201).
        // Must exceed both (parent + spacing) and MTP; capped at now + 2h.
        var timestamp: BigInt = (parentTs + blockSpacing).max(mtp + 1)
        val ceiling = now + BitcoinHelpers.MaxFutureBlockTime
        require(
          timestamp <= ceiling,
          s"No valid timestamp slot: need >$mtp / >${parentTs + blockSpacing} but ceiling is $ceiling"
        )

        val version = 0x20000000L // version bits, comfortably >= 4
        // Build the fabricated peg-in commitment once and mine to its real Merkle root.
        val commit = FakePegIn.commitment(parentCtx.height + 1)
        val merkleRoot = commit.merkleRoot

        // Required difficulty the validator way — handles retarget boundaries
        // (calculateNextWorkRequired) and the min-difficulty exception.
        def bitsFor(ts: BigInt): ByteString =
            BitcoinHelpers.getNextWorkRequired(
              parentCtx.height,
              parentCtx.currentBits,
              parentTs,
              parentCtx.prevDiffAdjTimestamp,
              params.powLimit,
              ts,
              params.allowMinDifficultyBlocks
            )

        var bits = bitsFor(timestamp)
        var tLE = targetLE(compactBitsToTarget(bits))

        // Search the full nonce space in parallel; if exhausted (possible since one
        // success per 2^32 is only ~63% likely per sweep), bump the timestamp (still
        // within the window), recompute bits/target, and rescan.
        val startNanos = System.nanoTime()
        var hashesTried = 0L
        var winning = -1L
        while winning < 0L do {
            val template =
                buildHeaderBytes(
                  version,
                  parentCtx.lastBlockHash,
                  merkleRoot,
                  timestamp.toLong,
                  bits,
                  0L
                ).bytes
            winning = searchNonce(template, tLE, threads)
            hashesTried += (if winning < 0L then 0x100000000L else winning + 1L)
            if winning < 0L then {
                timestamp = timestamp + 1
                require(timestamp <= ceiling, "Exhausted nonce + timestamp window while mining")
                bits = bitsFor(timestamp)
                tLE = targetLE(compactBitsToTarget(bits))
            }
        }
        val miningMillis = (System.nanoTime() - startNanos) / 1_000_000L

        val header = BlockHeader(
          buildHeaderBytes(
            version,
            parentCtx.lastBlockHash,
            merkleRoot,
            timestamp.toLong,
            bits,
            winning
          )
        )
        // Final canonical hash via the same builtin the validator uses.
        val hash = blockHeaderHash(header)
        val summary =
            BlockSummary(hash = hash, timestamp = timestamp, addedTimeDelta = now - timestamp)
        val ctxAfter = BitcoinValidator.accumulateBlock(parentCtx, summary, params.powLimit)
        MinedBlock(
          header = header,
          summary = summary,
          ctxAfter = ctxAfter,
          height = parentCtx.height + 1,
          nonce = winning,
          bits = bits,
          target = compactBitsToTarget(bits),
          blockProof = BitcoinHelpers.calculateBlockProof(compactBitsToTarget(bits)),
          parentHash = parentCtx.lastBlockHash,
          miningMillis = miningMillis,
          hashesTried = hashesTried,
          commitment = commit
        )
    }
}
