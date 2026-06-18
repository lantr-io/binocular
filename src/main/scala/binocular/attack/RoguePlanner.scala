package binocular.attack

import binocular.bitcoin.{BitcoinHelpers, BitcoinRpc}
import binocular.cli.{UpdatePlan, UpdatePlanner}
import binocular.oracle.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

/** Eve. Ignores Bitcoin; submits mined rogue blocks into the oracle fork tree.
  *
  * @param nowProvider
  *   injectable clock (seconds) for testing; defaults to wall clock.
  */
class RoguePlanner(
    parent: String,
    rogueSprint: Int,
    blockSpacing: BigInt,
    nowProvider: () => BigInt = () => BigInt(System.currentTimeMillis() / 1000)
) extends UpdatePlanner {

    // Eve's branch tip carried across cycles: hash for locating it in the read-back
    // tree, ctx for cheap extension without re-walking the tree, cumulative work.
    private var eveTipHash: scala.Option[ByteString] = scala.None
    private var eveTipCtx: scala.Option[TraversalCtx] = scala.None
    private var eveChainwork: BigInt = 0

    /** Mine up to `count` blocks chained off `startCtx`, respecting the +2h window. */
    private def mineChain(
        startCtx: TraversalCtx,
        count: Int,
        now: BigInt,
        params: BitcoinValidatorParams
    ): (scala.List[BlockHeader], TraversalCtx, BigInt) = {
        val headers = scala.collection.mutable.ListBuffer.empty[BlockHeader]
        var ctx = startCtx
        var work = BigInt(0)
        var i = 0
        var stop = false
        while i < count && !stop do {
            val ceiling = now + BitcoinHelpers.MaxFutureBlockTime
            // Safe slot pre-check: RogueMiner picks (parentTs + blockSpacing).max(mtp+1); since mtp <= parentTs (median of last 11 <= newest), that never exceeds parentTs + blockSpacing, so this bound matches.
            val nextTs = (ctx.timestamps.head + blockSpacing).max(0)
            if nextTs > ceiling then stop = true // out of timestamp slots for now
            else {
                val mined = RogueMiner.mineBlock(ctx, now, blockSpacing, params)
                headers += mined.header
                work += BitcoinHelpers.calculateBlockProof(
                  BitcoinHelpers.compactBitsToTarget(mined.header.bits)
                )
                ctx = mined.ctxAfter
                i += 1
            }
        }
        (headers.toList, ctx, work)
    }

    override def planUpdate(
        rpc: BitcoinRpc,
        chainState: ChainState,
        mpf: OffChainMPF,
        validityTime: BigInt,
        params: BitcoinValidatorParams
    ): UpdatePlan = {
        val now = nowProvider()

        // Locate Eve's branch tip in the freshly read tree (after a prior submission
        // landed). If we have never mined, or our tip isn't in the tree yet, start
        // from the resolved --parent anchor.
        val (parentPath, startCtx) =
            eveTipHash.flatMap(h => chainState.forkTree.findPathToHash(h).map(p => (h, p))) match {
                case scala.Some((_, tipPath)) =>
                    // Extend our own tip. Prefer the cached ctx; fall back to a tree walk.
                    val ctx = eveTipCtx.getOrElse(
                      ForkPoint.ctxAtPath(
                        chainState.forkTree,
                        tipPath,
                        chainState.ctx,
                        params.powLimit
                      )
                    )
                    (tipPath, ctx)
                case scala.None =>
                    val (p, _) = ForkPoint.resolve(chainState.forkTree, parent)
                    (
                      p,
                      ForkPoint.ctxAtPath(chainState.forkTree, p, chainState.ctx, params.powLimit)
                    )
            }

        // First batch mines the sprint; later cycles add one block per slot.
        val count = if eveTipHash.isEmpty then rogueSprint else 1
        val (headers, endCtx, work) = mineChain(startCtx, count, now, params)

        if headers.isEmpty then {
            UpdatePlan(
              PList.Nil,
              parentPath,
              s"rogue: no timestamp slot (tip height=${startCtx.height}, " +
                  s"chainwork=$eveChainwork) — waiting for +2h window to slide"
            )
        } else {
            eveTipHash = scala.Some(BitcoinHelpers.blockHeaderHash(headers.last))
            eveTipCtx = scala.Some(endCtx)
            eveChainwork += work
            val bestTip =
                chainState.forkTree.bestChainTipHash.map(_.toHex.take(12)).getOrElse("<confirmed>")
            UpdatePlan(
              PList.from(headers),
              parentPath,
              s"rogue: mined ${headers.size} block(s) → tip height=${endCtx.height}, " +
                  s"branch chainwork=$eveChainwork | oracle best tip=$bestTip"
            )
        }
    }
}
