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
    ): scala.List[RogueMiner.MinedBlock] = {
        val mined = scala.collection.mutable.ListBuffer.empty[RogueMiner.MinedBlock]
        var ctx = startCtx
        var i = 0
        var stop = false
        while i < count && !stop do {
            val ceiling = now + BitcoinHelpers.MaxFutureBlockTime
            // Safe slot pre-check: RogueMiner picks (parentTs + blockSpacing).max(mtp+1); since mtp <= parentTs (median of last 11 <= newest), that never exceeds parentTs + blockSpacing, so this bound matches.
            val nextTs = (ctx.timestamps.head + blockSpacing).max(0)
            if nextTs > ceiling then stop = true // out of timestamp slots for now
            else
                try {
                    val mb = RogueMiner.mineBlock(ctx, now, blockSpacing, params)
                    mined += mb
                    ctx = mb.ctxAfter
                    i += 1
                } catch {
                    // The window above this parent is only a slot or two wide (forking near a tip
                    // whose timestamp runs far ahead of real time) and a difficulty-1 nonce sweep
                    // missed it. Treat that as "no more room this cycle": stop and submit whatever
                    // we already mined instead of erroring the whole daemon cycle. The window widens
                    // as wall-clock advances, so the next cycle makes progress.
                    case _: MiningWindowExhausted =>
                        stop = true
                }
        }
        mined.toList
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
        val minedBlocks = mineChain(startCtx, count, now, params)

        if minedBlocks.isEmpty then {
            UpdatePlan(
              PList.Nil,
              parentPath,
              s"rogue: no timestamp slot (tip height=${startCtx.height}, " +
                  s"chainwork=$eveChainwork) — waiting for +2h window to slide"
            )
        } else {
            val headers = minedBlocks.map(_.header)
            val endCtx = minedBlocks.last.ctxAfter
            val work = minedBlocks.map(_.blockProof).sum

            // Honest best-chain chainwork + display tip, for the per-block report context.
            val oracleBestChainwork =
                BitcoinValidator.bestChainPath(chainState.forkTree, chainState.ctx.height, 0)._1
            val bestTip =
                chainState.forkTree.bestChainTipHash
                    .map(h => ByteString.fromArray(h.bytes.reverse).toHex.take(16))
                    .getOrElse("<confirmed>")

            // Print a detailed report for every mined block.
            minedBlocks.foreach(mb =>
                println(
                  RogueBlockReport.render(mb, now, params.powLimit, oracleBestChainwork, bestTip)
                )
            )

            eveTipHash = scala.Some(BitcoinHelpers.blockHeaderHash(headers.last))
            eveTipCtx = scala.Some(endCtx)
            eveChainwork += work
            UpdatePlan(
              PList.from(headers),
              parentPath,
              s"rogue: mined ${headers.size} block(s) → tip height=${endCtx.height}, " +
                  s"branch chainwork=$eveChainwork | oracle best tip=$bestTip"
            )
        }
    }
}
