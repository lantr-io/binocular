package binocular.attack

import binocular.oracle.*
import binocular.oracle.ForkTree.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.ByteString

/** Resolves the `--parent` selector to a fork anchor and its traversal context. */
object ForkPoint {

    /** Ordered best-chain blocks (confirmed tip → best tip). */
    private def bestChainBlocks(tree: ForkTree): scala.List[BlockSummary] = tree match
        case Blocks(blocks, _, next) => blocks.toScalaList ++ bestChainBlocks(next)
        case Fork(left, right) =>
            val (lw, _, _) = BitcoinValidator.bestChainPath(left, 0, 0)
            val (rw, _, _) = BitcoinValidator.bestChainPath(right, 0, 0)
            if lw >= rw then bestChainBlocks(left) else bestChainBlocks(right)
        case End => scala.Nil

    /** Parse the `--parent` value into either an integer depth or a 32-byte hash. */
    private def parseSelector(parent: String): Either[BigInt, ByteString] =
        parent.trim match
            // A 64-char all-digit string would parse as a (huge) depth and harmlessly fall through to the confirmed-tip fallback; hashes are non-decimal in practice.
            case s if s.nonEmpty && s.forall(_.isDigit) => Left(BigInt(s))
            case s if s.length == 64 =>
                Right(ByteString.fromArray(scalus.utils.Hex.hexToBytes(s)))
            case s =>
                throw new IllegalArgumentException(
                  s"Invalid --parent value: '$s' (expected integer depth or 64-hex block hash)"
                )

    /** Resolve to `(parentPath, anchorHashOption)`.
      *   - Empty path + None → attach at the confirmed tip.
      *   - Non-empty path → attach at the block reachable by that path.
      *
      * Depth `P`: 0 = best-chain tip, P back from tip along the best chain. Hash: located anywhere
      * in the tree (error if absent). Overflow / empty tree → confirmed tip.
      */
    def resolve(tree: ForkTree, parent: String): (PList[BigInt], scala.Option[ByteString]) = {
        if tree == ForkTree.End then return (PList.Nil, scala.None)
        parseSelector(parent) match
            case Left(depth) =>
                val chain = bestChainBlocks(tree)
                if depth >= BigInt(chain.size) then (PList.Nil, scala.None)
                else
                    val anchor = chain(chain.size - 1 - depth.toInt)
                    tree.findPathToHash(anchor.hash) match
                        case scala.Some(p) => (p, scala.Some(anchor.hash))
                        case scala.None    => (PList.Nil, scala.None)
            case Right(hash) =>
                tree.findPathToHash(hash) match
                    case scala.Some(p) => (p, scala.Some(hash))
                    case scala.None =>
                        throw new IllegalArgumentException(
                          s"--parent hash not found in fork tree: ${hash.toHex}"
                        )
    }

    /** Traversal context at the parent reached by `path`, accumulating blocks the same way
      * `validateAndInsert` does. Empty path → the confirmed-tip ctx.
      */
    def ctxAtPath(
        tree: ForkTree,
        path: PList[BigInt],
        rootCtx: TraversalCtx,
        powLimit: BigInt
    ): TraversalCtx = path match
        case PList.Nil => rootCtx
        case PList.Cons(h, tail) =>
            tree match
                case Blocks(blocks, _, next) =>
                    def loop(
                        count: BigInt,
                        remaining: PList[BlockSummary],
                        ctx: TraversalCtx
                    ): TraversalCtx =
                        remaining match
                            case PList.Nil =>
                                // pass-through: all blocks consumed, recurse into subtree
                                ctxAtPath(next, tail, ctx, powLimit)
                            case PList.Cons(b, t) =>
                                val ctx2 = BitcoinValidator.accumulateBlock(ctx, b, powLimit)
                                if count == h then ctx2 // parent block found
                                else loop(count + 1, t, ctx2)
                    loop(0, blocks, rootCtx)
                case Fork(left, right) =>
                    if h == BitcoinValidator.LeftFork then ctxAtPath(left, tail, rootCtx, powLimit)
                    else ctxAtPath(right, tail, rootCtx, powLimit)
                case End => rootCtx
}
