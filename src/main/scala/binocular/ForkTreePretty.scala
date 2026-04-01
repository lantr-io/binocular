package binocular

import binocular.ForkTree.*

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/** Pretty-print a [[ForkTree]] as an ASCII tree with optional ANSI colors.
  *
  * Usage:
  * {{{
  * import binocular.ForkTreePretty.*
  * println(chainState.forkTree.pretty(chainState.ctx.height))
  * println(chainState.forkTree.pretty(chainState.ctx.height, ansi = false)) // no colors
  * }}}
  */
object ForkTreePretty {

    // ANSI codes (matching cli/Console.scala palette)
    private val Reset = "\u001b[0m"
    private val Bold = "\u001b[1m"
    private val Dim = "\u001b[2m"
    private val Red = "\u001b[31m"
    private val Green = "\u001b[32m"
    private val Yellow = "\u001b[33m"
    private val Cyan = "\u001b[36m"
    private val Magenta = "\u001b[35m"
    private val White = "\u001b[37m"

    private val dateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    private def formatTime(epochSeconds: BigInt): String =
        Instant.ofEpochSecond(epochSeconds.toLong).atZone(ZoneId.of("UTC")).format(dateFmt)

    /** Format chainwork in compact scientific notation. */
    private def formatChainwork(cw: BigInt): String = {
        if cw == 0 then "0"
        else
            val s = cw.toString
            val exp = s.length - 1
            if exp <= 3 then s
            else s"${s.head}.${s.slice(1, 3)}e$exp"
        end if
    }

    private case class Colors(
        dim: String,
        bold: String,
        cyan: String,
        green: String,
        yellow: String,
        magenta: String,
        red: String,
        white: String,
        reset: String
    )
    private val ansiColors =
        Colors(Dim, Bold, Cyan, Green, Yellow, Magenta, Red, White, Reset)
    private val noColors = Colors("", "", "", "", "", "", "", "", "")

    extension (tree: ForkTree) {

        /** Pretty-print the fork tree.
          *
          * @param baseHeight
          *   confirmed chain height (fork tree blocks start at baseHeight + 1)
          * @param ansi
          *   whether to include ANSI color codes (default true)
          * @param currentTime
          *   current Cardano time in seconds for aging display (optional)
          * @param confirmedBlocks
          *   number of confirmed blocks stored in MPF (optional)
          */
        def pretty(
            baseHeight: BigInt,
            ansi: Boolean = true,
            currentTime: Option[BigInt] = None,
            confirmedBlocks: Option[Int] = None
        ): String = {
            val c = if ansi then ansiColors else noColors

            // Find the best chain path to mark it
            val (_, bestDepth, bestPath) = BitcoinValidator.bestChainPath(tree, baseHeight, 0)
            val bestPathSet = collectBestPathDirections(tree, bestPath)

            val sb = new StringBuilder
            // Header
            val mpfInfo = confirmedBlocks match
                case Some(n) => s"  ${c.dim}MPF: ${c.cyan}$n${c.dim} confirmed blocks${c.reset}"
                case None    => ""
            sb.append(
              s"${c.dim}confirmed tip:${c.reset} height=${c.cyan}$baseHeight${c.reset}$mpfInfo\n"
            )
            sb.append(s"${c.dim}│${c.reset}\n")

            tree match
                case End =>
                    sb.append(s"${c.dim}└── (empty)${c.reset}\n")
                case _ =>
                    renderNode(sb, tree, baseHeight, "", true, c, bestPathSet, 0, currentTime)

            sb.append(
              s"${c.dim}total: ${c.cyan}${tree.blockCount}${c.dim} blocks, " +
                  s"best tip: height ${c.cyan}$bestDepth${c.reset}\n"
            )
            sb.toString
        }
    }

    /** Collect identity tokens for nodes that are on the best chain path so we can mark them. We
      * use a mutable set of tree node identity hash codes — crude but sufficient for display.
      */
    private def collectBestPathDirections(
        tree: ForkTree,
        bestPath: scalus.cardano.onchain.plutus.prelude.List[BigInt]
    ): Set[Int] = {
        import scalus.cardano.onchain.plutus.prelude.List as ScalusList
        val ids = scala.collection.mutable.Set[Int]()
        ids += System.identityHashCode(tree)

        var current = tree
        var path = bestPath
        var continue = true
        while continue do
            current match
                case Blocks(_, _, next) =>
                    ids += System.identityHashCode(next)
                    current = next
                case Fork(left, right) =>
                    path match
                        case ScalusList.Cons(dir, tail) =>
                            val branch = if dir == 0 then left else right
                            ids += System.identityHashCode(branch)
                            current = branch
                            path = tail
                        case ScalusList.Nil =>
                            continue = false
                case End =>
                    continue = false
        ids.toSet
    }

    /** Render the detail lines for a Blocks node (first/last block info). */
    private def renderBlockDetails(
        sb: StringBuilder,
        blocks: scalus.cardano.onchain.plutus.prelude.List[BlockSummary],
        count: Int,
        firstHeight: BigInt,
        lastHeight: BigInt,
        detailPrefix: String,
        c: Colors,
        currentTime: Option[BigInt]
    ): Unit = {
        val first = blocks.head
        val last = blocks.last

        if count == 1 then
            sb.append(
              s"$detailPrefix${c.cyan}#$firstHeight${c.reset} ${c.magenta}${first.hash.toHex}${c.reset}"
            )
            sb.append(s"  ${c.dim}${formatTime(first.timestamp)} UTC${c.reset}")
            appendAging(sb, blocks, first, c, currentTime)
            sb.append("\n")
        else
            sb.append(
              s"$detailPrefix${c.cyan}#$firstHeight${c.reset} ${c.magenta}${first.hash.toHex}${c.reset}"
            )
            sb.append(s"  ${c.dim}${formatTime(first.timestamp)} UTC${c.reset}")
            appendAging(sb, blocks, first, c, currentTime)
            sb.append("\n")
            sb.append(
              s"$detailPrefix${c.cyan}#$lastHeight${c.reset} ${c.magenta}${last.hash.toHex}${c.reset}"
            )
            sb.append(s"  ${c.dim}${formatTime(last.timestamp)} UTC${c.reset}")
            sb.append("\n")
    }

    private def appendAging(
        sb: StringBuilder,
        blocks: scalus.cardano.onchain.plutus.prelude.List[BlockSummary],
        first: BlockSummary,
        c: Colors,
        currentTime: Option[BigInt]
    ): Unit = {
        currentTime.foreach { now =>
            val oldestAdded = blocks.foldLeft(first.timestamp + first.addedTimeDelta) { (min, b) =>
                val addedTime = b.timestamp + b.addedTimeDelta
                if addedTime < min then addedTime else min
            }
            val ageMinutes = (now - oldestAdded) / 60
            val ageColor =
                if ageMinutes >= 200 then c.green
                else if ageMinutes >= 100 then c.yellow
                else c.dim
            sb.append(s"  ${ageColor}age ${ageMinutes}m${c.reset}")
        }
    }

    private def renderNode(
        sb: StringBuilder,
        tree: ForkTree,
        baseHeight: BigInt,
        prefix: String,
        isLast: Boolean,
        c: Colors,
        bestNodes: Set[Int],
        depth: Int,
        currentTime: Option[BigInt]
    ): Unit = {
        val isBest = bestNodes.contains(System.identityHashCode(tree))
        val connector = if isLast then "└── " else "├── "
        val childPrefix = prefix + (if isLast then "    " else "│   ")

        tree match
            case Blocks(blocks, cw, next) =>
                val count = blocks.size.toInt
                val firstHeight = baseHeight + 1
                val lastHeight = baseHeight + count

                // Main line: connector + block range
                val bestMarker =
                    if isBest then s" ${c.green}${c.bold}★ best${c.reset}" else ""
                val cwStr = formatChainwork(cw)

                sb.append(s"$prefix${c.dim}$connector${c.reset}")
                val blocksLabel = if count == 1 then "block" else "blocks"
                sb.append(
                  s"${c.bold}${c.white}[$count $blocksLabel]${c.reset} " +
                      s"${c.cyan}#$firstHeight${c.reset}${c.dim}..${c.reset}${c.cyan}#$lastHeight${c.reset}"
                )
                sb.append(s"  ${c.dim}cw=${c.reset}$cwStr")
                sb.append(bestMarker)
                sb.append("\n")

                // Detail lines: first and last block
                renderBlockDetails(
                  sb,
                  blocks,
                  count,
                  firstHeight,
                  lastHeight,
                  childPrefix,
                  c,
                  currentTime
                )

                // Recurse into next
                next match
                    case End => () // nothing more
                    case _ =>
                        sb.append(s"$childPrefix${c.dim}│${c.reset}\n")
                        renderNode(
                          sb,
                          next,
                          lastHeight,
                          childPrefix,
                          true,
                          c,
                          bestNodes,
                          depth + 1,
                          currentTime
                        )

            case Fork(left, right) =>
                val leftIsBest = bestNodes.contains(System.identityHashCode(left))
                val rightIsBest = bestNodes.contains(System.identityHashCode(right))

                // Left branch
                val leftLabel =
                    if leftIsBest then s"${c.green}L${c.reset}" else s"${c.dim}L${c.reset}"
                sb.append(s"$prefix${c.dim}├─${c.reset}$leftLabel${c.dim}─${c.reset} ")
                renderBranch(
                  sb,
                  left,
                  baseHeight,
                  prefix + s"${c.dim}│${c.reset}    ",
                  prefix + s"${c.dim}│${c.reset}   ",
                  c,
                  bestNodes,
                  depth,
                  currentTime
                )

                sb.append(s"$prefix${c.dim}│${c.reset}\n")

                // Right branch
                val rightLabel =
                    if rightIsBest then s"${c.green}R${c.reset}" else s"${c.dim}R${c.reset}"
                sb.append(s"$prefix${c.dim}└─${c.reset}$rightLabel${c.dim}─${c.reset} ")
                renderBranch(
                  sb,
                  right,
                  baseHeight,
                  prefix + "     ",
                  prefix + "    ",
                  c,
                  bestNodes,
                  depth,
                  currentTime
                )

            case End => ()
    }

    /** Render a fork branch inline (first Blocks node on same line as L/R label). */
    private def renderBranch(
        sb: StringBuilder,
        tree: ForkTree,
        baseHeight: BigInt,
        childPrefix: String,
        contPrefix: String,
        c: Colors,
        bestNodes: Set[Int],
        depth: Int,
        currentTime: Option[BigInt]
    ): Unit = {
        val isBest = bestNodes.contains(System.identityHashCode(tree))

        tree match
            case Blocks(blocks, cw, next) =>
                val count = blocks.size.toInt
                val firstHeight = baseHeight + 1
                val lastHeight = baseHeight + count

                val bestMarker =
                    if isBest then s" ${c.green}${c.bold}★ best${c.reset}" else ""
                val cwStr = formatChainwork(cw)

                // Inline with the L/R label
                val blocksLabel = if count == 1 then "block" else "blocks"
                sb.append(
                  s"${c.bold}${c.white}[$count $blocksLabel]${c.reset} " +
                      s"${c.cyan}#$firstHeight${c.reset}${c.dim}..${c.reset}${c.cyan}#$lastHeight${c.reset}"
                )
                sb.append(s"  ${c.dim}cw=${c.reset}$cwStr")
                sb.append(bestMarker)
                sb.append("\n")

                renderBlockDetails(
                  sb,
                  blocks,
                  count,
                  firstHeight,
                  lastHeight,
                  s"$contPrefix ",
                  c,
                  currentTime
                )

                next match
                    case End => ()
                    case _ =>
                        sb.append(s"$contPrefix ${c.dim}│${c.reset}\n")
                        renderNode(
                          sb,
                          next,
                          lastHeight,
                          s"$contPrefix ",
                          true,
                          c,
                          bestNodes,
                          depth + 1,
                          currentTime
                        )

            case Fork(_, _) =>
                sb.append("\n")
                renderNode(sb, tree, baseHeight, contPrefix, true, c, bestNodes, depth, currentTime)

            case End =>
                sb.append(s"${c.dim}(empty)${c.reset}\n")
    }
}
