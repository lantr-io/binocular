package binocular.cli

import binocular.bitcoin.ChainTip
import binocular.bitcoin.ReorgDiagnostics.{PoolTag, ReorgReport, WinnerBlock}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}

/** Exercises the actual log rendering (`CommandHelpers.renderReorgReport`) — the literal lines the
  * operator sees — including the multi-winner and capped branches that a single live reorg can't
  * cover. Captures stdout (binocular.cli.Console routes through `scala.Console.out`).
  */
class ReorgRenderTest extends AnyFunSuite {

    private def capture(body: => Unit): String = {
        val baos = new ByteArrayOutputStream()
        scala.Console.withOut(new PrintStream(baos))(body)
        baos.toString("UTF-8")
    }

    private def winner(h: Long, pool: PoolTag, minDiff: Boolean): WinnerBlock =
        WinnerBlock(
          height = h,
          hashLe = f"${h}%064x",
          time = 1780474880L,
          pool = pool,
          minDifficulty = minDiff
        )

    test("renders depth, winning pool, and min-difficulty for a small reorg") {
        val report = ReorgReport(
          depth = 2,
          tipHeight = 137608,
          ancestorHeight = 137606,
          ancestorHashLe = "aa" * 32,
          orphanedTipHashLe = "bb" * 32,
          orphanedTipTime = Some(1780473679L),
          winners = Seq(
            winner(137607, PoolTag(None, None), minDiff = true),
            winner(137608, PoolTag(Some("MARA Pool"), None), minDiff = false)
          ),
          winnersCapped = false,
          canonicalWorkGain = Some(BigInt("128000000000")),
          workAdvantageOverOrphan = Some(BigInt("4295032833")),
          activeTip = Some(ChainTip(137713, "ff" * 32, 0, "active")),
          competingForkTips = Seq(ChainTip(137608, "cc" * 32, 1, "valid-fork")),
          nowEpoch = 1780475000L
        )
        val out = capture(CommandHelpers.renderReorgReport(report))

        assert(out.contains("Reorg depth: 2 block(s)"), out)
        assert(out.contains("common ancestor 137606"), out)
        assert(out.contains("MARA Pool"), out)
        assert(out.contains("untagged (likely solo)"), out)
        assert(out.contains("min-difficulty block"), out)
        // both winners listed (≤ 6)
        assert(out.contains("#137607") && out.contains("#137608"), out)
        // chainwork rendered in scientific notation
        assert(out.contains("1.28e11"), out)
        assert(out.contains("bitcoind tip"), out)
        assert(out.contains("bitcoind fork"), out)
        // greppable one-liner
        assert(out.contains("REORG depth=2 ancestor=137606 tip=137608"), out)
        assert(out.contains("min_diff=true"), out)
    }

    test("capped reorg shows first+tip only and honest fetched-count wording") {
        val winners = (1 to 25).map { i =>
            val h = 137606L + i
            winner(
              h,
              if i == 1 then PoolTag(Some("F2Pool"), None) else PoolTag(None, None),
              minDiff = false
            )
        }
        val report = ReorgReport(
          depth = 30,
          tipHeight = 137636,
          ancestorHeight = 137606,
          ancestorHashLe = "aa" * 32,
          orphanedTipHashLe = "bb" * 32,
          orphanedTipTime = None,
          winners = winners,
          winnersCapped = true,
          canonicalWorkGain = None,
          workAdvantageOverOrphan = None,
          activeTip = None,
          competingForkTips = Seq.empty,
          nowEpoch = 1780475000L
        )
        val out = capture(CommandHelpers.renderReorgReport(report))

        assert(out.contains("Reorg depth: 30 block(s)"), out)
        // condensed first + tip, not all 25
        assert(out.contains("canonical first"), out)
        assert(out.contains("canonical tip"), out)
        assert(!out.contains("canonical:"), out) // the ≤6 per-line label is absent
        // honest count: 25 fetched of 30
        assert(out.contains("25 of 30 blocks fetched"), out)
        assert(out.contains("F2Pool"), out)
    }
}
