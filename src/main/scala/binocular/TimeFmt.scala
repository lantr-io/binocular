package binocular

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/** Shared off-chain (JVM-only) time formatting helpers.
  *
  * Kept out of `@Compile` objects (e.g. `BitcoinHelpers`) because `java.time` cannot be compiled to
  * Plutus. Used by both [[binocular.oracle.ForkTreePretty]] and the reorg diagnostics.
  */
object TimeFmt {
    private val dateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    /** Format an epoch-seconds timestamp as `yyyy-MM-dd HH:mm` in UTC. */
    def utc(epochSeconds: BigInt): String =
        Instant.ofEpochSecond(epochSeconds.toLong).atZone(ZoneId.of("UTC")).format(dateFmt)

    /** Compact human-readable duration, e.g. `45s`, `3m`, `2h 5m`, `1d 4h`. Negative inputs (a
      * block timestamp ahead of wall-clock, which Bitcoin permits up to 2h) render as `0s`.
      */
    def humanDuration(seconds: Long): String = {
        val s = math.max(0L, seconds)
        if s < 60 then s"${s}s"
        else if s < 3600 then s"${s / 60}m"
        else if s < 86400 then {
            val h = s / 3600; val m = (s % 3600) / 60
            if m == 0 then s"${h}h" else s"${h}h ${m}m"
        } else {
            val d = s / 86400; val h = (s % 86400) / 3600
            if h == 0 then s"${d}d" else s"${d}d ${h}h"
        }
    }
}
