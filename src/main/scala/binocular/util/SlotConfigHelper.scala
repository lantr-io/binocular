package binocular.util

import binocular.BitcoinValidator
import scalus.cardano.ledger.CardanoInfo

import java.time.Instant

object SlotConfigHelper {

    /** Compute the validity interval time from CardanoInfo's slot config.
      *
      * The on-chain validator reads `tx.validRange.to` as its notion of "current time" (see the
      * comment in `BitcoinValidator.spend`), so the seconds value returned here corresponds to the
      * **end** of the validity interval, not the start.
      *
      *   - `validityInstant` (the wall-clock Instant) is still the **start** of the interval and is
      *     meant to be passed straight to `TxBuilder.validFrom(...)`. The caller derives
      *     `validTo = validityInstant + MaxValidityWindow`.
      *   - `onChainEndTimeInSeconds` is `validTo / 1000`, i.e. what the validator will see as
      *     `intervalEndInSeconds`. Off-chain state computation must pass this value as
      *     `currentTime` to `BitcoinValidator.computeUpdate` so that the off-chain prediction
      *     matches the on-chain reference.
      *
      * @param targetEndTimeSeconds
      *   Optional on-chain end-of-interval POSIXTime (seconds). When provided, the function
      *   reconstructs the corresponding `validFrom` slot (target − MaxValidityWindow) and
      *   re-derives the canonical end time. This is the round-trip case: a caller previously
      *   obtained an end time and now needs the matching wall-clock instant. When `None`, the
      *   function uses `now − 5 min` as the start anchor to guard against clock skew between the
      *   local machine and the Cardano node (the validity window itself is `MaxValidityWindow = 10
      *   min`).
      * @return
      *   `(validityInstant, onChainEndTimeInSeconds)` where `validityInstant` is the wall-clock
      *   start of the validity window and `onChainEndTimeInSeconds` is the on-chain reference time
      *   the validator will read.
      */
    def computeValidityIntervalTime(
        cardanoInfo: CardanoInfo,
        targetEndTimeSeconds: Option[BigInt] = None
    ): (Instant, BigInt) = {
        val slotConfig = cardanoInfo.slotConfig
        val maxWindowMs = BitcoinValidator.MaxValidityWindow.toLong
        val startAnchorMs = targetEndTimeSeconds match {
            case Some(endSeconds) =>
                // Round-trip: reconstruct the validFrom from a previously-returned end time.
                endSeconds.toLong * 1000 - maxWindowMs
            case None =>
                // Subtract 5 minutes to guard against clock skew between local
                // machine and the Cardano node. The validity window is 10 minutes.
                Instant.now().toEpochMilli - 300_000L
        }
        val startSlot = slotConfig.timeToSlot(startAnchorMs)
        val validityInstant = slotConfig.slotToInstant(startSlot)
        val endTimeMs = validityInstant.toEpochMilli + maxWindowMs
        val onChainEndTimeInSeconds = BigInt(endTimeMs / 1000)
        (validityInstant, onChainEndTimeInSeconds)
    }
}
