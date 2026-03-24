package binocular.util

import scalus.cardano.ledger.CardanoInfo

import java.time.Instant

object SlotConfigHelper {

    /** Compute the validity interval time from CardanoInfo's slot config.
      *
      * Returns both a wall-clock Instant (for TxBuilder.validFrom) and the on-chain POSIXTime in
      * seconds (for use in the redeemer / off-chain state computation).
      *
      * @param targetTimeSeconds
      *   Optional on-chain POSIXTime target (seconds). When provided, the function finds the slot
      *   corresponding to this on-chain time and returns its wall-clock Instant. When None, uses
      *   current time minus 5 minutes to guard against clock skew.
      * @return
      *   (validityInstant, onChainTimeInSeconds)
      */
    def computeValidityIntervalTime(
        cardanoInfo: CardanoInfo,
        targetTimeSeconds: Option[BigInt] = None
    ): (Instant, BigInt) = {
        val slotConfig = cardanoInfo.slotConfig
        val currentSlot = targetTimeSeconds match {
            case Some(seconds) =>
                // POSIXTime (seconds) → slot
                slotConfig.timeToSlot(seconds.toLong * 1000)
            case None =>
                // Subtract 5 minutes to guard against clock skew between local
                // machine and the Cardano node. The validity window is 10 minutes.
                slotConfig.timeToSlot(Instant.now().toEpochMilli - 300_000L)
        }
        val onChainPosixTimeMs = slotConfig.slotToTime(currentSlot)
        val onChainTimeInSeconds = BigInt(onChainPosixTimeMs / 1000)
        val wallClockInstant = slotConfig.slotToInstant(currentSlot)
        (wallClockInstant, onChainTimeInSeconds)
    }
}
