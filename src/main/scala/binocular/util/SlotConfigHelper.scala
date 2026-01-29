package binocular.util

import scalus.cardano.ledger.{CardanoInfo, SlotConfig}

import java.time.Instant

object SlotConfigHelper {

    /** Compute the validity interval time from CardanoInfo's slot config.
      *
      * Returns both the Instant (for TxBuilder.validFrom) and the time in seconds (for use in the
      * redeemer).
      *
      * @param cardanoInfo
      *   CardanoInfo from BlockchainProvider
      * @return
      *   (validityInstant, timeInSeconds)
      */
    def computeValidityIntervalTime(cardanoInfo: CardanoInfo): (Instant, BigInt) = {
        val now = Instant.now()
        val currentPosixTimeMs = now.toEpochMilli
        val slotConfig = cardanoInfo.slotConfig
        val currentSlot = slotConfig.timeToSlot(currentPosixTimeMs)
        // Compute the slot start time: zeroTime + (slot - zeroSlot) * slotLength
        val slotStartMs =
            slotConfig.zeroTime + (currentSlot - slotConfig.zeroSlot) * slotConfig.slotLength
        val timeInSeconds = BigInt(slotStartMs / 1000)
        (now, timeInSeconds)
    }
}
