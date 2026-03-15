package binocular.util

import scalus.cardano.ledger.CardanoInfo

import java.time.Instant

object SlotConfigHelper {

    /** Compute the validity interval time from CardanoInfo's slot config.
      *
      * Returns both a wall-clock Instant (for TxBuilder.validFrom) and the on-chain POSIXTime in
      * seconds (for use in the redeemer / off-chain state computation).
      *
      * The SlotConfig's `posixTimeOffset` (set by `fetchYaciSlotConfig` for Yaci DevKit) handles
      * the difference between wall-clock time and the Cardano node's on-chain POSIXTime
      * automatically.
      *
      * @param cardanoInfo
      *   CardanoInfo from BlockchainProvider
      * @param targetTimeSeconds
      *   Optional on-chain POSIXTime target (seconds). When provided, the function finds the slot
      *   corresponding to this on-chain time and returns its wall-clock Instant.
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
                // Convert on-chain POSIXTime to wall-clock ms for slot lookup
                slotConfig.timeToSlot(seconds.toLong * 1000 - slotConfig.posixTimeOffset)
            case None =>
                slotConfig.timeToSlot(Instant.now().toEpochMilli)
        }
        // slotToTime returns on-chain POSIXTime (includes posixTimeOffset)
        val onChainPosixTimeMs = slotConfig.slotToTime(currentSlot)
        val onChainTimeInSeconds = BigInt(onChainPosixTimeMs / 1000)
        // slotToInstant returns wall-clock Instant (for TxBuilder.validFrom)
        val wallClockInstant = slotConfig.slotToInstant(currentSlot)
        (wallClockInstant, onChainTimeInSeconds)
    }
}
