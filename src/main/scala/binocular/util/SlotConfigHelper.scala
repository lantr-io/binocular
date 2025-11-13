package binocular.util

import scalus.cardano.ledger.SlotConfig
import com.bloxbean.cardano.client.backend.api.BackendService

object SlotConfigHelper {

    //for shelley era
    val SLOTS_PER_EPOCH = 432_000
    val SLOT_LENGTH_MS = 1000L  // 1 second in milliseconds
    val zeroSlot = 0L


    def retrieveSlotConfig(backendService: BackendService): SlotConfig = {
        val ec = backendService.getEpochService.getLatestEpoch.getValue
        // ec.getStartTime() is in seconds, convert to milliseconds
        val epochStartTimeMs = ec.getStartTime() * 1000L
        // Calculate zero time by going back from epoch start
        val zeroTime = epochStartTimeMs - (ec.getEpoch() * SLOTS_PER_EPOCH * SLOT_LENGTH_MS)
        val slotConfig = SlotConfig(zeroTime = zeroTime, zeroSlot = zeroSlot, slotLength = SLOT_LENGTH_MS)
        slotConfig
    }

}
