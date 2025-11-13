package binocular.util

import scalus.cardano.ledger.SlotConfig
import com.bloxbean.cardano.client.backend.api.BackendService

object SlotConfigHelper {

    //for shelley era
    val SLOTS_PER_EPOCH = 432_000
    val SECONDS_PER_SLOT = 1
    val zeroSlot = 0L


    def retrieveSlotConfig(backendService: BackendService): SlotConfig = {
        val ec = backendService.getEpochService.getLatestEpoch.getValue
        val zeroTime = ec.getStartTime() - (ec.getEpoch() * SLOTS_PER_EPOCH * SECONDS_PER_SLOT);
        val slotConfig = SlotConfig(zeroTime = zeroTime, zeroSlot = zeroSlot, slotLength = SECONDS_PER_SLOT)
        slotConfig
    }

}
