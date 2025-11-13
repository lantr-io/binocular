package binocular.util

import scalus.cardano.ledger.SlotConfig
import com.bloxbean.cardano.client.backend.api.BackendService

object SlotConfigHelper {

    //for shelley era
    val SLOTS_PER_EPOCH = 432_000
    val SLOT_LENGTH_MS = 1000L  // 1 second in milliseconds
    val zeroSlot = 0L


    def retrieveSlotConfig(backendService: BackendService): SlotConfig = {
        // Get current block to compute slot configuration
        val latestBlock = backendService.getBlockService.getLatestBlock.getValue
        val currentSlot = latestBlock.getSlot
        val currentTimeMs = System.currentTimeMillis()
        
        // Calculate zeroTime from current slot and time
        // zeroTime = currentTime - (currentSlot * slotLength)
        val zeroTime = currentTimeMs - (currentSlot * SLOT_LENGTH_MS)
        

        val slotConfig = SlotConfig(zeroTime = zeroTime, zeroSlot = zeroSlot, slotLength = SLOT_LENGTH_MS)
        slotConfig
    }

}
