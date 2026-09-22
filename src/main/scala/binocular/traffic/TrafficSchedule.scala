package binocular.traffic

import binocular.watchtower.ScheduleParams

/** Chain slots only. Epoch-relative deadlines scale from the protocol's 432000-slot epoch; batch
  * pitch and stability are durations and must not scale. BigInt prevents boundary overflow.
  */
final class TrafficSchedule private (
    val tipSlot: Long,
    val cycle: Long,
    val cycleStart: BigInt,
    val updateYDeadline: BigInt,
    val finalCutoff: BigInt,
    batchInterval: BigInt,
    stabilityWindow: BigInt
) {

    /** The slot of the first batch a request created at the current chain tip can join. */
    def nextBatch: Option[BigInt] = {
        val offset = BigInt(tipSlot) - cycleStart
        val first = ((offset + stabilityWindow + batchInterval - 1) / batchInterval)
            .max(offset / batchInterval + 1)
        val slot = cycleStart + first * batchInterval
        Option.when(slot <= finalCutoff && slot > tipSlot)(slot)
    }
}

object TrafficSchedule {
    def at(
        tipSlot: Long,
        virtualEpochSlots: Long,
        schedule: ScheduleParams
    ): Either[String, TrafficSchedule] = {
        if tipSlot < 0 || virtualEpochSlots <= 0 || schedule.tmBatchInterval <= 0 ||
            schedule.stabilityWindow < 0 || schedule.updateYDeadline < 0 ||
            schedule.finalTmCutoff <= schedule.updateYDeadline || schedule.finalTmCutoff > 432000
        then Left("Invalid traffic schedule or chain slot")
        else {
            val cycle = tipSlot / virtualEpochSlots
            val start = BigInt(cycle) * virtualEpochSlots
            def scaled(value: BigInt) = start + value * virtualEpochSlots / 432000
            Right(
              new TrafficSchedule(
                tipSlot,
                cycle,
                start,
                scaled(schedule.updateYDeadline),
                scaled(schedule.finalTmCutoff),
                schedule.tmBatchInterval,
                schedule.stabilityWindow
              )
            )
        }
    }
}
