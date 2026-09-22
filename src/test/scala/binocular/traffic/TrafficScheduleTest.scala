package binocular.traffic

import binocular.watchtower.ScheduleParams
import org.scalatest.funsuite.AnyFunSuite

class TrafficScheduleTest extends AnyFunSuite {
    private val schedule =
        ScheduleParams(3600, 7200, 10800, 10800, 1800, 1800, 60, 129600, 345600, 7200)
    private def at(slot: Long, epoch: Long = 86400) =
        TrafficSchedule.at(slot, epoch, schedule).toOption.get

    test("scales epoch deadlines but not the batch pitch or stability window") {
        val now = at(2160)
        assert(now.cycle == 0)
        assert(now.cycleStart == 0)
        assert(now.updateYDeadline == 2160)
        assert(now.finalCutoff == 69120)
        for i <- 1 to 6 do assert(at(i * 10800L - 7200).nextBatch.get == i * 10800L)
        assert(at(57601).nextBatch.isEmpty)
        val noBatch =
            TrafficSchedule.at(100, 86400, schedule.copy(tmBatchInterval = 100000)).toOption.get
        assert(noBatch.nextBatch.isEmpty)
    }

    test("membership cutoff is inclusive; one slot late uses the next batch") {
        assert(at(3600).nextBatch.get == 10800)
        assert(at(3601).nextBatch.get == 21600)
        assert(at(86400 + 3600).nextBatch.get == 86400 + 10800)
    }

    test("with zero stability a batch already at the tip is skipped, not the whole cycle") {
        val now = TrafficSchedule.at(10800, 86400, schedule.copy(stabilityWindow = 0)).toOption.get
        assert(now.nextBatch.get == 21600)
    }

    test("invalid schedule values and slot inputs fail closed") {
        assert(TrafficSchedule.at(-1, 86400, schedule).isLeft)
        assert(TrafficSchedule.at(1, 0, schedule).isLeft)
        assert(TrafficSchedule.at(1, 86400, schedule.copy(tmBatchInterval = 0)).isLeft)
        assert(TrafficSchedule.at(1, 86400, schedule.copy(stabilityWindow = -1)).isLeft)
        assert(TrafficSchedule.at(1, 86400, schedule.copy(finalTmCutoff = 432001)).isLeft)
    }

    test("uses wide arithmetic near Long.MaxValue") {
        val now = at(Long.MaxValue)
        assert(now.cycleStart >= 0)
        assert(now.finalCutoff > now.cycleStart)
        assert(now.cycleStart <= Long.MaxValue)
        assert(now.cycleStart + 86400 > Long.MaxValue)
    }
}
