package binocular.traffic

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto.DoubleSha256Digest
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{TransactionHash, TransactionInput}
import scodec.bits.ByteVector

class TrafficPlanTest extends AnyFunSuite {

    private val MaxAmount = 50000L
    private val MeanInterval = 14400L
    private val Timeout = 720

    /** Exactly the double `decide` compares against, so the strict `<` boundary is bit-identical.
      */
    private val Threshold = TrafficPlan.TickSeconds.toDouble / MeanInterval

    private def outpoint(n: Int) =
        TransactionOutPoint(DoubleSha256Digest(ByteVector.fill(32)(n.toByte)), UInt32(0))

    private def request(n: Int) = TransactionInput(TransactionHash.fromHex("aa" * 32), n)

    private def dep(
        n: Int,
        confirmations: Int = 1,
        unspent: Boolean = true,
        swept: Boolean = false,
        requested: Option[TransactionInput] = None,
        inOracle: Boolean = false
    ) = DepositView(outpoint(n), 30000L, confirmations, unspent, swept, requested, inOracle)

    private def obs(
        deposits: Seq[DepositView] = Nil,
        fundingSat: Long = 1000000L,
        heldFbtc: Long = 0L,
        minPegOutSat: Long = 10000L,
        timeoutBlocks: Int = Timeout,
        inWindow: Boolean = true,
        openPegOuts: Int = 0
    ) = Observation(
      deposits,
      fundingSat,
      heldFbtc,
      minPegOutSat,
      timeoutBlocks,
      inWindow,
      openPegOuts
    )

    private def decide(o: Observation, draws: Draws) =
        TrafficPlan.decide(o, MaxAmount, MeanInterval, draws)

    private val noDraw = Draws(1.0, 1.0, 0.0)
    private val depositDraw = Draws(0.0, 1.0, 0.0)
    private val pegOutDraw = Draws(1.0, 0.0, 0.0)

    private val done = dep(1, swept = true)
    private val completable = dep(2, swept = true, requested = Some(request(2)))
    private val refundable = dep(3, confirmations = Timeout)
    private val requestable = dep(4, inOracle = true)
    private val waiting = dep(5)

    // [TRF-122]: every row of the [TRF-109] table.

    test("row 1: swept and not requested is done") {
        assert(TrafficPlan.status(done, Timeout) == Status.Done)
        assert(decide(obs(Seq(done)), noDraw).isEmpty)
    }

    test("row 2: swept and requested is completable") {
        assert(TrafficPlan.status(completable, Timeout) == Status.Completable)
        assert(
          decide(obs(Seq(completable)), noDraw).contains(
            Action.Complete(completable, request(2))
          )
        )
    }

    test("row 1 precedes row 2: swept wins over maturity") {
        val d = dep(1, confirmations = Timeout, swept = true)
        assert(TrafficPlan.status(d, Timeout) == Status.Done)
    }

    test("row 3: not swept, mature and unspent is refundable") {
        assert(TrafficPlan.status(refundable, Timeout) == Status.Refundable)
        assert(decide(obs(Seq(refundable)), noDraw).contains(Action.Refund(refundable)))
    }

    test("row 3 boundary: 719 confirmations is not refundable, 720 is") {
        assert(TrafficPlan.status(dep(3, confirmations = 719), Timeout) == Status.Waiting)
        assert(TrafficPlan.status(dep(3, confirmations = 720), Timeout) == Status.Refundable)
    }

    test("row 3: mature but spent is not refundable") {
        val d = dep(3, confirmations = Timeout, unspent = false)
        assert(TrafficPlan.status(d, Timeout) == Status.Done)
        assert(decide(obs(Seq(d)), noDraw).isEmpty)
    }

    test("row 3 precedes row 4: a mature deposit in the oracle is refunded, not requested") {
        val d = dep(3, confirmations = Timeout, inOracle = true)
        assert(TrafficPlan.status(d, Timeout) == Status.Refundable)
    }

    test("row 4: not requested and in the oracle is requestable") {
        assert(TrafficPlan.status(requestable, Timeout) == Status.Requestable)
        assert(decide(obs(Seq(requestable)), noDraw).contains(Action.Request(requestable)))
    }

    test("row 1: spent and not swept is refunded, so done, even when in the oracle") {
        val d = dep(4, confirmations = Timeout, unspent = false, inOracle = true)
        assert(TrafficPlan.status(d, Timeout) == Status.Done)
        assert(decide(obs(Seq(d)), noDraw).isEmpty)
    }

    test("row 5: requested, not swept and immature is waiting") {
        val d = dep(5, requested = Some(request(5)), inOracle = true)
        assert(TrafficPlan.status(d, Timeout) == Status.Waiting)
        assert(decide(obs(Seq(d)), noDraw).isEmpty)
    }

    test("row 5: not in the oracle and immature is waiting") {
        assert(TrafficPlan.status(waiting, Timeout) == Status.Waiting)
        assert(decide(obs(Seq(waiting)), noDraw).isEmpty)
    }

    // [TRF-110]: priority.

    test("priority peels off refund, complete, request, deposit, peg-out") {
        val all = Seq(waiting, completable, refundable, requestable)
        assert(decide(obs(all), depositDraw).contains(Action.Refund(refundable)))

        val noRefund = all.filterNot(_ == refundable)
        assert(
          decide(obs(noRefund), depositDraw).contains(
            Action.Complete(completable, request(2))
          )
        )

        val noComplete = noRefund.filterNot(_ == completable)
        assert(decide(obs(noComplete), depositDraw).contains(Action.Request(requestable)))

        val noRequest = noComplete.filterNot(_ == requestable)
        assert(decide(obs(noRequest), depositDraw).contains(Action.Deposit(10000L)))
        assert(
          decide(obs(noRequest, heldFbtc = 20000L), pegOutDraw).contains(
            Action.PegOut(10000L)
          )
        )
        assert(decide(obs(noRequest), noDraw).isEmpty)
    }

    test("first means first in the observation order within a status") {
        val a = dep(6, confirmations = Timeout)
        val b = dep(7, confirmations = Timeout)
        assert(decide(obs(Seq(a, b)), noDraw).contains(Action.Refund(a)))
        assert(decide(obs(Seq(b, a)), noDraw).contains(Action.Refund(b)))
    }

    // [TRF-111]: the probability gate.

    test("the gate is strict: exactly 300/mean does not fire, just below does") {
        val o = obs(heldFbtc = 20000L)
        assert(decide(o, Draws(Threshold, Threshold, 0.0)).isEmpty)
        assert(
          decide(o, Draws(Math.nextDown(Threshold), 1.0, 0.0)).exists(
            _.isInstanceOf[
              Action.Deposit
            ]
          )
        )
        assert(
          decide(o, Draws(1.0, Math.nextDown(Threshold), 0.0)).exists(
            _.isInstanceOf[
              Action.PegOut
            ]
          )
        )
    }

    // [TRF-112], [TRF-113]: deposit guards.

    test("no deposit outside the window") {
        assert(decide(obs(inWindow = false), depositDraw).isEmpty)
    }

    test("a deposit draw that fails the funding guard falls through to the peg-out draw") {
        // funding - amount - FeeCapSat = 1_000_000 - 10_000 - 10_000 < 50_000 needs funding < 70_000
        val low = obs(fundingSat = 69999L, heldFbtc = 20000L)
        assert(decide(low, Draws(0.0, 1.0, 0.0)).isEmpty)
        assert(decide(low, Draws(0.0, 0.0, 0.0)).contains(Action.PegOut(10000L)))
        assert(
          decide(obs(fundingSat = 70000L), Draws(0.0, 1.0, 0.0)).contains(
            Action.Deposit(10000L)
          )
        )
    }

    test("no deposit when the amount range is empty") {
        assert(decide(obs(minPegOutSat = MaxAmount + 1), depositDraw).isEmpty)
    }

    // [TRF-114], [TRF-115]: amounts.

    test("deposit amount spans the whole range and never exceeds the maximum") {
        val narrow = (o: Observation) => TrafficPlan.decide(o, 10499L, MeanInterval, _: Draws)
        val o = obs(minPegOutSat = 10000L)
        assert(narrow(o)(Draws(0.0, 1.0, 0.0)).contains(Action.Deposit(10000L)))
        assert(narrow(o)(Draws(0.0, 1.0, 0.999)).contains(Action.Deposit(10499L)))
        for i <- 0 to 100 do
            val Some(Action.Deposit(a)) = narrow(o)(Draws(0.0, 1.0, i / 100.0)): @unchecked
            assert(a >= 10000L && a <= 10499L, s"draw ${i / 100.0} gave $a")
    }

    test("peg-out is refused below the minimum and never exceeds held fBTC") {
        assert(decide(obs(heldFbtc = 9999L), pegOutDraw).isEmpty)
        assert(decide(obs(heldFbtc = 10000L), pegOutDraw).contains(Action.PegOut(10000L)))
        for i <- 0 to 100 do
            val o = obs(heldFbtc = 12000L)
            val Some(Action.PegOut(a)) = decide(o, Draws(1.0, 0.0, i / 100.0)): @unchecked
            assert(a >= 10000L && a <= 12000L, s"draw ${i / 100.0} gave $a")
    }

    test("peg-out is capped by max-amount-sat as well as by held fBTC") {
        val o = obs(heldFbtc = 1000000L)
        for i <- 0 to 100 do
            val Some(Action.PegOut(a)) = decide(o, Draws(1.0, 0.0, i / 100.0)): @unchecked
            assert(a >= 10000L && a <= MaxAmount, s"draw ${i / 100.0} gave $a")
    }

    // [TRF-124]: no action repeats.

    test("a refunded deposit is not refunded again") {
        val before = obs(Seq(refundable))
        assert(decide(before, noDraw).contains(Action.Refund(refundable)))
        val spent = refundable.copy(unspent = false, inOracle = true)
        assert(decide(obs(Seq(spent)), noDraw).isEmpty)
    }

    test("a completed deposit is not completed again") {
        assert(decide(obs(Seq(completable)), noDraw).nonEmpty)
        val after = completable.copy(requested = None)
        assert(decide(obs(Seq(after)), noDraw).isEmpty)
    }

    test("a requested deposit is not requested again") {
        assert(decide(obs(Seq(requestable)), noDraw).contains(Action.Request(requestable)))
        val after = requestable.copy(requested = Some(request(4)))
        assert(decide(obs(Seq(after)), noDraw).isEmpty)
    }

    test("a deposit and a peg-out do not repeat once their draws no longer fire") {
        val o = obs(heldFbtc = 20000L)
        assert(decide(o, depositDraw).contains(Action.Deposit(10000L)))
        assert(decide(o, pegOutDraw).contains(Action.PegOut(10000L)))
        assert(decide(o, noDraw).isEmpty)
    }

    // [TRF-48]: the summary line.

    test("summary carries every status count and both balances") {
        val o = obs(
          Seq(done, done, completable, requestable, waiting, waiting, waiting),
          fundingSat = 1331581L,
          heldFbtc = 29000L,
          openPegOuts = 1
        )
        assert(
          TrafficPlan.summary(o) ==
              "deposits: 2 done, 1 completable, 0 refundable, 1 requestable, 3 waiting" +
              " | fBTC held: 29000 | open peg-outs: 1 | funding: 1331581 sat"
        )
    }
}
