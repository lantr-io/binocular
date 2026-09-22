package binocular.traffic

import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import scalus.cardano.ledger.TransactionInput

/** One of our deposits as observed this tick. `inOracle` is computed only for unrequested deposits.
  */
final case class DepositView(
    outpoint: TransactionOutPoint, // vout 0 of our deposit
    amountSat: Long,
    confirmations: Int,
    unspent: Boolean, // gettxout non-null, mempool included
    swept: Boolean,
    requested: Option[TransactionInput], // the PegInRequest UTxO, if one names this deposit
    inOracle: Boolean
)

final case class Observation(
    deposits: Seq[DepositView],
    fundingSat: Long, // confirmed balance at the funding address
    heldFbtc: Long, // fBTC in the sponsor wallet
    minPegOutSat: Long, // live Config min_peg_out_fbtc
    timeoutBlocks: Int, // live Config pegin_refund_timeout_blocks
    inWindow: Boolean, // tip inside the deposit window, [TRF-112]
    openPegOuts: Int // our open peg-out requests; reported by [TRF-48], used by no rule
)

/** Uniform draws in [0, 1), one per tick, supplied by the caller. [TRF-119] */
final case class Draws(deposit: Double, pegOut: Double, amount: Double)

enum Status { case Done, Completable, Refundable, Requestable, Waiting }

enum Action {
    case Refund(deposit: DepositView)
    case Complete(deposit: DepositView, request: TransactionInput)
    case Request(deposit: DepositView)
    case Deposit(amountSat: Long)
    case PegOut(amountSat: Long)
}

/** The pure decision of [TRF-119]: the observed ledgers plus three draws in, at most one action
  * out. No I/O, no configuration, no clock.
  */
object TrafficPlan {
    val TickSeconds: Long = 300 // [TRF-100], [TRF-120]

    /** [TRF-120]; also the fee bound used by [TRF-113], because the fee is unknown before the
      * build.
      */
    val FeeCapSat: Long = 10000

    /** [TRF-109]: first matching row. Completed reads as swept and not requested; refunded as spent
      * and not swept (see Definitions).
      */
    def status(deposit: DepositView, timeoutBlocks: Int): Status =
        if deposit.swept then if deposit.requested.isEmpty then Status.Done else Status.Completable
        else if !deposit.unspent then Status.Done
        else if deposit.confirmations >= timeoutBlocks then Status.Refundable
        else if deposit.requested.isEmpty && deposit.inOracle then Status.Requestable
        else Status.Waiting

    /** [TRF-110]..[TRF-115]. `maxAmountSat` and `meanIntervalSeconds` come from TrafficConfig;
      * passed as plain values so this file has no config dependency.
      */
    def decide(
        obs: Observation,
        maxAmountSat: Long,
        meanIntervalSeconds: Long,
        draws: Draws
    ): Option[Action] = {
        def first(wanted: Status) = obs.deposits.find(status(_, obs.timeoutBlocks) == wanted)
        // [TRF-111]: strictly below, so a draw of exactly the threshold does not fire.
        def fires(u: Double) = u < TickSeconds.toDouble / meanIntervalSeconds
        def uniform(lo: Long, hi: Long) = math.min(lo + (draws.amount * (hi - lo + 1)).toLong, hi)

        // [TRF-112], [TRF-113], [TRF-114].
        def randomDeposit =
            val amount = uniform(obs.minPegOutSat, maxAmountSat)
            Option.when(
              fires(draws.deposit) && obs.inWindow && obs.minPegOutSat <= maxAmountSat
                  && obs.fundingSat - amount - FeeCapSat >= maxAmountSat
            )(Action.Deposit(amount))

        // [TRF-115].
        def randomPegOut =
            val upper = math.min(obs.heldFbtc, maxAmountSat)
            Option.when(fires(draws.pegOut) && upper >= obs.minPegOutSat)(
              Action.PegOut(uniform(obs.minPegOutSat, upper))
            )

        first(Status.Refundable)
            .map(Action.Refund.apply)
            .orElse(first(Status.Completable).map(d => Action.Complete(d, d.requested.get)))
            .orElse(first(Status.Requestable).map(Action.Request.apply))
            .orElse(randomDeposit)
            .orElse(randomPegOut)
    }

    /** [TRF-48]: one line. */
    def summary(obs: Observation): String = {
        val counts = obs.deposits.groupBy(status(_, obs.timeoutBlocks))
        def n(s: Status) = counts.get(s).fold(0)(_.size)
        s"deposits: ${n(Status.Done)} done, ${n(Status.Completable)} completable," +
            s" ${n(Status.Refundable)} refundable, ${n(Status.Requestable)} requestable," +
            s" ${n(Status.Waiting)} waiting | fBTC held: ${obs.heldFbtc}" +
            s" | open peg-outs: ${obs.openPegOuts} | funding: ${obs.fundingSat} sat"
    }
}
