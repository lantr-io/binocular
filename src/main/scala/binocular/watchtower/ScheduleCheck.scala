package binocular.watchtower

import binocular.oracle.CardanoNetwork

/** The governing authority's constraint check on a proposed `schedule` (Config `params[0]`).
  *
  * The spec is explicit about where this belongs: "`config.ak` accepts any datum shape by design"
  * and "the constraints marked MUST are the governing authority's to enforce" (spec §TM batches and
  * the protocol schedule). `update-config --schedule` IS that authority's tool, so the constraints
  * live here — nothing on chain will catch a schedule that cannot work.
  *
  * Getting one of these wrong is quiet rather than loud. A `final_tm_cutoff` set below a couple of
  * `tm_batch_interval`s does not fail: it produces a bridge that has two batch opportunities in an
  * epoch and then sits idle for four days, which reads as "the SPOs are down" and is diagnosed by
  * subtracting slot numbers by hand. That is the case this file exists to refuse.
  *
  * ==Pre-existing findings are not this update's fault==
  *
  * Every check runs against the proposed schedule AND against the deployed one. A failure present
  * in both is reported as a warning rather than an error: the operator is editing an unrelated
  * field of a schedule that was already inconsistent, and blocking that edit helps nobody. Only a
  * constraint this update BREAKS is an error.
  */
object ScheduleCheck {

    enum Severity {
        case Error, Warning
    }

    final case class Finding(severity: Severity, rule: String, message: String) {
        def isError: Boolean = severity == Severity.Error
    }

    /** Slots per epoch, by network. The batch grid is anchored at the epoch boundary and
      * `final_tm_cutoff` is an offset into the epoch, so the epoch length is what decides how much
      * of it the grid actually covers.
      */
    def epochLengthSlots(network: CardanoNetwork): Long = network match {
        case CardanoNetwork.Mainnet => 432000L // 5 d
        case CardanoNetwork.Preprod => 432000L // 5 d, mainnet parameters
        case CardanoNetwork.Preview => 86400L //  1 d
        case CardanoNetwork.Testnet => 432000L
    }

    /** `3k/f` for the host chain — the Cardano stability window the spec DERIVES
      * `stability_window` from. Below it, a rollback can change a batch's membership after every
      * SPO has already signed the transaction that batch produced.
      */
    def stabilityWindowFloor(network: CardanoNetwork): Long = network match {
        case CardanoNetwork.Preview => 25920L // k=432,  f=0.05
        case _                      => 129600L // k=2160, f=0.05
    }

    /** Normal Binocular confirmation latency: ~100 Bitcoin blocks plus the oracle's challenge
      * window, which the spec puts at 17-20 h. `tm_recovery_window` must exceed it, or a perfectly
      * healthy in-flight TM is declared stuck and "recovered" out from under itself.
      */
    val BinocularConfirmationLatencySlots: Long = 61200L // 17 h, the low end of the spec's range

    /** Opportunities the grid actually offers in one epoch: `B_i = E + i × interval` for
      * `i = 1, 2, …` while `B_i ≤ E + final_tm_cutoff`.
      */
    def opportunitiesPerEpoch(s: ScheduleParams): Long =
        if s.tmBatchInterval <= 0 then 0L
        else (s.finalTmCutoff / s.tmBatchInterval).toLong

    /** Slots between the epoch's last opportunity and the next epoch's first — the window in which
      * no batch can be built no matter how much work is waiting.
      */
    def deadSlotsPerEpoch(s: ScheduleParams, epochLength: Long): Long = {
        val n = opportunitiesPerEpoch(s)
        if n <= 0 then epochLength
        else {
            val lastB = n * s.tmBatchInterval.toLong
            val nextB = epochLength + s.tmBatchInterval.toLong
            nextB - lastB
        }
    }

    /** Every constraint, as a predicate that is TRUE when the schedule is broken. Shared by the
      * proposed and deployed evaluations so the two cannot drift.
      */
    private def violations(s: ScheduleParams, epochLength: Long, network: CardanoNetwork)
        : List[(Severity, String, String)] = {
        val b = List.newBuilder[(Severity, String, String)]

        if s.tmBatchInterval <= 0 then
            b += ((Severity.Error, "interval>0", "tm_batch_interval is 0 — no batch grid exists"))
        else if s.finalTmCutoff < s.tmBatchInterval then
            b += (
              (
                Severity.Error,
                "cutoff>=interval",
                s"final_tm_cutoff ${s.finalTmCutoff} is below tm_batch_interval " +
                    s"${s.tmBatchInterval}, so B_1 already exceeds it: the epoch has NO batch " +
                    "opportunity at all"
              )
            )

        if !(s.dkgR1Deadline > 0 && s.dkgR1Deadline < s.dkgR2Deadline &&
                s.dkgR2Deadline < s.updateYDeadline)
        then
            b += (
              (
                Severity.Error,
                "dkg-ordering",
                s"0 < dkg_r1_deadline < dkg_r2_deadline < update_y_deadline is violated " +
                    s"(${s.dkgR1Deadline}, ${s.dkgR2Deadline}, ${s.updateYDeadline})"
              )
            )

        // The spec's constraint is "> sign windows + posting margin". The margin is a judgement
        // call, but the sum is not: at or below it, the next opportunity opens before the current
        // batch can finish its two FROST rounds, so a batch can never complete.
        val rounds = s.signR1Window + s.signR2Window
        if s.tmBatchInterval <= rounds then
            b += (
              (
                Severity.Error,
                "interval>rounds",
                s"tm_batch_interval ${s.tmBatchInterval} does not exceed sign_r1_window + " +
                    s"sign_r2_window ($rounds) — the next opportunity opens before the current " +
                    "batch can finish signing, leaving no posting margin"
              )
            )

        // Spec: final_tm_cutoff <= epoch_length - (sign windows + posting + tm_recovery_window +
        // handoff margin). Checked without the unquantifiable margins, so this is the loosest form
        // of the constraint: failing THIS means the epoch's final TM provably cannot be recovered
        // before the boundary rotates the key that would have to sign the recovery.
        val needed = rounds + s.tmRecoveryWindow
        if s.finalTmCutoff + needed > epochLength then
            b += (
              (
                Severity.Error,
                "cutoff+recovery<=epoch",
                s"final_tm_cutoff ${s.finalTmCutoff} + sign windows + tm_recovery_window " +
                    s"($needed) = ${s.finalTmCutoff + needed} exceeds the $epochLength-slot " +
                    "epoch: a TM stuck at the last opportunity cannot be recovered before the " +
                    "boundary"
              )
            )

        if s.tmRecoveryWindow < BinocularConfirmationLatencySlots then
            b += (
              (
                Severity.Warning,
                "recovery>latency",
                s"tm_recovery_window ${s.tmRecoveryWindow} is below the normal Binocular " +
                    s"confirmation latency (~$BinocularConfirmationLatencySlots slots = 100 BTC " +
                    "blocks + challenge): healthy in-flight TMs will be declared stuck"
              )
            )

        val floor = stabilityWindowFloor(network)
        if s.stabilityWindow < floor then
            b += (
              (
                Severity.Warning,
                "stability>=3k/f",
                s"stability_window ${s.stabilityWindow} is below the host chain's 3k/f ($floor): " +
                    "a Cardano rollback can change a batch's membership after every SPO has " +
                    "signed the transaction built from it"
              )
            )

        val opps = opportunitiesPerEpoch(s)
        if opps > 0 && opps < 4 then
            b += (
              (
                Severity.Warning,
                "opportunities",
                s"only $opps batch opportunit${if opps == 1 then "y" else "ies"} per epoch, " +
                    s"then ${deadSlotsPerEpoch(s, epochLength)} slots with none"
              )
            )

        b.result()
    }

    /** Check a proposed schedule, discounting anything the deployed one already violates.
      *
      * @return
      *   findings, errors first. An empty list means the proposal is consistent.
      */
    def check(
        deployed: ScheduleParams,
        proposed: ScheduleParams,
        network: CardanoNetwork
    ): List[Finding] = {
        val epochLength = epochLengthSlots(network)
        val before = violations(deployed, epochLength, network).map(_._2).toSet
        val onProposed = violations(proposed, epochLength, network)
            .map { case (sev, rule, msg) =>
                if before.contains(rule) then
                    // Already broken before this update. Say so and step out of the way.
                    Finding(Severity.Warning, rule, s"$msg [pre-existing, not caused by this update]")
                else Finding(sev, rule, msg)
            }
        (onProposed ++ deltas(deployed, proposed)).sortBy(f => if f.isError then 0 else 1)
    }

    /** Constraints on the CHANGE rather than on the result. These can never be "pre-existing" —
      * the movement itself is what they judge — so they are never downgraded.
      */
    private def deltas(deployed: ScheduleParams, proposed: ScheduleParams): List[Finding] =
        // Spec §TM batches: stability_window is DERIVED from the host chain's 3k/f, and "the
        // governing authority MUST reject smaller values — it is fund-safety-critical, tunable
        // only upward". Lowering it is the one schedule edit that can cost funds rather than
        // latency: below the chain's own stability window, a rollback can change which requests
        // were in a batch AFTER every SPO has signed the Bitcoin transaction built from it.
        if proposed.stabilityWindow < deployed.stabilityWindow then
            List(
              Finding(
                Severity.Error,
                "stability-only-upward",
                s"stability_window is being LOWERED, ${deployed.stabilityWindow} -> " +
                    s"${proposed.stabilityWindow}. The spec makes this parameter tunable only " +
                    "upward: it is derived from the host chain's 3k/f and is fund-safety-critical"
              )
            )
        else Nil

    /** A human-readable description of the grid a schedule produces, for the change report. */
    def gridSummary(s: ScheduleParams, network: CardanoNetwork): String = {
        val epochLength = epochLengthSlots(network)
        val opps = opportunitiesPerEpoch(s)
        def h(slots: BigInt): String = f"${slots.toDouble / 3600}%.1f h"
        s"$opps opportunities/epoch, every ${h(s.tmBatchInterval)}; a request is eligible " +
            s"${h(s.stabilityWindow)} after it is created; last opportunity at E+" +
            s"${h(s.finalTmCutoff)}, then ${h(deadSlotsPerEpoch(s, epochLength))} with none"
    }
}
