# test_schedule_pre_frac.R
# Property checks for schedule_pre_frac() (event-mode pre-frac scheduler,
# R/engine_core.R). Tests the scheduler in isolation against
# synthetic well-level workloads -- no need to run the full Monte Carlo loop.
# Run: Rscript test_schedule_pre_frac.R
ENGINE_FILES <- if (file.exists("engine_core.R")) {
  c("engine_core.R", "summaries.R", "report_pdf.R", "optimiser_cascade.R")
} else "archive/simulation_engine.R"
suppressPackageStartupMessages({ for (.ef in ENGINE_FILES) source(.ef) })

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

# -- 1. Single well, single unit of each resource: finish times match a
# straight hand calculation. Frac's fleet timeline is independent of CT and
# wireline (the fleet can start pumping stage 1 as soon as it's free -- only
# the well's FINISH is bounded below by wireline finishing, not its start,
# per the well-level pacing approximation). Here frac_workload (8) on its own
# already exceeds wireline's finish (5), so frac's own duration paces the well.
r1 <- schedule_pre_frac(
  well_order_index = 1,
  ct_workload_days = 2, wireline_workload_days = 3, frac_workload_days = 8,
  ct_units = 1, wireline_units = 1, frac_fleets = 1
)
chk(r1$well_schedule$ct_finish_day == 2, "single well: CT finishes at t=2")
chk(r1$well_schedule$wireline_start_day == 2, "single well: wireline starts at t=2, gated by CT finishing")
chk(r1$well_schedule$wireline_finish_day == 5, "single well: wireline finishes at t=2+3=5")
chk(r1$well_schedule$frac_start_day == 0, "single well: frac starts at t=0, independent of CT/wireline timing")
chk(r1$well_schedule$frac_finish_day == 8, "single well: frac finishes at t=0+8=8 (its own workload paces the well here)")
chk(r1$total_wireline_readiness_delay_days == 0,
    "single well: frac's own workload (8) already exceeds wireline finish (5) -- wireline causes NO extra wait")

# -- 2. Frac workload shorter than wireline's: frac can't finish before its
# own wireline does (well-level pacing approximation), and the wait this
# costs frac must be measured LOCALLY (wireline finish minus what frac's
# finish would have been on its own), not as frac_finish - wireline_finish --
# the latter compares two cumulative, possibly unrelated queue positions and
# is wrong in general (see schedule_pre_frac()'s wireline_wait_days comment).
r2 <- schedule_pre_frac(
  well_order_index = 1,
  ct_workload_days = 0, wireline_workload_days = 8, frac_workload_days = 1,
  ct_units = 1, wireline_units = 1, frac_fleets = 1
)
chk(r2$well_schedule$frac_finish_day == 8, "frac cannot finish before wireline (max(1, 8) = 8)")
chk(r2$total_wireline_readiness_delay_days == 7,
    "wireline (finishes at 8) forces frac to wait 7 days beyond its own 1-day workload")

# -- 3. Two wells, 1 wireline unit: real contention -- well 2's wireline start
# is gated by well 1's wireline finish, not by a per-well formula.
r3 <- schedule_pre_frac(
  well_order_index = 1:2,
  ct_workload_days = c(0, 0), wireline_workload_days = c(4, 4), frac_workload_days = c(0, 0),
  ct_units = 1, wireline_units = 1, frac_fleets = 1
)
chk(r3$well_schedule$wireline_start_day[2] == 4, "well 2's wireline is gated by well 1's wireline finishing (single shared unit)")
chk(r3$well_schedule$wireline_finish_day[2] == 8, "well 2's wireline finishes at 8 (4+4, queued behind well 1)")

# -- 4. Two wells, 2 wireline units, asymmetric workload: a unit that
# finishes well 1 early picks up well 2 immediately -- the actual capability
# this scheduler adds over the formula path (which has no notion of a unit
# "racing ahead" to the next well).
r4 <- schedule_pre_frac(
  well_order_index = 1:2,
  ct_workload_days = c(0, 0), wireline_workload_days = c(2, 10), frac_workload_days = c(0, 0),
  ct_units = 1, wireline_units = 2, frac_fleets = 1
)
chk(r4$well_schedule$wireline_start_day[2] == 0, "well 2 starts immediately on the second unit, not gated by well 1")
chk(r4$well_schedule$wireline_unit[1] != r4$well_schedule$wireline_unit[2], "wells 1 and 2 are assigned to different wireline units")

# -- 5. Same asymmetric case but with only 1 wireline unit: well 2 must now
# queue behind well 1 -- confirms contention actually bites when capacity is
# tight, not just that the algorithm runs.
r5 <- schedule_pre_frac(
  well_order_index = 1:2,
  ct_workload_days = c(0, 0), wireline_workload_days = c(2, 10), frac_workload_days = c(0, 0),
  ct_units = 1, wireline_units = 1, frac_fleets = 1
)
chk(r5$well_schedule$wireline_start_day[2] == 2, "with 1 unit, well 2 queues behind well 1's wireline (starts at t=2)")
chk(r5$well_schedule$wireline_finish_day[2] > r4$well_schedule$wireline_finish_day[2],
    "dropping wireline_units from 2 to 1 makes well 2 finish later (real contention, not masked by a formula)")

# -- 6. CT runs on an independent timeline -- its schedule for well 2 is not
# gated by well 1's frac finishing, since CT and frac are separate resource
# pools (this is what lets CT "run in parallel with the previous well's
# frac" fall out of the model instead of being a special-cased formula).
r6 <- schedule_pre_frac(
  well_order_index = 1:2,
  ct_workload_days = c(3, 3), wireline_workload_days = c(0, 0), frac_workload_days = c(20, 1),
  ct_units = 1, wireline_units = 1, frac_fleets = 1
)
chk(r6$well_schedule$ct_start_day[2] == 3, "well 2's CT starts right after well 1's CT (t=3), unaffected by well 1's long frac (t=20)")
chk(r6$well_schedule$ct_finish_day[2] == 6, "well 2's CT finishes at t=6, long before well 1's frac finishes at t=20")

# -- 7. Multiple frac fleets: wells distributed by earliest availability, not
# round-robin -- a fleet that finishes a short well early picks up the next
# short well instead of sitting idle while a round-robin assignment would
# hand it to the other fleet.
r7 <- schedule_pre_frac(
  well_order_index = 1:3,
  ct_workload_days = c(0, 0, 0), wireline_workload_days = c(0, 0, 0),
  frac_workload_days = c(1, 1, 10),
  ct_units = 1, wireline_units = 1, frac_fleets = 2
)
chk(r7$well_schedule$frac_fleet[1] != r7$well_schedule$frac_fleet[2],
    "wells 1 and 2 (both available at t=0) are split across the 2 fleets, not serialized on one")
chk(r7$well_schedule$frac_start_day[3] == 1,
    "well 3 starts as soon as a fleet frees up (t=1), not after both short wells serialize on one fleet")
chk(r7$well_schedule$frac_finish_day[3] == 11, "well 3 (workload 10) finishes at t=11 given 2 fleets and 2 short wells ahead of it")

# -- 8. Busy-time totals are exact sums of input workloads, independent of
# contention/queueing -- a sanity invariant that must always hold regardless
# of unit counts.
r8 <- schedule_pre_frac(
  well_order_index = 1:3,
  ct_workload_days = c(1, 2, 3), wireline_workload_days = c(4, 5, 6), frac_workload_days = c(7, 8, 9),
  ct_units = 1, wireline_units = 2, frac_fleets = 2
)
chk(r8$total_ct_busy_days == 6, "total CT busy-days is the exact sum of CT workload (1+2+3)")
chk(r8$total_wireline_busy_days == 15, "total wireline busy-days is the exact sum of wireline workload (4+5+6)")
chk(r8$total_frac_busy_days == 24, "total frac busy-days is the exact sum of frac workload (7+8+9)")

# -- 9. Empty input (n_wells = 0) returns a degenerate-but-valid result
# rather than erroring -- matches schedule_post_frac_milling()'s n==0 guard.
r9 <- schedule_pre_frac(
  well_order_index = integer(0),
  ct_workload_days = numeric(0), wireline_workload_days = numeric(0), frac_workload_days = numeric(0),
  ct_units = 1, wireline_units = 1, frac_fleets = 1
)
chk(nrow(r9$well_schedule) == 0, "n_wells = 0 returns an empty schedule, not an error")
chk(r9$total_wireline_readiness_delay_days == 0, "n_wells = 0: aggregate totals are all zero")

# -- 10. Ample, uniform capacity must produce a BOUNDED, campaign-length-
# independent residual wait -- not zero (a real multi-stage pipeline, CT ->
# wireline -> frac, has a one-time startup transient even with zero workload
# variance: early on, the serial CT queue's accumulated position can
# momentarily exceed a still-fresh frac fleet's unpaced finish time, before
# the fleets build up enough head start that it never recurs). The
# correctness property that distinguishes this from a real bug is that the
# residual must NOT grow with n_wells -- a bug here would look like the wait
# scaling with campaign length; legitimate startup-transient behavior stays
# flat. Found by direct investigation (see PR discussion): an n_wells = 30
# check alone is not enough to tell the two apart, hence asserting equality
# across several very different campaign lengths.
uniform_wait <- function(n) {
  schedule_pre_frac(
    well_order_index = seq_len(n), ct_workload_days = rep(1, n),
    wireline_workload_days = rep(3, n), frac_workload_days = rep(4, n),
    ct_units = 1, wireline_units = 3, frac_fleets = 2
  )$total_wireline_readiness_delay_days
}
waits <- sapply(c(30, 100, 300, 1000), uniform_wait)
chk(length(unique(waits)) == 1,
    sprintf("ample uniform capacity: residual wait is identical across n_wells = 30/100/300/1000 (got %s)",
            paste(waits, collapse = ", ")))
chk(waits[1] > 0,
    "the residual is genuinely nonzero (a real startup transient), not accidentally zero by construction")
chk(waits[1] < 5,
    "the residual is small relative to campaign scale, consistent with a one-time transient, not systemic undersizing")

# -- 11. Attribution split (wireline_capacity_wait_days vs ct_caused_wait_days):
# a slow CT unit can push wireline's own finish time later without wireline
# itself being undersized. Found by direct investigation: with ct_units=1 and
# ample wireline_units, the dashboard's single "waiting on wireline" figure
# was ~99% caused by CT, not wireline capacity at all -- a real attribution
# bug, not a math bug (the total was correct; it was blamed on the wrong
# resource). These two components must (a) sum exactly to the existing total
# at every well, and (b) respond to the resource that actually causes them:
# adding CT units should collapse ct_caused_wait_days while leaving
# wireline_capacity_wait_days roughly unchanged.
r10 <- schedule_pre_frac(
  well_order_index = 1:5,
  ct_workload_days = rep(6, 5), wireline_workload_days = rep(1, 5), frac_workload_days = rep(1, 5),
  ct_units = 1, wireline_units = 5, frac_fleets = 5
)
chk(all(abs(r10$well_schedule$wireline_capacity_wait_days + r10$well_schedule$ct_caused_wait_days -
            r10$well_schedule$wireline_wait_days) < 1e-9),
    "wireline_capacity_wait_days + ct_caused_wait_days == wireline_wait_days exactly, every well")
chk(abs(r10$total_wireline_capacity_wait_days + r10$total_ct_caused_wait_days -
        r10$total_wireline_readiness_delay_days) < 1e-9,
    "the two totals sum exactly to total_wireline_readiness_delay_days")
chk(sum(r10$well_schedule$wireline_capacity_wait_days) < 1e-9,
    "with ample wireline (5 units, 1-day workload each) and a slow single CT unit, wireline's OWN capacity contributes ~0 wait")
chk(r10$total_ct_caused_wait_days > 0,
    "the wait that exists is correctly attributed to CT, not wireline capacity")

# Adding CT units should shrink ct_caused_wait_days monotonically, then
# PLATEAU once ct_units >= n_wells (every well gets a dedicated unit, zero
# queueing left to remove). It must NOT necessarily hit ~0 at that point --
# here ct_workload_days (6) exceeds wireline+frac's own pace (1+1=2), so
# CT's precedence over wireline (it must finish before wireline starts, for
# every well, real units or not) leaves an inherent per-well floor that more
# CT UNITS cannot remove -- only a shorter CT task could. Conflating that
# floor with queueing-driven delay (which #54's real-data investigation
# showed CAN collapse toward ~0 when CT's own duration is modest) would be
# exactly the kind of unjustified "just add more CT units" advice this
# attribution split exists to avoid giving.
ct_caused_by_units <- sapply(c(1, 2, 4, 8, 10, 20), function(ctu) {
  schedule_pre_frac(
    well_order_index = 1:10, ct_workload_days = rep(6, 10),
    wireline_workload_days = rep(1, 10), frac_workload_days = rep(1, 10),
    ct_units = ctu, wireline_units = 10, frac_fleets = 10
  )$total_ct_caused_wait_days
})
chk(all(diff(ct_caused_by_units) <= 1e-9),
    sprintf("ct_caused_wait_days is non-increasing as ct_units grows 1->2->4->8->10->20 (got %s)",
            paste(round(ct_caused_by_units, 2), collapse = ", ")))
chk(ct_caused_by_units[5] < ct_caused_by_units[1],
    "ct_caused_wait_days does shrink substantially from 1 unit (heavy queueing) to 10 units (one per well)")
chk(ct_caused_by_units[5] == ct_caused_by_units[6],
    "ct_caused_wait_days plateaus once ct_units >= n_wells (10 == 20): no more queueing left to remove")
chk(ct_caused_by_units[5] > 0,
    "the plateau is correctly nonzero -- CT's own per-well duration (6) exceeds wireline+frac's pace (1+1), a real duration floor, not a bug")

# -- 12. Second-level split: ct_caused_wait_days = ct_queueing_wait_days +
# ct_duration_floor_wait_days. Distinguishes delay removable by adding CT
# units (queueing) from a floor that survives even with a dedicated CT unit
# per well (CT's own task takes longer than wireline+frac's pace). Without
# this, "add CT capacity" advice would overstate what more units can fix --
# the plateau in test 11 above (60 at ct_units=10/20) should now resolve to
# ALL duration_floor and ZERO queueing once units stop being scarce.
floors_and_queues <- lapply(c(1, 2, 4, 8, 10, 20), function(ctu) {
  schedule_pre_frac(
    well_order_index = 1:10, ct_workload_days = rep(6, 10),
    wireline_workload_days = rep(1, 10), frac_workload_days = rep(1, 10),
    ct_units = ctu, wireline_units = 10, frac_fleets = 10
  )
})
chk(all(sapply(floors_and_queues, function(r)
  abs(r$total_ct_queueing_wait_days + r$total_ct_duration_floor_wait_days - r$total_ct_caused_wait_days) < 1e-9)),
    "ct_queueing_wait_days + ct_duration_floor_wait_days == ct_caused_wait_days exactly, every ct_units value")
chk(all(sapply(floors_and_queues, function(r) all(abs(
  r$well_schedule$ct_queueing_wait_days + r$well_schedule$ct_duration_floor_wait_days -
  r$well_schedule$ct_caused_wait_days) < 1e-9))),
    "the same identity holds at every individual well, not just in aggregate")
duration_floors <- sapply(floors_and_queues, `[[`, "total_ct_duration_floor_wait_days")
chk(length(unique(round(duration_floors, 6))) == 1,
    sprintf("ct_duration_floor_wait_days is IDENTICAL regardless of ct_units (got %s) -- it's a floor, not a queueing effect",
            paste(round(duration_floors, 2), collapse = ", ")))
queueing_at_plateau <- floors_and_queues[[5]]$total_ct_queueing_wait_days  # ct_units = 10 = n_wells
chk(abs(queueing_at_plateau) < 1e-9,
    "ct_queueing_wait_days is exactly 0 once ct_units == n_wells (no queueing left to remove)")
chk(floors_and_queues[[1]]$total_ct_queueing_wait_days > floors_and_queues[[1]]$total_ct_duration_floor_wait_days,
    "at ct_units=1 (heavy contention), queueing dominates the total -- this is the regime adding CT units actually fixes")

# -- 13. Pure duration-floor case (no queueing at all, ct_units already
# ample): ct_queueing_wait_days must be exactly 0 and ct_duration_floor_wait_days
# must equal ct_caused_wait_days exactly -- confirms the split correctly
# attributes 100% to the floor when there is genuinely no queueing to find.
r11 <- schedule_pre_frac(
  well_order_index = 1:3, ct_workload_days = rep(5, 3),
  wireline_workload_days = rep(1, 3), frac_workload_days = rep(1, 3),
  ct_units = 3, wireline_units = 3, frac_fleets = 3
)
chk(abs(r11$total_ct_queueing_wait_days) < 1e-9,
    "one dedicated CT unit per well: zero queueing wait, even though CT's own duration (5) exceeds wireline+frac's pace (1+1)")
chk(abs(r11$total_ct_duration_floor_wait_days - r11$total_ct_caused_wait_days) < 1e-9,
    "with zero queueing, the entire ct_caused_wait_days is correctly attributed to the duration floor")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
