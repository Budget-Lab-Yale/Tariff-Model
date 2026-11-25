# Brief: short-run model for history, GTAP blend from 2027 Q2 (applies to ALL runs)

Decision by the model owner (John Ricco): GTAP is comparative statics and has
no clock, so it should not shape the 2025-26 history. Change the macro timing
for every run (current law and scenarios alike):

1. **Blend start and length.** The quarterly GDP path (src/05_calculate_macro.R,
   `blend_usmm_gdp_deviation()` in src/helpers.R) currently moves from the USMM
   surrogate (weight 1 at 2025Q1) to the GTAP long-run GDP over 16 quarters.
   Change to: pure USMM through 2027Q1; blend starts 2027Q2 and reaches full
   GTAP long run after 12 quarters (2030Q2). Make start year, start quarter and
   length configurable (global_assumptions.yaml or similar, with these values as
   the defaults), not hard-coded at call sites. Apply the same timing to every
   other place that blends toward GTAP or phases in a long-run response over
   time — in particular the revenue phase-in in src/04_calculate_revenue.R
   ("converge LINEARLY to full GTAP over 5 fiscal years from anchor_fy"): find
   every such phase-in and report what you changed and what you left alone,
   and why.

2. **Short-run surrogate timing.** src/05a_usmm_surrogate.R `decompose_shocks()`
   treats the LAST date's tariff level as a permanent shock from 2025Q2 and
   nets earlier dates as temporary components. A rate increase that starts in
   2027 therefore changes the 2025-26 response. Change it so the permanent
   path is built as a sequence of permanent shocks, each entering at its own
   date with size equal to that date's change in the permanent level (shift the
   IRF onset to that quarter — `shift_irf()` exists), keeping the existing
   temporary-component machinery for explicitly temporary pieces
   (temp_component_dates) and the refund scaling. Preserve the interpolation /
   extrapolation over the 2/5/10-point brackets per component, and document the
   linearity assumption that summing shifted responses implies.

## Must hold (write unit tests)
- Two rate schedules that are identical before date D produce identical GDP,
  unemployment, payroll and revenue paths for every quarter / fiscal year
  before D (exactly, or to 1e-10). Test with D = 2027-04-01.
- A schedule with a single change in 2025Q2 reproduces today's USMM response
  for that schedule (the old and new decomposition agree when there is only
  one permanent step).
- GDP is pure USMM through 2027Q1 and pure GTAP long run from 2030Q2.
- Existing tests still pass (update only those that encode the old timing, and
  say which).

## Limits
Light only (login node, 5 GB): no GTAP, no full runs, no Slurm. Do not commit
or push. Work only in this worktree. The four scenario configs under
config/scenarios/tracker_*_VINTAGE_TBD stay as they are.

## Report
Plain English for a public finance economist, no jargon, under 40 lines: what
changed, how the published current-law 2025-26 numbers will move (direction,
and rough size if you can tell from the code and test fixtures), test results,
caveats.
