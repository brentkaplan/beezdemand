# Multi-Start Rescue Protocol for fit_demand_fixed()

Internal helpers implementing the multi-start rescue protocol.
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
always runs the legacy
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
heuristic exactly as before (the "production start"). A subject whose
production fit is strict-converged (`converged_strict`: optimizer
`isConv` AND finite coefficients/objective AND not sitting on a
user-supplied bound) is accepted immediately and no sampled starts are
ever run for it; its row/fit/prediction/data entries stay byte-identical
to the single-start (`S = 1`) protocol by construction. Only subjects
whose production fit is not strict-converged are re-fit from `S - 1`
additional sampled starting values; among the sampled attempts that
themselves strict-converge, the minimum-`AbsSS` (residual sum of
squares) start wins, ties resolved by draw order (first index).
