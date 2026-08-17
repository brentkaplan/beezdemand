# Find the smallest sample size reaching a target power (demand)

Bisection search over `n_subjects` for the smallest N whose Monte Carlo
power estimate from
[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
reaches `target_power`. The search accounts for Monte Carlo noise: at
each evaluated N, replicates are added in batches (up to `n_sim_max`)
until the Wilson interval for power lies wholly above or below the
target; if it still straddles the target at the cap, the decision falls
back to the point estimate and the result is flagged `uncertain`. The
selected N and its lower neighbor are then re-evaluated with fresh
replicates before minimality is claimed.

The returned `n` is an *estimated minimum under Monte Carlo
uncertainty*, not an exact bound. For grant-quality reporting, rerun
[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
at the returned `n` with a large `n_sim` (2000+) and report that
estimate with its Monte Carlo confidence interval.

**Monotonicity assumption.** Bisection presumes that power is
non-decreasing in `n_subjects`. Because every evaluated N is judged from
its own independent replicates (and a convergence-conditioned
denominator), a Monte Carlo fluctuation can make a lower N read "below"
when its true power is above the target, so the search may step past a
lower crossing that it never revisits. Evaluated N that contradict the
assumption (a lower N reading "above" the selected N) demote the status
to `"uncertain"`; N that were never evaluated cannot be checked. Widen
`n_sim`/`n_sim_max` when the reported `n` matters. When the target is
already met at `n_range[1]`, that bound is likewise re-evaluated with
fresh replicates before `"at_lower_bound"` is reported; if the second
look does not clear the target the bound is treated as below and the
bisection proceeds upward.

## Usage

``` r
find_n_demand(
  target_power = 0.8,
  effect = list(delta_q0 = NULL, delta_alpha = NULL),
  design = list(),
  n_range = c(6, 200),
  n_sim = 200,
  n_sim_max = 4 * n_sim,
  alpha = 0.05,
  df = NULL,
  seed = NULL,
  equation = "simplified",
  random_effects = NULL,
  multi_start = FALSE,
  verbose = TRUE,
  design_type = c("within", "between"),
  ...
)
```

## Arguments

- target_power:

  Target power in (0, 1).

- effect:

  Named list supplying exactly one of `delta_q0` or `delta_alpha`: the
  true condition shift on natural-log Q0 (or natural-log alpha) for
  condition 2 relative to condition 1. `0` is allowed (useful for Type I
  error checks). E.g. `delta_q0 = log(1.5)` means condition 2's Q0 is
  1.5 times condition 1's.

- design:

  Named list of data-generating settings, merged over the simulator
  defaults: `prices` (vector), `log_q0_pop`, `log_alpha_pop`, `sigma_b`
  (per-condition subject SD on log Q0), `sigma_d` (same for log alpha),
  `rho_bd` (must be 0 in this version), and `sigma_e` (residual SD on
  log consumption). All SDs must be strictly positive.

- n_range:

  Integer bracket `c(lower, upper)` to search (`2 <= lower < upper`).
  The search errors (rather than extrapolating) if the target is not
  reached at `upper`.

- n_sim:

  Replicates per evaluation batch. Smaller than the
  [`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
  default because several N values are evaluated; the adaptive rule adds
  batches where the verdict is close.

- n_sim_max:

  Maximum replicates per evaluated N (default `4 * n_sim`).

- alpha:

  Nominal two-sided test level.

- df:

  Degrees of freedom for the Wald test's t reference. `NULL` (default)
  tracks the evaluated sample size as `n - 1` for
  `design_type = "within"` and `n - 2` for `design_type = "between"`; a
  numeric value (or `Inf` for the asymptotic z-test) is used at every
  evaluated N. With the default `df` and `design_type = "between"`,
  `n_range[1]` must be `>= 3` (df = n - 2 needs n \>= 3).

- seed:

  Optional integer seed; identical seeds give identical results. The
  caller's RNG state is restored on exit.

- equation:

  Demand equation passed to
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md).
  The default `"simplified"` shares the simulator's mean function, so
  the simulated `delta` and the fitted contrast share the same scale.
  Note the error model is a *working model*: the simulator draws
  multiplicative lognormal errors while `"simplified"` fits additive
  Gaussian errors on raw consumption; the approximation is closest at
  small `sigma_e`, and Type I calibration is verified by the test suite
  at the default and a 3x-larger `sigma_e`. Other equations are
  sensitivity analyses with a different estimand – their contrasts are
  not on the scale of the simulated delta.

- random_effects:

  Random-effects specification passed to
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md).
  `NULL` (default) resolves to a specification matching `design_type`:
  `nlme::pdDiag(Q0 + alpha ~ condition - 1)` for `"within"` (independent
  per-condition subject effects on both parameters, matching the
  simulator's data-generating process when `rho_bd = 0`), and
  `nlme::pdDiag(Q0 + alpha ~ 1)` for `"between"` (per-subject
  intercepts, which are correctly specified because each subject appears
  in only one condition). Supply a specification to override.

- multi_start:

  Passed to
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md).
  Defaults to `FALSE` for speed (roughly 3x fewer optimizations);
  non-convergent replicates are excluded and surfaced rather than
  biasing the estimate.

- verbose:

  Logical; report each evaluation.

- design_type:

  Either `"within"` (default) or `"between"`. `"within"` simulates the
  two-condition within-subject design (every subject observed at every
  price in both conditions) and tests the within-subject condition
  contrast. `"between"` assigns each subject to exactly one of two arms
  (`ceiling(n/2)` to condition 1, the rest to condition 2) and tests the
  group difference; the two-arm dataset is composed from the same
  simulator run once per arm with a single condition, so no new
  data-generating process is introduced (see Details). The `df` and
  `random_effects` defaults track `design_type`.

- ...:

  Additional arguments passed to
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md).

## Value

An object of class `beezdemand_power_n`: a list with

- n:

  Estimated smallest `n_subjects` reaching `target_power`; `NA` when the
  confirmation pass contradicted the search (`status = "unresolved"`).

- target_power:

  As supplied.

- status:

  `"confirmed"` (selected N re-confirmed above target and N - 1 below),
  `"uncertain"` (a decision relied on a point estimate, N - 1 also
  cleared the target on reconfirmation, or an evaluated lower N read
  above the target – so the returned N may not be minimal),
  `"unresolved"` (the selected N failed its own reconfirmation; `n` is
  `NA`), or `"at_lower_bound"` (the target was already met at
  `n_range[1]` on two independent looks; smaller N was not explored –
  widen `n_range` downward if that matters). These labels describe a
  heuristic Monte Carlo decision rule – repeated looks at ordinary
  Wilson intervals across several N – not a formal sequential error
  guarantee.

- uncertain:

  Logical; `TRUE` when any search decision was made on a point estimate
  rather than a conclusive Wilson interval, or the status is not
  `"confirmed"`/`"at_lower_bound"`.

- evaluations:

  Tibble of every evaluation: `n_subjects`, `n_sim_total`, `n_used`,
  `usable_fraction`, `power`, `ci_lower`, `ci_upper`, `decision`. A
  warning fires when any evaluation had fewer than 95% usable fits.

- alpha, df, effect, design, n_range, n_sim, n_sim_max, seed, settings,
  call:

  Echoed inputs and effective settings.

## See also

[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
for the Monte Carlo engine.

Other power-analysis:
[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)

## Examples

``` r
# \donttest{
# Tiny search for demonstration only (use n_sim >= 200 and a wide
# n_range for real planning; see vignette("power-analysis"))
res <- find_n_demand(
  target_power = 0.8,
  effect = list(delta_q0 = log(2.5)),
  n_range = c(4, 8), n_sim = 5, n_sim_max = 10, seed = 1, verbose = FALSE
)
print(res)
#> Sample-size search (Monte Carlo power)
#>   Design: within-subject (2 conditions)
#>   Target power 0.80 for delta_q0 = 0.9163 at alpha = 0.05
#>   Estimated minimum n_subjects = 5 (status: uncertain)
#>   This is an estimated minimum under Monte Carlo uncertainty;
#>   rerun the power function at this N with a large n_sim to report it.
#> 
#>   Evaluations:
#>  n_subjects n_sim_total n_used usable_fraction power  ci_lower  ci_upper
#>           8          10     10               1   1.0 0.7224672 1.0000000
#>           4          10     10               1   0.7 0.3967781 0.8922087
#>           6          10     10               1   1.0 0.7224672 1.0000000
#>           5          10     10               1   0.9 0.5958500 0.9821238
#>           5          10     10               1   1.0 0.7224672 1.0000000
#>           4          10     10               1   1.0 0.7224672 1.0000000
#>         decision
#>  ambiguous_above
#>  ambiguous_below
#>  ambiguous_above
#>  ambiguous_above
#>  ambiguous_above
#>  ambiguous_above
# }
```
