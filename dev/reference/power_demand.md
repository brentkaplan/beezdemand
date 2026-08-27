# Monte Carlo power analysis for two-condition demand designs

Estimates statistical power to detect a single fixed-effect difference
in a demand parameter (`Q0` or `alpha`) between two conditions, by
simulation: each replicate (1) simulates a two-condition dataset from
the mixed-effects demand model in
[`.simulate_within_subject_demand()`](https://brentkaplan.github.io/beezdemand/reference/dot-simulate_within_subject_demand.md)
under assumed population parameters plus the effect `delta`, under
either a within-subject design (every subject observed in both
conditions) or, with `design_type = "between"`, a two-arm
between-subject design (each subject in one condition); (2) refits it
with
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md);
and (3) tests the condition contrast on the estimation (natural log)
scale with a Wald test at level `alpha`, referred to a t distribution
with `df` degrees of freedom (see the `df` argument). Power is the
proportion of *usable* fits (converged, positive-definite Hessian,
finite standard error) that reject.

Because the power estimate is a proportion from finitely many
replicates, it is reported with a Wilson score confidence interval
(`power_mc_ci`). Both a p-value verdict (`p < alpha`) and a
confidence-interval verdict (Wald CI excludes 0) are recorded per
replicate; they use the same standard error and reference distribution,
so they coincide by construction, and both rates are returned.

## Usage

``` r
power_demand(
  n_subjects,
  effect = list(delta_q0 = NULL, delta_alpha = NULL),
  design = list(),
  n_sim = 500,
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

- n_subjects:

  Number of simulated subjects per replicate. For
  `design_type = "within"` each subject is observed at every price in
  both conditions; for `"between"` this is the *total* sample, split
  `ceiling(n_subjects / 2)` to condition 1 and the rest to condition 2
  (an odd total therefore gives arms differing by one subject).

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

- n_sim:

  Number of Monte Carlo replicates. 500 (default) is suitable for
  interactive exploration; use 2000+ for grant-quality precision (see
  [`vignette("power-analysis")`](https://brentkaplan.github.io/beezdemand/articles/power-analysis.md)).

- alpha:

  Nominal two-sided test level.

- df:

  Degrees of freedom for the Wald test's t reference distribution.
  `NULL` (default) uses `n_subjects - 1` for `design_type = "within"`
  and `n_subjects - 2` for `design_type = "between"` (the two-sample
  df). This is an *empirically calibrated* small-sample correction
  rather than a model-derived df (the TMB fit has no exact t sampling
  theory): the asymptotic z-test was measurably anticonservative in the
  package's Type I calibration battery (empirical rate 0.089 at nominal
  .05 with 15 subjects), while the t reference passes the battery's null
  checks across the tested sample sizes, target parameters, residual-SD
  settings, and both designs. `Inf` gives the asymptotic z-test.

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
  sensitivity analyses with a different estimand, so their contrasts are
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

  Logical; show a progress bar.

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
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  (e.g. `tmb_control`).

## Value

An object of class `beezdemand_power`: a list with

- power:

  Estimated power: proportion of usable replicates whose Wald CI
  excludes 0 (equal to `hit_rate_ci`). `NA` if no replicate was usable.

- power_mc_ci:

  Wilson 95% confidence interval on `power`, reflecting Monte Carlo
  uncertainty from `n_used` replicates.

- hit_rate_p:

  Proportion of usable replicates with `p < alpha`.

- hit_rate_ci:

  Proportion of usable replicates whose Wald CI excludes 0 (the same
  decision rule as `hit_rate_p`, since both use the same SE and t
  reference; both reported).

- n_sim:

  Total replicates attempted.

- n_converged:

  Replicates whose fit converged.

- n_hessian_pd:

  Replicates with a positive-definite Hessian.

- n_used:

  Replicates entering the power denominator (converged,
  positive-definite Hessian, finite SE).

- alpha:

  Nominal test level.

- df:

  Degrees of freedom of the t reference distribution actually used
  (`n_subjects - 1` for `"within"`, `n_subjects - 2` for `"between"`,
  unless overridden).

- effect:

  The validated effect specification (name and delta).

- target_term:

  The tested coefficient (e.g. `"Q0:conditionC2"`).

- design:

  The merged design list actually used.

- n_subjects:

  As supplied.

- replicates:

  Tibble with one row per replicate: `sim`, `status` (`"ok"`,
  `"nonconverged"`, `"hessian_not_pd"`, `"se_unusable"`, `"error"`),
  `converged`, `hessian_pd`, `estimate`, `se`, `statistic`, `p_value`,
  `ci_lower`, `ci_upper`, `hit_p`, `hit_ci`, and `message` (error text,
  if any). Estimates are on the natural-log scale of the simulated
  delta.

- seed:

  As supplied.

- settings:

  List of `equation`, `design_type`, `multi_start`, and the deparsed
  random-effects specification.

- call:

  The matched call.

## Details

A replicate whose fit fails (non-convergence, non-positive-definite
Hessian, unusable standard error, or an error) is excluded from the
power denominator and reported through the `n_*` counts and
`$replicates$status`. It is never counted as "no effect detected", which
would bias power in an unpredictable direction. A warning is issued when
fewer than 95% of replicates are usable, since power conditional on
convergence can be selected when convergence depends on the realized
data.

The v1 scope is a single fixed-effect delta. Joint Q0 + alpha effects,
power for derived measures (Pmax, Omax), and arbitrary designs are out
of scope; see
[`vignette("power-analysis")`](https://brentkaplan.github.io/beezdemand/articles/power-analysis.md).

For `design_type = "between"`, the two arms are composed by running the
within-subject simulator once per arm with a single condition, then
binding the arms and refitting with per-subject intercept random
effects. Because each subject appears in only one condition, that
random-effects *structure* matches the composed data-generating process
exactly, unlike the within-subject default's per-condition effects. The
additive-Gaussian residual likelihood remains a *working model* for the
simulator's multiplicative-lognormal errors in both designs (closest at
small `sigma_e`). Type I error is calibrated by the test suite for both
designs.

## See also

[`find_n_demand()`](https://brentkaplan.github.io/beezdemand/reference/find_n_demand.md)
to search for the smallest adequate sample size;
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
for the model being refit.

Other power-analysis:
[`find_n_demand()`](https://brentkaplan.github.io/beezdemand/reference/find_n_demand.md)

## Examples

``` r
# \donttest{
# Tiny illustrative run (use n_sim >= 500 for real planning; the
# Monte Carlo interval at n_sim = 5 is uninformative by design)
res <- power_demand(
  n_subjects = 12,
  effect = list(delta_q0 = log(1.5)),
  n_sim = 5, seed = 1, verbose = FALSE
)
print(res)
#> Monte Carlo power analysis (beezdemand)
#>   Design: within-subject (2 conditions)
#>   Target: Q0:conditionC2 (delta_q0 = 0.4055), two-sided alpha = 0.05, t reference (df = 11)
#>   n_subjects = 12, n_sim = 5 (converged 5, usable 5)
#>   Power (CI-exclusion): 1.000 [95% MC CI 0.566, 1.000]
#>   p-value hit rate:     1.000

# Between-subject design: group difference in Q0 across two arms
res_b <- power_demand(
  n_subjects = 16,
  effect = list(delta_q0 = log(1.5)),
  design_type = "between",
  n_sim = 5, seed = 1, verbose = FALSE
)
print(res_b)
#> Monte Carlo power analysis (beezdemand)
#>   Design: between-subject (2 conditions)
#>   Target: Q0:conditionC2 (delta_q0 = 0.4055), two-sided alpha = 0.05, t reference (df = 14)
#>   n_subjects = 16, n_sim = 5 (converged 5, usable 5)
#>   Power (CI-exclusion): 0.800 [95% MC CI 0.376, 0.964]
#>   p-value hit rate:     0.800
# }
```
