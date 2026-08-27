# Power Analysis for Demand Designs

The Monte Carlo runs below are precomputed (the full set of
[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
/
[`find_n_demand()`](https://brentkaplan.github.io/beezdemand/reference/find_n_demand.md)
calls in this vignette takes several minutes, well past CRAN’s
vignette-build budget), so the outputs shown come from real runs; copy
the chunks into your session to reproduce them.

## Why simulation-based power?

Before running a study, a researcher planning a demand experiment needs
an answer to: *“How many subjects do I need to detect an effect of this
size, with this design, at 80% power?”* Closed-form power formulas do
not exist for nonlinear mixed-effects demand models, and Wald
asymptotics are unreliable at realistic study sizes, especially near
boundary parameters.
[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
therefore estimates power by Monte Carlo simulation in five steps:

1.  Simulate a dataset under assumed true population parameters, a
    hypothesized effect size, and the study design.
2.  Refit it with the package’s TMB mixed-effects fitter,
    \[fit_demand_tmb()\].
3.  Extract the Wald standard error of the target condition contrast
    from the TMB `sdreport`.
4.  Record both a p-value verdict (Wald test against `alpha`) and a
    confidence-interval verdict (does the Wald CI exclude 0). Both are
    referred to a t distribution (with `n_subjects - 1` degrees of
    freedom within-subject, `n_subjects - 2` between-subject) rather
    than the asymptotic normal: the calibration battery showed the
    z-test is anticonservative at study-relevant N (empirical Type I
    error 0.089 at nominal .05 with 15 subjects), while the null Wald
    statistics closely follow the t with the design’s contrast df. The
    two verdicts use the same standard error and reference distribution,
    so they always agree (both report formats are returned). `df = Inf`
    recovers the asymptotic z-test if you need it.
5.  Repeat `n_sim` times. Power is the proportion of *usable* fits
    (converged, positive-definite Hessian, finite SE) that reject,
    reported with a Wilson confidence interval because it is itself an
    estimate from finitely many replicates.

The data-generating model is the package’s existing within-subject
demand simulator: every subject is observed at every price under both
levels of a within-subject condition, with per-(subject, condition)
random effects on log Q0 and log alpha and lognormal residual error. The
effect of interest is a shift on natural-log Q0 (`delta_q0`) *or*
natural-log alpha (`delta_alpha`) in condition 2 relative to
condition 1. For example, `delta_q0 = log(1.5)` means condition 2’s
intensity is 1.5 times condition 1’s. The same simulator also drives a
two-arm *between-subject* design (`design_type = "between"`, described
below), where each subject is assigned to one condition and the effect
is the group difference.

## Estimating power at a fixed sample size

Suppose we expect a 50% increase in Q0 (`delta_q0 = log(1.5) ≈ 0.41`)
and plan 20 subjects. The `design` list sets the assumed population
values and price grid (defaults shown in
[`?power_demand`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)):

``` r

res <- power_demand(
  n_subjects = 20,
  effect = list(delta_q0 = log(1.5)),
  design = list(
    prices = c(0.1, 0.5, 1, 2, 5, 10),
    sigma_b = 0.3,  # subject SD on log Q0, per condition
    sigma_d = 0.3,  # subject SD on log alpha, per condition
    sigma_e = 0.1   # residual SD on log consumption
  ),
  n_sim = 40,       # small for a fast vignette; see guidance below
  seed = 1234,
  verbose = FALSE
)
res
#> Monte Carlo power analysis (beezdemand)
#>   Design: within-subject (2 conditions)
#>   Target: Q0:conditionC2 (delta_q0 = 0.4055), two-sided alpha = 0.05, t reference (df = 19)
#>   n_subjects = 20, n_sim = 40 (converged 40, usable 40)
#>   Power (CI-exclusion): 1.000 [95% MC CI 0.912, 1.000]
#>   p-value hit rate:     1.000
```

The Monte Carlo confidence interval (`power_mc_ci`) is wide at
`n_sim = 40`, which is the point of reporting it. Interactive
exploration is fine at a few hundred replicates; numbers destined for a
grant application should use `n_sim = 2000` or more (see the guidance
section below).

Convergence diagnostics are part of the output. Replicates whose refit
fails are excluded from the power denominator and counted in
`n_converged` / `n_used` rather than treated as “no effect detected”.

``` r

res$n_converged
#> [1] 40
res$n_used
#> [1] 40
table(res$replicates$status)
#> 
#> ok 
#> 40
```

## Searching for a sample size

[`find_n_demand()`](https://brentkaplan.github.io/beezdemand/reference/find_n_demand.md)
wraps the engine in a bisection search over `n_subjects` for the
smallest N whose estimated power reaches the target. The search is aware
of Monte Carlo noise: at each candidate N it adds replicates until the
Wilson interval falls clearly above or below the target (up to
`n_sim_max`), and it re-evaluates the selected N and its lower neighbor
(or the lower bound itself, when the target is already met there) before
reporting. Bisection assumes power is monotone in N; because each N is
judged from independent replicates, a fluctuation at a lower N can hide
a crossing the search never revisits. Evaluated N that contradict
monotonicity demote the status to `uncertain`, but never-visited N
cannot be checked. Treat the result as an estimate and confirm it with a
large `n_sim` at the chosen N.

``` r

search <- find_n_demand(
  target_power = 0.8,
  effect = list(delta_q0 = log(2)),
  design = list(prices = c(0.1, 0.5, 1, 2, 5, 10)),
  n_range = c(4, 30),
  n_sim = 30,       # small for a fast vignette
  seed = 5678,
  verbose = FALSE
)
search
#> Sample-size search (Monte Carlo power)
#>   Design: within-subject (2 conditions)
#>   Target power 0.80 for delta_q0 = 0.6931 at alpha = 0.05
#>   Estimated minimum n_subjects = 5 (status: uncertain)
#>   This is an estimated minimum under Monte Carlo uncertainty;
#>   rerun the power function at this N with a large n_sim to report it.
#> 
#>   Evaluations:
#>  n_subjects n_sim_total n_used usable_fraction     power  ci_lower  ci_upper
#>          30          30     30               1 1.0000000 0.8864866 1.0000000
#>           4          30     30               1 0.6333333 0.4551356 0.7812608
#>          17          30     30               1 1.0000000 0.8864866 1.0000000
#>          10          30     30               1 1.0000000 0.8864866 1.0000000
#>           7          30     30               1 1.0000000 0.8864866 1.0000000
#>           5          60     60               1 0.9166667 0.8193106 0.9638795
#>           5         120    120               1 0.8083333 0.7287884 0.8687497
#>           4          30     30               1 0.6000000 0.4232036 0.7540937
#>         decision
#>            above
#>            below
#>            above
#>            above
#>            above
#>            above
#>  ambiguous_above
#>            below
```

The result is an *estimated minimum under Monte Carlo uncertainty* (the
`status` field says whether the confirmation pass was conclusive). For a
defensible grant number, rerun
[`power_demand()`](https://brentkaplan.github.io/beezdemand/reference/power_demand.md)
at the returned N with a large `n_sim` and report that estimate with its
Monte Carlo interval.

## Between-subject designs

The examples above are *within-subject*: every subject is observed at
every price under both conditions, and the test is the within-subject
condition contrast. Set `design_type = "between"` to plan a two-group
study instead, where each subject is assigned to exactly one arm and the
test is the group difference on log-Q0 (or log-alpha):

``` r

res_b <- power_demand(
  n_subjects = 40,          # total across both arms (20 per arm)
  effect = list(delta_q0 = log(1.5)),
  design = list(prices = c(0.1, 0.5, 1, 2, 5, 10)),
  design_type = "between",
  n_sim = 40,
  seed = 1234,
  verbose = FALSE
)
res_b
#> Monte Carlo power analysis (beezdemand)
#>   Design: between-subject (2 conditions)
#>   Target: Q0:conditionC2 (delta_q0 = 0.4055), two-sided alpha = 0.05, t reference (df = 38)
#>   n_subjects = 40, n_sim = 40 (converged 40, usable 40)
#>   Power (CI-exclusion): 0.975 [95% MC CI 0.871, 0.996]
#>   p-value hit rate:     0.975
```

`n_subjects` is the *total* sample; it is split `ceiling(n / 2)` to the
first arm and the rest to the second. Two defaults track `design_type`
automatically: the Wald t reference uses `df = n - 2` (the two-sample
df, versus `n - 1` within-subject), and the random-effects specification
becomes per-subject intercepts (`pdDiag(Q0 + alpha ~ 1)`) rather than
the per-condition effects used within-subject. Both are overridable.

[`find_n_demand()`](https://brentkaplan.github.io/beezdemand/reference/find_n_demand.md)
takes the same argument. Because the default `df = n - 2` requires at
least three subjects, the search bracket’s lower bound must be `>= 3`
for a between-subject design (the function errors otherwise):

``` r

search_b <- find_n_demand(
  target_power = 0.8,
  effect = list(delta_q0 = log(2)),
  design = list(prices = c(0.1, 0.5, 1, 2, 5, 10)),
  design_type = "between",
  n_range = c(6, 60),
  n_sim = 30,
  seed = 5678,
  verbose = FALSE
)
search_b
#> Sample-size search (Monte Carlo power)
#>   Design: between-subject (2 conditions)
#>   Target power 0.80 for delta_q0 = 0.6931 at alpha = 0.05
#>   Estimated minimum n_subjects = 8 (status: uncertain)
#>   This is an estimated minimum under Monte Carlo uncertainty;
#>   rerun the power function at this N with a large n_sim to report it.
#> 
#>   Evaluations:
#>  n_subjects n_sim_total n_used usable_fraction     power  ci_lower  ci_upper
#>          60          30     30               1 1.0000000 0.8864866 1.0000000
#>           6          90     90               1 0.7000000 0.5987349 0.7848909
#>          33          30     30               1 1.0000000 0.8864866 1.0000000
#>          19          30     30               1 1.0000000 0.8864866 1.0000000
#>          12          60     60               1 0.9500000 0.8629948 0.9828505
#>           9          30     30               1 0.9666667 0.8332961 0.9940914
#>           7          30     30               1 0.6000000 0.4232036 0.7540937
#>           8          30     30               1 0.9666667 0.8332961 0.9940914
#>           8         120    120               1 0.8666667 0.7943520 0.9162339
#>           7         120    120               1 0.7833333 0.7014564 0.8476328
#>         decision
#>            above
#>            below
#>            above
#>            above
#>            above
#>            above
#>            below
#>            above
#>  ambiguous_above
#>  ambiguous_below
```

## Sensitivity to the assumed effect size

Power claims are conditional on the assumed effect and variance
components. A small sweep makes that dependence visible:

``` r

deltas <- c(log(1.25), log(1.5), log(2))
sweep <- vapply(deltas, function(d) {
  power_demand(
    n_subjects = 20, effect = list(delta_q0 = d),
    design = list(prices = c(0.1, 0.5, 1, 2, 5, 10)),
    n_sim = 30, seed = 42, verbose = FALSE
  )$power
}, numeric(1))
data.frame(q0_ratio = exp(deltas), power = sweep)
#>   q0_ratio power
#> 1     1.25   0.6
#> 2     1.50   1.0
#> 3     2.00   1.0
```

## How many replicates do you need?

The Monte Carlo standard error of a power estimate near `p` is
`sqrt(p (1 - p) / n_sim)`:

| `n_sim` | MC SE near power = 0.8 | Use                                  |
|--------:|-----------------------:|--------------------------------------|
|     100 |                  0.040 | quick interactive exploration        |
|     500 |                  0.018 | default; serious exploration         |
|    2000 |                  0.009 | grant applications, preregistrations |

Always report the Wilson interval (`power_mc_ci`) alongside the point
estimate; a power of “0.80” from 100 replicates is `[0.71, 0.87]`.

## Validity and Limitations

**Validity checks.** The package test suite
(`tests/testthat/test-power-demand.R`) verifies, with preregistered
seeds and tolerance bands fixed before the tests were first run:

- **Type I error calibration** (the primary check): with the effect set
  to zero, the empirical false-positive rate at nominal `alpha = .05`
  over 1,200 replicates must fall in \[0.03, 0.07\], a band of 3.18
  binomial standard errors that excludes both half and 1.5 times the
  nominal rate. `n_sim = 1200` was computed from that tolerance
  (`9 * .05 * .95 / .02^2 ≈ 1069`). A second null check runs at a
  realistic N = 40 on the alpha contrast. This check is what motivated
  the t reference distribution: the first run, using the asymptotic
  z-test, failed it (empirical rate 0.089 at n = 15), and the t(n - 1)
  reference was adopted and re-validated rather than widening the band.
  The between-subject design carries its own preregistered calibration
  and larger-N null checks against the same bands, with the two-sample
  `t(n - 2)` reference.
- **Convergence handling**: a configuration that reliably produces
  non-convergence confirms failed replicates are excluded from the power
  denominator and surfaced via `n_converged` / `n_used` rather than
  silently counted as misses.
- **Closed-form benchmark**: in a degenerate configuration (tiny
  residual and alpha-side variability) the within-subject design reduces
  to a paired comparison of per-condition log-Q0 values, and the
  between-subject design reduces to a two-sample comparison; each Monte
  Carlo estimate must match the corresponding analytic power from
  [`pwr::pwr.t.test()`](https://rdrr.io/pkg/pwr/man/pwr.t.test.html)
  within a preregistered tolerance of 0.10.
- **Monotonicity**: power increases with `n_subjects` and effect size
  and decreases with random-effect SD, within Monte Carlo slack.
- **Reproducibility**: identical `seed` gives identical results,
  asserted exactly.

These checks validate the default configuration:
`equation = "simplified"` with independent per-condition random effects
(`rho_bd = 0`). Note that the refit is a *working model*: the simulator
draws multiplicative lognormal errors while the simplified equation fits
additive Gaussian errors on raw consumption (they share the mean
function; the error models differ). The approximation is closest at
small residual SD, and the null calibration is verified at both the
default `sigma_e = 0.1` and a stress value of 0.3. The between-subject
design’s random-effects structure is a step *less* approximate: with
each subject in a single arm, the per-subject intercept refit matches
the composed data-generating process exactly (only the error model
remains a working approximation). Other equations remain available as
sensitivity analyses, but their condition contrasts are defined on
different parameterizations and are not validated against the simulated
delta; `rho_bd` is locked at 0 in this version.

The `find_n_*` search statuses (`"confirmed"`, `"uncertain"`,
`"unresolved"`) describe a heuristic Monte Carlo decision rule (repeated
looks at ordinary Wilson intervals across candidate N), rather than a
formal sequential testing procedure with a guaranteed error rate. When
the confirmation pass contradicts the search, the function returns
`n = NA` rather than an unsupported number.

**Explicitly out of scope in v1** (flagged as future work rather than
silently approximated):

- Simultaneous multi-parameter effects (e.g., a joint Q0 + alpha shift).
- Power for derived measures (Pmax, Omax, elasticity at a point,
  breakpoint).
- Arbitrary user-supplied designs beyond the two the simulator supports:
  the within-subject design (two conditions, all subjects at all prices
  in both) and the two-arm between-subject design
  (`design_type = "between"`). Correlated random effects (`rho_bd != 0`)
  and user-specified unequal allocation ratios remain future work (an
  odd total N already splits into arms differing by one subject).
- Any graphical or interactive interface.

**Interpreting the estimate.** A reported power estimate is (a)
conditional on the assumed population parameters and variance components
(vary them and look at the sensitivity of the answer); (b) conditional
on usable fits (take the `n_used` warning seriously if it fires); and
(c) a Monte Carlo estimate (cite it with its interval at an `n_sim`
sized for the decision it supports).
