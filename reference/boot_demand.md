# Bootstrap Confidence Intervals for Derived Demand Metrics

Computes confidence intervals on derived demand metrics (Pmax, Omax,
Qmax, EV, elasticity-at-Pmax) for a TMB mixed-effects demand fit, via a
parametric bootstrap. Draws of the fixed-effect parameter vector are
taken from the joint asymptotic Gaussian posterior \\N(\hat\beta,
\hat\Sigma)\\, mapped to per-condition \\(Q_0, \alpha, k)\\ through the
model's fixed-effect design, passed through the canonical Pmax/Omax
engine, and summarized by empirical quantiles.

For a factor-expanded fit, one CI is returned per factor cell (e.g. one
row per `gender` level); for an intercept-only fit, one row per
statistic with `condition = NA`. The per-cell point estimate reproduces
[`calc_group_metrics`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md)`(fit, at = cell)`;
the bootstrap supplies only the interval.

## Usage

``` r
boot_demand(
  fit,
  statistics = c("Pmax", "Omax", "EV"),
  method = c("parametric"),
  R = 1000L,
  ci_level = 0.95,
  at = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- fit:

  A `beezdemand_tmb` object. NLME (`beezdemand_nlme`) and hurdle fits
  are not supported in this version and error helpfully.

- statistics:

  Character vector; any of
  `c("Pmax", "Omax", "Qmax", "EV", "elasticity_at_pmax")`. Default
  `c("Pmax", "Omax", "EV")`.

- method:

  Resampling scheme. Only `"parametric"` is available in this version
  (nonparametric subject resampling is planned).

- R:

  Integer number of bootstrap draws; minimum 100, default 1000.

- ci_level:

  Confidence level for the empirical-quantile interval (default 0.95).

- at:

  Optional named list of factor-level filters / continuous-covariate
  value overrides, with the same shape as the `at` argument of
  [`calc_group_metrics`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md).
  When `NULL` (default) all factor cells are enumerated; supplying `at`
  conditions to (or filters) the requested cell(s).

- seed:

  Optional integer seed for reproducible draws. The caller's RNG state
  is left unperturbed.

- ...:

  Reserved for future extension; must be empty. Unknown arguments (e.g.
  a misspelled `statistics`) raise an error rather than being silently
  ignored.

## Value

A tibble with one row per `(statistic, condition)`:

- statistic:

  Metric name.

- condition:

  Factor-cell label (e.g. `"gender=Male"`); `NA` when the fit has no
  factors.

- estimate:

  Point estimate (from the coefficient vector and point `k`).

- conf.low, conf.high:

  Empirical-quantile interval bounds.

- level:

  The confidence level used.

The bootstrap settings are attached as attributes `"method"`, `"R"`, and
`"seed"`, plus `"n_nonfinite"` (a per-row count of draws excluded as
non-finite).

## Details

The parametric bootstrap is asymptotically equivalent to the delta
method but avoids its linearization, so it is the more defensible
recourse for the strongly nonlinear derived metrics (Pmax/Omax via
Lambert-W). Draws are fixed-effect-only (population / per-condition
metrics); per-subject metric CIs would require random-effect-aware draws
and are out of scope for now.

When `k` is estimated, its uncertainty is propagated (the `log_k` column
is in the draw matrix); when `k` is fixed, the fixed value is used. The
point estimate always uses the point `k`, matching
[`calc_group_metrics()`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md).

Note that percentile intervals of a nonlinear transform are not
guaranteed to bracket the point estimate; `conf.low <= conf.high` always
holds, but `conf.low <= estimate <= conf.high` may not at boundary
cases.

Some draws can leave a metric's domain (e.g. Pmax is undefined when a
drawn `k` falls below the Lambert-W threshold). Such non-finite draws
are excluded from the quantiles; the per-row count of excluded draws is
recorded in `attr(x, "n_nonfinite")` (so the realized draw count is `R`
minus that). If *every* draw of a requested metric/condition is
non-finite, an error is raised because the interval is undefined.

## See also

[`calc_group_metrics`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md),
[`fit_demand_tmb`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md),
[`confint.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/confint.beezdemand_tmb.md)

## Examples

``` r
# \donttest{
data(apt, package = "beezdemand")
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
boot_demand(fit, statistics = c("Pmax", "Omax", "EV"), R = 500, seed = 1)
#> # A tibble: 3 × 6
#>   statistic condition estimate conf.low conf.high level
#>   <chr>     <chr>        <dbl>    <dbl>     <dbl> <dbl>
#> 1 Pmax      NA          11.2      8.50      17.1   0.95
#> 2 Omax      NA          23.9     18.1       32.6   0.95
#> 3 EV        NA           0.863    0.544      1.37  0.95
# }
```
