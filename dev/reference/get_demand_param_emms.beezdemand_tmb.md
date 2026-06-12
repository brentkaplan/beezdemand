# Get Demand Parameter Estimated Marginal Means for TMB Model

Computes estimated marginal means (EMMs) for demand parameters from a
`beezdemand_tmb` model. Uses design matrices and beta vectors with vcov
from [`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html).

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
get_demand_param_emms(
  fit_obj,
  param = c("Q0", "alpha"),
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_tmb` object.

- param:

  Character. Which parameter to compute EMMs for: `"Q0"` or `"alpha"`.

- factors_in_emm:

  Character vector of factors to retain in the EMM reference grid. If it
  names a strict subset of the fitted factors, the omitted factors are
  **marginalized over** using equal weights across the full crossing of
  their levels (emmeans' default `weights = "equal"`), matching the NLME
  backend. If `NULL` (default), all fitted factors are retained (no
  marginalization). Under asymmetric `collapse_levels` you may name
  either the original factor or its collapsed per-parameter column; a
  name that resolves to neither for this parameter is rejected with an
  error.

- at:

  Named list specifying factor levels and continuous-covariate values
  for conditional EMMs. For continuous covariates, a single numeric
  value per covariate; multiple values produce a warning and only the
  first is used. `at` on a marginalized (omitted) factor restricts the
  level set averaged over.

- ci_level:

  Numeric. Confidence level for intervals.

- ...:

  Additional arguments.

## Value

A tibble with columns: level, estimate, std.error, conf.low, conf.high.

## Note

Marginalization is exact because `Q0`/`alpha` are linear in the
fixed-effect coefficients on the log scale, so averaging the
reference-grid design rows and then multiplying by the coefficient
vector equals averaging the per-cell parameter predictions.

## Examples

``` r
# \donttest{
data(apt_full)
dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
fit <- fit_demand_tmb(dat, equation = "exponential",
                      factors = "gender", verbose = 0)
#>   equation='exponential': Dropped 5839 zero-consumption observations (12827 remaining).
#> Warning: ! Some standard errors are unavailable (non-positive variance estimates from
#>   `TMB::sdreport()`).
#> ℹ This usually reflects a weakly identified fit; check `$hessian_pd` and
#>   `summary()` diagnostics.
get_demand_param_emms(fit, param = "Q0")
#> # A tibble: 2 × 6
#>   level         estimate estimate_log std.error conf.low conf.high
#>   <chr>            <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Female     5.03         1.62    0.0270     4.77      5.30
#> 2 gender=Male       6.40         1.86    0.0310     6.02      6.80
get_demand_param_emms(fit, param = "alpha")
#> Warning: NaNs produced
#> Warning: NaNs produced
#> # A tibble: 2 × 6
#>   level         estimate estimate_log std.error conf.low conf.high
#>   <chr>            <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Female  0.00320        -5.74       NaN      NaN       NaN
#> 2 gender=Male    0.00316        -5.76       NaN      NaN       NaN
# }
```
