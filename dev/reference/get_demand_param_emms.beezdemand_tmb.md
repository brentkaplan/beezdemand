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

  Character vector of factors to marginalize over. If NULL, uses all
  factors in the model.

- at:

  Named list specifying factor levels for conditional EMMs.

- ci_level:

  Numeric. Confidence level for intervals.

- ...:

  Additional arguments.

## Value

A tibble with columns: level, estimate, std.error, conf.low, conf.high.

## Examples

``` r
# \donttest{
data(apt_full)
dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
fit <- fit_demand_tmb(dat, equation = "exponential",
                      factors = "gender", verbose = 0)
#>   equation='exponential': Dropped 5839 zero-consumption observations (12827 remaining).
#> Warning: NaNs produced
#> Warning: NaNs produced
get_demand_param_emms(fit, param = "Q0")
#> # A tibble: 2 × 6
#>   level         estimate estimate_log std.error conf.low conf.high
#>   <chr>            <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Male       6.40         1.86    0.0310     6.02      6.80
#> 2 gender=Female     5.03         1.62    0.0270     4.77      5.30
get_demand_param_emms(fit, param = "alpha")
#> Warning: NaNs produced
#> Warning: NaNs produced
#> # A tibble: 2 × 6
#>   level         estimate estimate_log std.error conf.low conf.high
#>   <chr>            <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Male    0.00316        -5.76       NaN      NaN       NaN
#> 2 gender=Female  0.00320        -5.74       NaN      NaN       NaN
# }
```
