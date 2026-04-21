# Get Demand Parameter Comparisons for TMB Model

Computes pairwise contrasts between factor levels for demand parameters
from a `beezdemand_tmb` model.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
get_demand_comparisons(
  fit_obj,
  param = c("Q0", "alpha"),
  contrast_type = c("pairwise", "trt.vs.ctrl"),
  p_adjust = "holm",
  ci_level = 0.95,
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_tmb` object.

- param:

  Character. Which parameter: `"Q0"` or `"alpha"`.

- contrast_type:

  Character. Type of contrast: `"pairwise"` or `"trt.vs.ctrl"`.

- p_adjust:

  Character. P-value adjustment method (default `"holm"`).

- ci_level:

  Numeric. Confidence level.

- ...:

  Additional arguments.

## Value

A tibble with contrast results.

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
get_demand_comparisons(fit, param = "Q0")
#> # A tibble: 1 × 9
#>   contrast   estimate_log estimate_ratio std.error statistic p.value.raw p.value
#>   <chr>             <dbl>          <dbl>     <dbl>     <dbl>       <dbl>   <dbl>
#> 1 gender=Ma…        0.241           1.27    0.0409      5.89     3.84e-9 3.84e-9
#> # ℹ 2 more variables: conf.low <dbl>, conf.high <dbl>
# }
```
