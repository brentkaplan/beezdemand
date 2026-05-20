# Tidy a beezdemand_tmb Model

Tidy a beezdemand_tmb Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
tidy(
  x,
  effects = c("fixed", "ran_pars"),
  report_space = c("natural", "log10", "internal"),
  ...
)
```

## Arguments

- x:

  A `beezdemand_tmb` object.

- effects:

  Character. Which effects to return: `"fixed"` for the fixed-effect
  (core demand parameter) rows, `"ran_pars"` for the random-effect
  variance components, or both (the default). Matches the `effects`
  argument of
  [`tidy.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_nlme.md).

- report_space:

  Character. Reporting space for the fixed-effect (core demand
  parameter) rows. One of `"natural"`, `"log10"`, or `"internal"`.
  Variance-component rows are unaffected (see Details).

- ...:

  Additional arguments.

## Value

A tibble of model terms with columns `term`, `estimate`, `std.error`,
`statistic`, `p.value`, `component`, `estimate_scale`, and
`term_display`. An `estimate_internal` column (the pre-transform
estimate) is additionally present whenever `effects` includes `"fixed"`.
Fixed-effect rows carry `component == "fixed"` (matching
[`tidy.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_nlme.md)
and the nlme/lme4 convention); variance-component rows carry
`component == "variance"`.

## Details

Variance-component rows (`effects = "ran_pars"`) are exactly the rows of
`summary(x)$variance_components`: the Q0 and alpha random-effect
standard deviations on the **log10 scale** and the residual standard
deviation on the model's likelihood scale. They are not the raw internal
`logsigma` optimizer coefficients and do not respond to `report_space`;
`std.error` is `NA` for them. Random-effect *correlations* are not
tidied here – see `summary(x)$correlations` or `VarCorr(x)` for those.

One cross-backend difference is not yet harmonized:
`tidy.beezdemand_tmb(effects = "ran_pars")` reports random-effect
*standard deviations*, whereas
[`tidy.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_nlme.md)
reports *variances*. Code consuming `estimate` from `"ran_pars"` rows
must account for this.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
tidy(fit)
#> # A tibble: 6 × 9
#>   term           estimate std.error statistic   p.value component estimate_scale
#>   <chr>             <dbl>     <dbl>     <dbl>     <dbl> <chr>     <chr>         
#> 1 Q0:(Intercept)  6.51      0.810        8.04  8.80e-16 fixed     natural       
#> 2 alpha:(Interc…  0.00302   0.00169      1.79  7.41e- 2 fixed     natural       
#> 3 log_k           0.895     0.484        1.85  6.42e- 2 fixed     log           
#> 4 sigma_b (Q0 R…  0.167    NA           NA    NA        variance  log10         
#> 5 sigma_c (alph…  0.199    NA           NA    NA        variance  log10         
#> 6 sigma_e (Resi…  0.142    NA           NA    NA        variance  natural       
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
tidy(fit, effects = "fixed", report_space = "log10")
#> # A tibble: 3 × 9
#>   term            estimate std.error statistic  p.value component estimate_scale
#>   <chr>              <dbl>     <dbl>     <dbl>    <dbl> <chr>     <chr>         
#> 1 Q0:(Intercept)     0.814    0.0540     15.1  2.59e-51 fixed     log10         
#> 2 alpha:(Interce…   -2.52     0.243     -10.4  3.75e-25 fixed     log10         
#> 3 log_k              0.895    0.484       1.85 6.42e- 2 fixed     log           
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
tidy(fit, effects = "ran_pars")
#> # A tibble: 3 × 8
#>   term             estimate std.error statistic p.value component estimate_scale
#>   <chr>               <dbl>     <dbl>     <dbl>   <dbl> <chr>     <chr>         
#> 1 sigma_b (Q0 RE …    0.167        NA        NA      NA variance  log10         
#> 2 sigma_c (alpha …    0.199        NA        NA      NA variance  log10         
#> 3 sigma_e (Residu…    0.142        NA        NA      NA variance  natural       
#> # ℹ 1 more variable: term_display <chr>
# }
```
