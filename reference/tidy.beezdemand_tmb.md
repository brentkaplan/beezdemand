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
  `estimate`/`std.error` follow this scale; `statistic`/`p.value` are
  always on the estimation scale (transformation-invariant).

- ...:

  Additional arguments.

## Value

A tibble of model terms with columns `term`, `estimate`, `std.error`,
`statistic`, `p.value`, `df` (`Inf` on fixed-effect rows: the Wald test
is an asymptotic z, i.e. a t on infinite df; `NA` on variance rows),
`component`, `estimate_scale`, and `term_display`. An
`estimate_internal` column (the pre-transform estimate) is additionally
present whenever `effects` includes `"fixed"`. Fixed-effect rows carry
`component == "fixed"` (matching
[`tidy.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_nlme.md)
and the nlme/lme4 convention); variance-component rows carry
`component == "variance"`. A `hessian_warning` attribute (character
scalar, or absent) is attached depending on `x$hessian_pd`: absent (no
attribute) when `hessian_pd` is `TRUE` or `NULL` (the field is missing
on a legacy fit object, so there is nothing to report); a message noting
the Hessian is not positive definite when `hessian_pd` is `FALSE`; a
message noting positive-definiteness is unknown (because
[`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html) failed
entirely, so SEs/CIs are unavailable, not merely unreliable) when
`hessian_pd` is `NA`. This attribute is not printed by an ordinary
tibble print (see
[`summary.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_tmb.md)
or
[`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
for the surfaced versions of the same diagnostic).

## Details

Variance-component rows (`effects = "ran_pars"`) are exactly the rows of
`summary(x)$variance_components`: the Q0 and alpha random-effect
standard deviations on the **log10 scale** and the residual standard
deviation on the model's likelihood scale. They are not the raw internal
`logsigma` optimizer coefficients and do not respond to `report_space`;
`std.error` is `NA` for them. Random-effect *correlations* are not
tidied here (see `summary(x)$correlations` or `VarCorr(x)` for those).
The NLME sibling
[`tidy.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_nlme.md)
likewise reports SDs, so backend-agnostic code can consume the
`estimate` column without dispatch logic on either side.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
tidy(fit)
#> # A tibble: 5 × 10
#>   term    estimate std.error statistic    p.value    df component estimate_scale
#>   <chr>      <dbl>     <dbl>     <dbl>      <dbl> <dbl> <chr>     <chr>         
#> 1 Q0:(In…  6.55     0.812         15.2  5.68e- 52   Inf fixed     natural       
#> 2 alpha:…  0.00380  0.000562     -37.7  2.86e-311   Inf fixed     natural       
#> 3 sigma_…  0.168   NA             NA   NA            NA variance  log10         
#> 4 sigma_…  0.200   NA             NA   NA            NA variance  log10         
#> 5 sigma_…  0.143   NA             NA   NA            NA variance  natural       
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
tidy(fit, effects = "fixed", report_space = "log10")
#> # A tibble: 2 × 10
#>   term     estimate std.error statistic   p.value    df component estimate_scale
#>   <chr>       <dbl>     <dbl>     <dbl>     <dbl> <dbl> <chr>     <chr>         
#> 1 Q0:(Int…    0.816    0.0538      15.2 5.68e- 52   Inf fixed     log10         
#> 2 alpha:(…   -2.42     0.0642     -37.7 2.86e-311   Inf fixed     log10         
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
tidy(fit, effects = "ran_pars")
#> # A tibble: 3 × 9
#>   term       estimate std.error statistic p.value    df component estimate_scale
#>   <chr>         <dbl>     <dbl>     <dbl>   <dbl> <dbl> <chr>     <chr>         
#> 1 sigma_b (…    0.168        NA        NA      NA    NA variance  log10         
#> 2 sigma_c (…    0.200        NA        NA      NA    NA variance  log10         
#> 3 sigma_e (…    0.143        NA        NA      NA    NA variance  natural       
#> # ℹ 1 more variable: term_display <chr>
# }
```
