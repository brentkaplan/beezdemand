# Confidence Intervals for Mixed-Effects Demand Model Parameters

Computes confidence intervals for fixed effect parameters from an
NLME-based mixed-effects demand model.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
confint(object, parm = NULL, level = 0.95, method = c("wald", "profile"), ...)
```

## Arguments

- object:

  A `beezdemand_nlme` object from
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md).

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all fixed effect parameters.

- level:

  Confidence level (default 0.95).

- method:

  Character. Method for computing intervals:

  - `"wald"`: Wald-type intervals using asymptotic normality (default,
    fast)

  - `"profile"`: Profile likelihood intervals via
    [`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html)
    (slower but more accurate for small samples)

- ...:

  Additional arguments passed to
  [`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html)
  when `method = "profile"`.

## Value

A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `component`.

## Details

For Wald intervals, confidence bounds are computed as estimate ± z \* SE
using standard errors from the model summary.

For profile intervals,
[`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html) is
called on the underlying nlme model object. This method provides more
accurate intervals but can be computationally intensive for complex
models.

## Examples

``` r
# \donttest{
data(ko)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
                        id_var = "monkey", equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 2 (Q0: 1, alpha: 1)
confint(fit)
#> # A tibble: 2 × 6
#>   term  estimate conf.low conf.high level component
#>   <chr>    <dbl>    <dbl>     <dbl> <dbl> <chr>    
#> 1 Q0        2.16     2.08      2.24  0.95 fixed    
#> 2 alpha    -4.59    -4.64     -4.53  0.95 fixed    
# }
```
