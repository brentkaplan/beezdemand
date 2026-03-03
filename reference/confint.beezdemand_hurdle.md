# Confidence Intervals for Hurdle Demand Model Parameters

Computes confidence intervals for fixed effect parameters from a
TMB-based hurdle demand model using the asymptotic normal approximation.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
confint(
  object,
  parm = NULL,
  level = 0.95,
  report_space = c("internal", "natural"),
  ...
)
```

## Arguments

- object:

  A `beezdemand_hurdle` object from
  [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md).

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all fixed effect parameters.

- level:

  Confidence level (default 0.95).

- report_space:

  Character. Reporting space for parameters:

  - `"internal"`: parameters on internal/fitting scale (log for Q0,
    alpha)

  - `"natural"`: back-transformed to natural scale

- ...:

  Additional arguments (ignored).

## Value

A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `component`, `estimate_scale`.

## Details

Confidence intervals are computed using the asymptotic normal
approximation based on standard errors from
[`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html). For
parameters estimated on the log scale (Q0, alpha, k), intervals can be
back-transformed to the natural scale using `report_space = "natural"`.

The transformation uses:

- For log-scale parameters: exp(estimate +/- z \* SE)

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   Converged in 81 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 32.81
confint(fit)
#> # A tibble: 12 × 7
#>    term       estimate conf.low conf.high level component        estimate_scale
#>    <chr>         <dbl>    <dbl>     <dbl> <dbl> <chr>            <chr>         
#>  1 beta0      -294.    -608.       20.5    0.95 zero_probability logit         
#>  2 beta1       104.     -15.6     224.     0.95 zero_probability logit         
#>  3 log(Q0)       1.87     1.63      2.12   0.95 consumption      log           
#>  4 log(k)        1.83     0.720     2.95   0.95 consumption      log           
#>  5 log(alpha)   -4.04    -5.33     -2.76   0.95 consumption      log           
#>  6 logsigma_a    4.89     2.25      7.53   0.95 variance         natural       
#>  7 logsigma_b   -0.953   -1.40     -0.504  0.95 variance         natural       
#>  8 logsigma_c   -0.792   -1.26     -0.325  0.95 variance         natural       
#>  9 logsigma_e   -1.95    -2.07     -1.83   0.95 variance         natural       
#> 10 rho_ab_raw    0.183   -0.253     0.619  0.95 variance         natural       
#> 11 rho_ac_raw    0.239   -0.302     0.779  0.95 variance         natural       
#> 12 rho_bc_raw    0.405   -0.234     1.04   0.95 variance         natural       
# }
```
