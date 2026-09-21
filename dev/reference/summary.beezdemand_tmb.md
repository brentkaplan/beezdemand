# Summarize a TMB Mixed-Effects Demand Model Fit

Summarize a TMB Mixed-Effects Demand Model Fit

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
summary(object, report_space = c("natural", "log10", "internal"), ...)
```

## Arguments

- object:

  An object of class `beezdemand_tmb`.

- report_space:

  Character. Reporting space for core demand parameters. One of
  `"internal"`, `"natural"`, `"log10"`. `estimate`/`std.error` are
  reported on this scale; `statistic`/`p.value` are always computed on
  the estimation scale (the Wald test is defined there and is not
  recomputed after back-transforming, so on the natural scale
  `statistic != estimate/std.error`, by design).

- ...:

  Additional arguments (currently unused).

## Value

An object of class `summary.beezdemand_tmb` (also inherits from
`beezdemand_summary`). The `variance_components` element reports the Q0
and alpha random-effect SDs on the **log10 scale**: the random effects
are estimated on the natural-log scale internally and divided by
`log(10)` for reporting, so they are directly comparable with
[`nlme::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html) on a
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
fit using the default `param_space = "log10"`. The residual SD is
reported on the model's likelihood scale (equation-dependent) and the
random-effect correlations are scale-invariant; neither is rescaled.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
summary(fit)
#> 
#> TMB Mixed-Effects Demand Model Summary
#> ================================================== 
#> 
#> Equation: exponential 
#> Backend: TMB_mixed 
#> Convergence: Yes 
#> Subjects: 10  Observations: 146 
#> 
#> --- Fixed Effects ---
#>               term estimate std.error statistic  p.value
#>     Q0:(Intercept)   6.5533    0.8122   15.1690  < 2e-16
#>  alpha:(Intercept)   0.0038    0.0006  -37.7147  < 2e-16
#>           logsigma  -0.9506    0.2294   -4.1441 3.41e-05
#>           logsigma  -0.7772    0.2304   -3.3733 0.000743
#>         logsigma_e  -1.9469    0.0629  -30.9396  < 2e-16
#>            rho_raw  -0.4593    0.3292   -1.3951 0.162994
#> 
#> --- Variance Components ---
#> (Q0/alpha RE SDs on log10 scale; residual SD on likelihood scale)
#>              Component Estimate
#>     sigma_b (Q0 RE SD)   0.1679
#>  sigma_c (alpha RE SD)   0.1996
#>  sigma_e (Residual SD)   0.1427
#> 
#> --- RE Correlations ---
#>                      Component Estimate
#>  rho_bc (Q0-alpha correlation)  -0.4295
#> 
#> --- Fit Statistics ---
#> Log-likelihood: 40.53 
#> AIC: -69.07 
#> BIC: -51.17 
#> 
#> --- Population Demand Metrics ---
#> Pmax: 11.6482  Omax: 23.9232  Method: analytic_lambert_w
#> 
#> --- Individual Parameter Summaries ---
#>   Q0: Min=2.8583  Med=6.2762  Mean=7.0238  Max=10.2728
#>   alpha: Min=0.0021  Med=0.0043  Mean=0.0042  Max=0.0078
#>   Pmax: Min=5.9675  Med=12.0725  Mean=12.6217  Max=22.1596
#>   Omax: Min=11.6871  Med=21.2953  Mean=26.1968  Max=44.1029
#> 
#> Notes:
#>   * 14 zero-consumption observations dropped for equation='exponential'. 
summary(fit, report_space = "log10")
#> 
#> TMB Mixed-Effects Demand Model Summary
#> ================================================== 
#> 
#> Equation: exponential 
#> Backend: TMB_mixed 
#> Convergence: Yes 
#> Subjects: 10  Observations: 146 
#> 
#> --- Fixed Effects ---
#>               term estimate std.error statistic  p.value
#>     Q0:(Intercept)   0.8165    0.0538   15.1690  < 2e-16
#>  alpha:(Intercept)  -2.4199    0.0642  -37.7147  < 2e-16
#>           logsigma  -0.9506    0.2294   -4.1441 3.41e-05
#>           logsigma  -0.7772    0.2304   -3.3733 0.000743
#>         logsigma_e  -1.9469    0.0629  -30.9396  < 2e-16
#>            rho_raw  -0.4593    0.3292   -1.3951 0.162994
#> 
#> --- Variance Components ---
#> (Q0/alpha RE SDs on log10 scale; residual SD on likelihood scale)
#>              Component Estimate
#>     sigma_b (Q0 RE SD)   0.1679
#>  sigma_c (alpha RE SD)   0.1996
#>  sigma_e (Residual SD)   0.1427
#> 
#> --- RE Correlations ---
#>                      Component Estimate
#>  rho_bc (Q0-alpha correlation)  -0.4295
#> 
#> --- Fit Statistics ---
#> Log-likelihood: 40.53 
#> AIC: -69.07 
#> BIC: -51.17 
#> 
#> --- Population Demand Metrics ---
#> Pmax: 11.6482  Omax: 23.9232  Method: analytic_lambert_w
#> 
#> --- Individual Parameter Summaries ---
#>   Q0: Min=2.8583  Med=6.2762  Mean=7.0238  Max=10.2728
#>   alpha: Min=0.0021  Med=0.0043  Mean=0.0042  Max=0.0078
#>   Pmax: Min=5.9675  Med=12.0725  Mean=12.6217  Max=22.1596
#>   Omax: Min=11.6871  Med=21.2953  Mean=26.1968  Max=44.1029
#> 
#> Notes:
#>   * 14 zero-consumption observations dropped for equation='exponential'. 
# }
```
