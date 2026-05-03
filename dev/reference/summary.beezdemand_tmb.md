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
  `"internal"`, `"natural"`, `"log10"`.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `summary.beezdemand_tmb` (also inherits from
`beezdemand_summary`).

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
#>     Q0:(Intercept)   6.5120    0.8097    8.0425 8.80e-16
#>  alpha:(Intercept)   0.0030    0.0017    1.7860 0.074103
#>              log_k   0.8955    0.4838    1.8509 0.064184
#>           logsigma  -0.9528    0.2292   -4.1564 3.23e-05
#>           logsigma  -0.7798    0.2302   -3.3879 0.000704
#>         logsigma_e  -1.9498    0.0631  -30.9183  < 2e-16
#>            rho_raw  -0.4675    0.3292   -1.4202 0.155547
#> 
#> --- Variance Components ---
#>              Component Estimate
#>     sigma_b (Q0 RE SD)   0.3857
#>  sigma_c (alpha RE SD)   0.4585
#>  sigma_e (Residual SD)   0.1423
#> 
#> --- RE Correlations ---
#>                      Component Estimate
#>  rho_bc (Q0-alpha correlation)  -0.4362
#> 
#> --- Fit Statistics ---
#> Log-likelihood: 40.65 
#> AIC: -67.3 
#> BIC: -46.41 
#> 
#> --- Population Demand Metrics ---
#> Pmax: 11.2377  Omax: 23.8941  Method: analytic_lambert_w
#> 
#> --- Individual Parameter Summaries ---
#>   Q0: Min=2.8370  Med=6.2483  Mean=6.9793  Max=10.2274
#>   alpha: Min=0.0016  Med=0.0034  Mean=0.0034  Max=0.0062
#>   Pmax: Min=5.7902  Med=11.6341  Mean=12.1608  Max=21.1473
#>   Omax: Min=11.7465  Med=21.2007  Mean=26.1542  Max=44.1357
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
#>     Q0:(Intercept)   0.8137    0.0540   15.0689  < 2e-16
#>  alpha:(Intercept)  -2.5194    0.2432  -10.3606  < 2e-16
#>              log_k   0.8955    0.4838    1.8509 0.064184
#>           logsigma  -0.9528    0.2292   -4.1564 3.23e-05
#>           logsigma  -0.7798    0.2302   -3.3879 0.000704
#>         logsigma_e  -1.9498    0.0631  -30.9183  < 2e-16
#>            rho_raw  -0.4675    0.3292   -1.4202 0.155547
#> 
#> --- Variance Components ---
#>              Component Estimate
#>     sigma_b (Q0 RE SD)   0.3857
#>  sigma_c (alpha RE SD)   0.4585
#>  sigma_e (Residual SD)   0.1423
#> 
#> --- RE Correlations ---
#>                      Component Estimate
#>  rho_bc (Q0-alpha correlation)  -0.4362
#> 
#> --- Fit Statistics ---
#> Log-likelihood: 40.65 
#> AIC: -67.3 
#> BIC: -46.41 
#> 
#> --- Population Demand Metrics ---
#> Pmax: 11.2377  Omax: 23.8941  Method: analytic_lambert_w
#> 
#> --- Individual Parameter Summaries ---
#>   Q0: Min=2.8370  Med=6.2483  Mean=6.9793  Max=10.2274
#>   alpha: Min=0.0016  Med=0.0034  Mean=0.0034  Max=0.0062
#>   Pmax: Min=5.7902  Med=11.6341  Mean=12.1608  Max=21.1473
#>   Omax: Min=11.7465  Med=21.2007  Mean=26.1542  Max=44.1357
#> 
#> Notes:
#>   * 14 zero-consumption observations dropped for equation='exponential'. 
# }
```
