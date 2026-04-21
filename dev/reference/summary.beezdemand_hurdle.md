# Summarize a Hurdle Demand Model Fit

Provides a summary of a fitted hurdle demand model, including fixed
effects, variance components, correlations, and fit statistics.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
summary(object, report_space = c("natural", "log10", "internal"), ...)
```

## Arguments

- object:

  An object of class `beezdemand_hurdle` from
  [`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md).

- report_space:

  Character. Reporting space for core demand parameters. One of:

  - `"internal"`: report internal/fitting parameters (default internal
    naming)

  - `"natural"`: report natural-scale parameters when a natural mapping
    exists

  - `"log10"`: report [`log10()`](https://rdrr.io/r/base/Log.html)-scale
    parameters when a mapping exists

- ...:

  Additional arguments (currently unused).

## Value

An object of class `summary.beezdemand_hurdle` (also inherits from
`beezdemand_summary`) containing:

- call:

  The original function call

- model_class:

  "beezdemand_hurdle"

- backend:

  "TMB"

- coefficients:

  Tibble of fixed effects with estimates, SEs, z-values, p-values

- coefficients_matrix:

  Matrix form for printing (legacy compatibility)

- variance_components:

  Matrix of variance/covariance estimates

- correlations:

  Matrix of correlation estimates

- n_subjects:

  Number of subjects

- nobs:

  Number of observations

- converged:

  Logical indicating convergence

- logLik:

  Log-likelihood at convergence

- AIC:

  Akaike Information Criterion

- BIC:

  Bayesian Information Criterion

- group_metrics:

  Group-level Pmax and Omax

- individual_metrics:

  Summary of individual-level parameters

- notes:

  Character vector of warnings/notes

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
summary(fit)
#> 
#> Two-Part Mixed Effects Hurdle Demand Model
#> ============================================
#> 
#> Call:
#> fit_demand_hurdle(data = apt, y_var = "y", x_var = "x", id_var = "id")
#> 
#> Convergence: Yes 
#> Number of subjects: 10 
#> Number of observations: 160 
#> Random effects: 3 (zeros, q0, alpha) 
#> 
#> Fixed Effects:
#> --------------
#>              Estimate Std. Error z value
#> beta0      -293.94893  160.41398  -1.832
#> beta1       104.07743   61.07262   1.704
#> log_q0        1.87220    0.12435  15.056
#> log_k         1.83359    0.56794   3.228
#> log_alpha    -4.04181    0.65639  -6.158
#> logsigma_a    4.88805    1.34747   3.628
#> logsigma_b   -0.95269    0.22912  -4.158
#> logsigma_c   -0.79237    0.23839  -3.324
#> logsigma_e   -1.95094    0.06294 -30.998
#> rho_ab_raw    0.18315    0.22247   0.823
#> rho_ac_raw    0.23874    0.27580   0.866
#> rho_bc_raw    0.40454    0.32560   1.242
#> 
#> Variance Components:
#> --------------------
#>          Estimate Std. Error
#> alpha      0.0176     0.0115
#> k          6.2563     3.5532
#> var_a  17607.7199 47451.7741
#> var_b      0.1488     0.0682
#> var_c      0.2050     0.0977
#> cov_ab     9.2703     7.6244
#> cov_ac    14.0774    11.5420
#> cov_bc     0.0715     0.0627
#> var_e      0.0202     0.0025
#> 
#> Correlations:
#> -------------
#>        Estimate Std. Error
#> rho_ab   0.1811     0.2152
#> rho_ac   0.2343     0.2607
#> rho_bc   0.4094     0.2735
#> 
#> Model Fit:
#> ----------
#>   Log-likelihood: 32.81
#>   AIC: -41.63
#>   BIC: -4.73
#> 
#> Demand Metrics (Group-Level):
#> -----------------------------
#>   Pmax (price at max expenditure): 11.0485
#>   Omax (max expenditure): 23.8281
#>   Q at Pmax: 2.1567
#>   Elasticity at Pmax: -1.0000
#>   Method: analytic_lambert_w_hurdle
#> 
#> Derived Parameters (Individual-Level Summary):
#> ----------------------------------------------
#>   Q0 (Intensity):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.828   5.758   6.236   6.962   9.247  10.209 
#>   Alpha:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.00934 0.01240 0.01733 0.01948 0.02624 0.03388 
#>   Breakpoint:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   7.593  12.357  15.064  16.995  22.276  27.643 
#>   Pmax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   5.729   7.404  11.483  11.986  15.742  20.780 
#>   Omax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   11.78   16.42   21.14   26.14   36.37   44.19 
#> 
# }
```
