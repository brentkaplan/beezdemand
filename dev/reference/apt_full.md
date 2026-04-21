# Full alcohol purchase task dataset

A larger dataset containing alcohol purchase task data with demographic
covariates. Suitable for testing hurdle models and mixed-effects models
with covariates.

## Usage

``` r
apt_full
```

## Format

A data frame with 18,700 rows and 8 columns:

- id:

  Unique participant identifier (1-1100)

- gender:

  Participant gender (Male/Female)

- age:

  Participant age in years

- binges:

  Number of binge drinking episodes

- totdrinks:

  Total number of drinks consumed

- tothours:

  Total hours spent drinking

- x:

  Price point for the purchase task

- y:

  Number of drinks participant would purchase at price x

## Examples

``` r
# \donttest{
data(apt_full)
# Use a subset for quick demonstration
apt_sub <- apt_full[apt_full$id %in% unique(apt_full$id)[1:20], ]
fit <- fit_demand_hurdle(apt_sub, y_var = "y", x_var = "x", id_var = "id")
#> Sample size may be too small for reliable estimation.
#>   Subjects: 20, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 20, Observations: 340
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   WARNING: Did not converge (code 1: false convergence (8))
#>   Computing standard errors...
#> Warning: NaNs produced
#> Warning: ! Hessian is not positive definite (`pdHess = FALSE`).
#> ℹ Standard errors, p-values, and confidence intervals may be unreliable.
#> ℹ Run `check_demand_model()` for detailed diagnostics.
#> ℹ Consider simplifying the model (fewer random effects) or checking data
#>   quality.
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Done. Log-likelihood: -86.69
summary(fit)
#> 
#> Two-Part Mixed Effects Hurdle Demand Model
#> ============================================
#> 
#> Call:
#> fit_demand_hurdle(data = apt_sub, y_var = "y", x_var = "x", id_var = "id")
#> 
#> Convergence: No 
#> Number of subjects: 20 
#> Number of observations: 340 
#> Random effects: 3 (zeros, q0, alpha) 
#> 
#> Fixed Effects:
#> --------------
#>              Estimate Std. Error z value
#> beta0      -1.047e+02        NaN      NA
#> beta1       1.007e+02        NaN      NA
#> log_q0      1.989e+00  8.579e-02   23.18
#> log_k       3.002e+01  5.786e-02  518.86
#> log_alpha  -3.155e+01  9.347e-02 -337.50
#> logsigma_a  6.260e+00        NaN      NA
#> logsigma_b -7.181e-01  8.392e-03  -85.56
#> logsigma_c -9.699e-01  6.061e-02  -16.00
#> logsigma_e -1.524e+00  1.571e-02  -96.96
#> rho_ab_raw  4.377e-04        NaN      NA
#> rho_ac_raw  1.731e-01        NaN      NA
#> rho_bc_raw  3.465e-01        NaN      NA
#> 
#> Variance Components:
#> --------------------
#>            Estimate   Std. Error
#> alpha  0.000000e+00 0.000000e+00
#> k      1.093285e+13 6.326052e+11
#> var_a  2.737727e+05          NaN
#> var_b  2.379000e-01 4.000000e-03
#> var_c  1.437000e-01 1.740000e-02
#> cov_ab 1.117000e-01          NaN
#> cov_ac 3.400540e+01          NaN
#> cov_bc 6.070000e-02          NaN
#> var_e  4.750000e-02 1.500000e-03
#> 
#> Correlations:
#> -------------
#>        Estimate Std. Error
#> rho_ab   0.0004        NaN
#> rho_ac   0.1714        NaN
#> rho_bc   0.3284        NaN
#> 
#> Model Fit:
#> ----------
#>   Log-likelihood: -86.69
#>   AIC: 197.37
#>   BIC: 243.32
#> 
#> Demand Metrics (Group-Level):
#> -----------------------------
#>   Pmax (price at max expenditure): 4.5898
#>   Omax (max expenditure): 12.3377
#>   Q at Pmax: 2.6881
#>   Elasticity at Pmax: 0.0000
#>   Method: analytic_lambert_w_hurdle
#> 
#> Derived Parameters (Individual-Level Summary):
#> ----------------------------------------------
#>   Q0 (Intensity):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.394   6.117   7.483   8.042  10.806  12.295 
#>   Alpha:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 1.068e-14 1.523e-14 1.784e-14 2.001e-14 2.237e-14 4.266e-14 
#>   Breakpoint:
#>       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> -0.0000563  5.4797580  8.9804575  9.6755820 11.4621669 28.6259695 
#>   Pmax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.144   4.090   5.128   5.172   6.007   8.564 
#>   Omax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   5.815   9.730  12.179  15.004  19.360  36.014 
#> 
# }
```
