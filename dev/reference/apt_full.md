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
#> Warning: NaNs produced
#> Done. Log-likelihood: -81.74
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
#>              Estimate Std. Error   t value
#> beta0      -1.068e+02        NaN       NaN
#> beta1       1.038e+02  4.916e-01   211.100
#> log_q0      1.998e+00        NaN       NaN
#> log_k       3.090e+01        NaN       NaN
#> log_alpha  -3.245e+01  9.069e-03 -3577.696
#> logsigma_a  6.384e+00  1.625e-02   392.831
#> logsigma_b -6.577e-01  1.761e-02   -37.348
#> logsigma_c -9.817e-01        NaN       NaN
#> logsigma_e -1.532e+00  4.854e-02   -31.564
#> rho_ab_raw -1.980e-02  2.425e-03    -8.165
#> rho_ac_raw  1.669e-01  5.353e-03    31.176
#> rho_bc_raw  3.480e-01        NaN       NaN
#> 
#> Variance Components:
#> --------------------
#>             Estimate Std. Error
#> alpha   0.000000e+00     0.0000
#> k       2.630596e+13        NaN
#> var_a   3.505057e+05 11391.5330
#> var_b   2.684000e-01     0.0095
#> var_c   1.404000e-01        NaN
#> cov_ab -6.071100e+00     0.4469
#> cov_ac  3.667870e+01     2.5187
#> cov_bc  6.340000e-02        NaN
#> var_e   4.670000e-02     0.0045
#> 
#> Correlations:
#> -------------
#>        Estimate Std. Error
#> rho_ab  -0.0198     0.0024
#> rho_ac   0.1653     0.0052
#> rho_bc   0.3266        NaN
#> 
#> Model Fit:
#> ----------
#>   Log-likelihood: -81.74
#>   AIC: 187.48
#>   BIC: 233.43
#> 
#> Demand Metrics (Group-Level):
#> -----------------------------
#>   Pmax (price at max expenditure): 4.6947
#>   Omax (max expenditure): 12.7530
#>   Q at Pmax: 2.7165
#>   Elasticity at Pmax: 0.0000
#>   Method: analytic_lambert_w_hurdle
#> 
#> Derived Parameters (Individual-Level Summary):
#> ----------------------------------------------
#>   Q0 (Intensity):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.389   6.111   7.477   8.043  10.825  12.320 
#>   Alpha:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 4.420e-15 6.228e-15 7.358e-15 8.266e-15 9.276e-15 1.765e-14 
#>   Breakpoint:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -0.000065  5.567882  8.984549  9.874247 10.916976 32.413180 
#>   Pmax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.154   4.098   5.167   5.199   6.104   8.600 
#>   Omax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   5.890   9.771  12.353  15.091  19.566  36.106 
#> 
# }
```
