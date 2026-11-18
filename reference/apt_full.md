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
#> Warning: ! Some standard errors are unavailable (non-positive variance estimates from
#>   `TMB::sdreport()`).
#> ℹ This usually reflects a weakly identified fit; check `$hessian_pd` and
#>   `summary()` diagnostics.
#> Warning: ! Hessian is not positive definite (`pdHess = FALSE`).
#> ℹ Standard errors, p-values, and confidence intervals may be unreliable.
#> ℹ Run `check_demand_model()` for detailed diagnostics.
#> ℹ Consider simplifying the model (fewer random effects) or checking data
#>   quality.
#> Done. Log-likelihood: -80.84
summary(fit)
#> 
#> Two-Part Mixed Effects Hurdle Demand Model
#> ============================================
#> 
#> Call:
#> fit_demand_hurdle(data = apt_sub, y_var = "y", x_var = "x", id_var = "id")
#> 
#> Convergence: No 
#> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#> WARNING: this fit did not pass the convergence/Hessian gate.
#>   Warning: Optimizer did not converge (code 1: "false convergence (8)").
#>   Warning: Hessian not positive definite -- standard errors may be unreliable.
#>   Recommended stability check: refit with random_effects = c("zeros", "q0") (dropping the alpha random effect) and compare empirical-Bayes subject parameters.
#> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#> Number of subjects: 20 
#> Number of observations: 340 
#> Random effects: 3 (zeros, q0, alpha) 
#> 
#> Fixed Effects:
#> --------------
#>              Estimate Std. Error   z value
#> beta0      -1.156e+02  3.549e-01  -325.626
#> beta1       1.089e+02        NaN        NA
#> log_q0      1.982e+00  1.581e-01    12.540
#> log_k       3.284e+01  6.526e-02   503.159
#> log_alpha  -3.445e+01  1.702e-02 -2023.748
#> logsigma_a  6.387e+00        NaN        NA
#> logsigma_b -7.721e-01  1.099e-01    -7.024
#> logsigma_c -9.420e-01  2.281e-02   -41.302
#> logsigma_e -1.527e+00  5.210e-02   -29.302
#> rho_ab_raw -1.685e-02  7.616e-03    -2.212
#> rho_ac_raw  1.530e-01  5.004e-03    30.584
#> rho_bc_raw  5.267e-01        NaN        NA
#> 
#> Variance Components:
#> --------------------
#>             Estimate   Std. Error
#> alpha   0.000000e+00 0.000000e+00
#> k       1.825765e+14 1.191568e+13
#> var_a   3.527714e+05          NaN
#> var_b   2.135000e-01 4.690000e-02
#> var_c   1.520000e-01 6.900000e-03
#> cov_ab -4.622500e+00 2.635800e+00
#> cov_ac  3.516320e+01 1.095900e+00
#> cov_bc  8.550000e-02 9.700000e-03
#> var_e   4.720000e-02 4.900000e-03
#> 
#> Correlations:
#> -------------
#>        Estimate Std. Error
#> rho_ab  -0.0168     0.0076
#> rho_ac   0.1519     0.0049
#> rho_bc   0.4746        NaN
#> 
#> Model Fit:
#> ----------
#>   Log-likelihood: -80.84
#>   AIC: 185.68
#>   BIC: 231.63
#> 
#> Demand Metrics (Group-Level):
#> -----------------------------
#>   Pmax (price at max expenditure): 5.0093
#>   Omax (max expenditure): 13.4662
#>   Q at Pmax: 2.6882
#>   Elasticity at Pmax: 0.0000
#>   Method: analytic_lambert_w_hurdle
#> 
#> Derived Parameters (Individual-Level Summary):
#> ----------------------------------------------
#>   Q0 (Intensity):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.367   6.086   7.426   8.000  10.766  12.318 
#>   Alpha:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 6.078e-16 8.610e-16 1.045e-15 1.171e-15 1.331e-15 2.564e-15 
#>   Breakpoint:
#>       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> -0.0000731  5.5246723  9.0208189  9.5167226 11.5063300 24.7984416 
#>   Pmax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.136   4.116   5.239   5.328   6.362   9.011 
#>   Omax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   6.839   9.850  12.917  15.279  19.745  35.959 
#> 
# }
```
