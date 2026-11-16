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
#> Done. Log-likelihood: -86.08
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
#>              Estimate Std. Error  z value
#> beta0      -1.185e+02        NaN       NA
#> beta1       1.131e+02        NaN       NA
#> log_q0      1.995e+00  1.964e-02   101.55
#> log_k       3.333e+01        NaN       NA
#> log_alpha  -3.482e+01  1.354e-02 -2572.87
#> logsigma_a  6.134e+00        NaN       NA
#> logsigma_b -6.897e-01        NaN       NA
#> logsigma_c -9.218e-01  8.597e-03  -107.22
#> logsigma_e -1.504e+00  4.864e-02   -30.92
#> rho_ab_raw -5.737e-02        NaN       NA
#> rho_ac_raw  1.419e-01        NaN       NA
#> rho_bc_raw  5.490e-01        NaN       NA
#> 
#> Variance Components:
#> --------------------
#>             Estimate Std. Error
#> alpha   0.000000e+00     0.0000
#> k       2.997419e+14        NaN
#> var_a   2.128520e+05        NaN
#> var_b   2.518000e-01        NaN
#> var_c   1.583000e-01     0.0027
#> cov_ab -1.326540e+01        NaN
#> cov_ac  2.587260e+01        NaN
#> cov_bc  9.700000e-02        NaN
#> var_e   4.940000e-02     0.0048
#> 
#> Correlations:
#> -------------
#>        Estimate Std. Error
#> rho_ab  -0.0573        NaN
#> rho_ac   0.1410        NaN
#> rho_bc   0.4859        NaN
#> 
#> Model Fit:
#> ----------
#>   Log-likelihood: -86.08
#>   AIC: 196.17
#>   BIC: 242.11
#> 
#> Demand Metrics (Group-Level):
#> -----------------------------
#>   Pmax (price at max expenditure): 4.4332
#>   Omax (max expenditure): 12.0084
#>   Q at Pmax: 2.7087
#>   Elasticity at Pmax: 0.0000
#>   Method: analytic_lambert_w_hurdle
#> 
#> Derived Parameters (Individual-Level Summary):
#> ----------------------------------------------
#>   Q0 (Intensity):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.403   6.156   7.532   8.077  10.904  12.465 
#>   Alpha:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 3.899e-16 5.636e-16 6.470e-16 7.361e-16 8.358e-16 1.572e-15 
#>   Breakpoint:
#>       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> -0.0000415  5.4983508  8.9759897  9.7593745 11.2859038 29.0094132 
#>   Pmax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.122   3.992   5.157   5.131   5.921   8.557 
#>   Omax:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   6.301   9.741  12.081  14.919  18.928  35.821 
#> 
# }
```
