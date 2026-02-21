# Fit Two-Part Mixed Effects Hurdle Demand Model

Fits a two-part hurdle model for demand data using TMB (Template Model
Builder). Part I models the probability of zero consumption using
logistic regression. Part II models log-consumption given positive
response using a nonlinear mixed effects model.

## Usage

``` r
fit_demand_hurdle(
  data,
  y_var,
  x_var,
  id_var,
  random_effects = c("zeros", "q0", "alpha"),
  epsilon = 0.001,
  start_values = NULL,
  tmb_control = list(max_iter = 200, eval_max = 1000, trace = 0),
  verbose = 1,
  part2 = c("zhao_exponential", "exponential", "simplified_exponential"),
  ...
)
```

## Arguments

- data:

  A data frame containing the demand data.

- y_var:

  Character string specifying the column name for consumption values.

- x_var:

  Character string specifying the column name for price.

- id_var:

  Character string specifying the column name for subject IDs.

- random_effects:

  Character vector specifying which random effects to include. Options
  are `"zeros"` (a_i for Part I), `"q0"` (b_i for intensity), and
  `"alpha"` (c_i for elasticity). Default is `c("zeros", "q0", "alpha")`
  for the full 3-random-effect model. Use `c("zeros", "q0")` for the
  simplified 2-random-effect model (fixed alpha across subjects).

- epsilon:

  Small constant added to price before log transformation in Part I.
  Used to handle zero prices: `log(price + epsilon)`. Default is 0.001.

- start_values:

  Optional named list of starting values for optimization. If `NULL`
  (default), sensible defaults are used.

- tmb_control:

  List of control parameters for TMB optimization:

  max_iter

  :   Maximum number of optimization iterations (default 200)

  eval_max

  :   Maximum number of function evaluations (default 1000)

  trace

  :   Print optimization trace: 0 = none, 1 = some (default 0)

- verbose:

  Integer controlling output verbosity: 0 = silent, 1 = progress
  messages, 2 = detailed optimization trace. Default is 1.

- part2:

  Character string selecting the Part II mean function. Options are
  `"zhao_exponential"` (default; no Q0 normalization in the exponent),
  `"exponential"` (HS-standardized; Q0 inside the exponent), and
  `"simplified_exponential"` (SND/log-linear; no `k` parameter).

- ...:

  Additional arguments (reserved for future use).

## Value

An object of class `beezdemand_hurdle` containing:

- model:

  List with coefficients, se, variance_components, correlations

- random_effects:

  Matrix of empirical Bayes random effect estimates

- subject_pars:

  Data frame of subject-specific parameters including Q0, alpha,
  breakpoint, Pmax, Omax

- tmb_obj:

  TMB objective function object

- opt:

  Optimization result from `nlminb`

- sdr:

  TMB sdreport object

- call:

  The matched call

- data:

  Original data used for fitting

- param_info:

  List with y_var, x_var, id_var, n_subjects, n_obs, etc.

- converged:

  Logical indicating convergence

- loglik:

  Log-likelihood at convergence

- AIC, BIC:

  Information criteria

- error_message:

  Error message if fitting failed, NULL otherwise

## Details

The model structure is:

**Part I (Binary - probability of zero consumption):**
\$\$logit(\pi\_{ij}) = \beta_0 + \beta_1 \cdot \log(price + \epsilon) +
a_i\$\$

**Part II (Continuous - log consumption given positive):**

With 3 random effects (`random_effects = c("zeros", "q0", "alpha")`):
\$\$\log(Q\_{ij}) = (\log Q_0 + b_i) + k \cdot (\exp(-\alpha_i \cdot
price) - 1) + \epsilon\_{ij}\$\$ where \\\alpha_i = \exp(\log(\alpha) +
c_i)\\ and \\k = \exp(\log(k))\\.

With 2 random effects (`random_effects = c("zeros", "q0")`):
\$\$\log(Q\_{ij}) = (\log Q_0 + b_i) + k \cdot (\exp(-\alpha \cdot
price) - 1) + \epsilon\_{ij}\$\$ where \\\alpha = \exp(\log(\alpha))\\
and \\k = \exp(\log(k))\\.

Random effects follow a multivariate normal distribution with
unstructured covariance matrix. Use
[`compare_hurdle_models`](https://brentkaplan.github.io/beezdemand/reference/compare_hurdle_models.md)
for likelihood ratio tests comparing nested models.

## Parameterization and comparability

The TMB backend estimates positive-constrained parameters on the
natural-log scale: \\\log(Q_0)\\, \\\log(\alpha)\\, and \\\log(k)\\.
Reporting methods ([`summary()`](https://rdrr.io/r/base/summary.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`coef()`](https://rdrr.io/r/stats/coef.html)) can back-transform to the
natural scale or present parameters on the \\\log\_{10}\\ scale.

To compare \\\alpha\\ estimates with models fit in \\\log\_{10}\\ space,
use: \$\$\log\_{10}(\alpha) = \log(\alpha) / \log(10).\$\$

## See also

[`summary.beezdemand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_hurdle.md),
[`predict.beezdemand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_hurdle.md),
[`plot.beezdemand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/plot.beezdemand_hurdle.md),
[`compare_hurdle_models`](https://brentkaplan.github.io/beezdemand/reference/compare_hurdle_models.md),
[`simulate_hurdle_data`](https://brentkaplan.github.io/beezdemand/reference/simulate_hurdle_data.md)

## Examples

``` r
# \donttest{
# Load example data
data(apt)

# Fit full model with 3 random effects
fit3 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                          random_effects = c("zeros", "q0", "alpha"))
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

# Fit simplified model with 2 random effects (fixed alpha)
fit2 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                          random_effects = c("zeros", "q0"))
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 9, Recommended minimum: 45 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand2RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 9, Random effects per subject: 2
#>   Optimizing...
#>   Converged in 93 iterations
#>   Computing standard errors...
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Note: k (1.742) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> Done. Log-likelihood: 2.31

# View results
summary(fit3)
#> 
#> Two-Part Mixed Effects Hurdle Demand Model
#> ============================================
#> 
#> Call:
#> fit_demand_hurdle(data = apt, y_var = "y", x_var = "x", id_var = "id", 
#>     random_effects = c("zeros", "q0", "alpha"))
#> 
#> Convergence: Yes 
#> Number of subjects: 10 
#> Number of observations: 160 
#> Random effects: 3 (zeros, q0, alpha) 
#> 
#> Fixed Effects:
#> --------------
#>              Estimate Std. Error t value
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

# Compare models with likelihood ratio test
compare_hurdle_models(fit3, fit2)
#> 
#> Likelihood Ratio Test
#> =====================
#>           Model n_RE   LogLik df       AIC       BIC
#>     Full (3 RE)    3 32.81453 12 -41.62905 -4.726965
#>  Reduced (2 RE)    2  2.30934  9  13.38132 41.057884
#> 
#> LR statistic: 61.0104 
#> df: 3 
#> p-value: 3.5757e-13 
# }
```
