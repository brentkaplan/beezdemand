# Fit Mixed-Effects Demand Models via TMB

Fits nonlinear mixed-effects demand models using Template Model Builder
(TMB) for exact automatic differentiation and Laplace approximation.
This provides reliable convergence where traditional NLME approaches
fail.

## Usage

``` r
fit_demand_tmb(
  data,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  equation = c("exponentiated", "exponential", "simplified", "zben"),
  estimate_k = TRUE,
  k = NULL,
  random_effects = Q0 + alpha ~ 1,
  covariance_structure = c("pdSymm", "pdDiag"),
  factors = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL,
  collapse_levels = NULL,
  start_values = NULL,
  tmb_control = list(iter_max = 1000, eval_max = 2000),
  multi_start = TRUE,
  validate_subject_pars = TRUE,
  verbose = 1,
  ...
)
```

## Arguments

- data:

  A data frame in long format with columns for subject ID, price, and
  consumption.

- y_var:

  Character. Name of the consumption/response variable.

- x_var:

  Character. Name of the price variable.

- id_var:

  Character. Name of the subject identifier variable.

- equation:

  Character. The demand equation to fit. One of:

  `"exponentiated"`

  :   Koffarnus et al. (2015). Gaussian on raw Q. Zeros allowed. Has k
      parameter.

  `"exponential"`

  :   Hursh & Silberberg (2008). Gaussian on log(Q). Observations with Q
      = 0 are automatically dropped. Has k parameter.

  `"simplified"`

  :   Simplified exponential (no k). Gaussian on raw Q. Zeros allowed.

  `"zben"`

  :   Zero-bounded exponential (no k). Gaussian on LL4- transformed Q.
      User must pass LL4-transformed y_var. Note: Q0 on the log10 scale
      is clamped to a minimum of 0.001 to avoid a singularity at Q0 = 1
      (where log10(Q0) = 0 causes division by zero in the decay rate).
      Subjects with estimated Q0 near 1 may have biased parameter
      estimates.

- estimate_k:

  Logical. If `TRUE` (default), estimate k as a free parameter. If
  `FALSE`, fix k at the value given in `k`. Only relevant for
  "exponentiated" and "exponential" equations.

- k:

  Numeric or `NULL`. Fixed value of k when `estimate_k = FALSE`. If
  `NULL` and `estimate_k = FALSE`, k defaults to 2.

- random_effects:

  Specification of subject-level random effects. Accepts any of the
  following, in order of generality:

  formula (default)

  :   `Q0 + alpha ~ 1` – random intercepts on both parameters
      (equivalent to the legacy `c("q0", "alpha")` shortcut). `Q0 ~ 1`
      limits REs to Q0. Formulas with factor-expanded RHS (e.g.,
      `Q0 + alpha ~ condition` or `Q0 + alpha ~ condition - 1`) are now
      supported in Phase 2 – see TICKET-011 Phase 2 for details. The
      within-subject factor must vary within each `id`; pure
      between-subject factors belong in `factors`, not in the RE
      formula.

  [`nlme::pdMat`](https://rdrr.io/pkg/nlme/man/pdMat.html)

  :   e.g., `nlme::pdDiag(Q0 + alpha ~ 1)` or
      `nlme::pdSymm(Q0 + alpha ~ condition)`. Pre-constructed pdMat
      objects are accepted and their covariance class is honored
      (overrides `covariance_structure`).

  list of `pdMat` / [`nlme::pdBlocked`](https://rdrr.io/pkg/nlme/man/pdBlocked.html)

  :   Multi-block covariance structures like
      `list(pdSymm(Q0+alpha~1), pdDiag(Q0+alpha~cond-1))`. Parsed, but
      fitting is deferred to Phase 3 of TICKET-011 – use
      [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
      for multi-block fits in the meantime.

  character vector (deprecated)

  :   `c("q0", "alpha")` or `"q0"`. Soft-deprecated in 0.4.0; emits a
      [`lifecycle::deprecate_soft()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
      message. Translated internally to the formula `Q0 + alpha ~ 1` or
      `Q0 ~ 1`.

- covariance_structure:

  `"pdSymm"` (default; unstructured) or `"pdDiag"` (diagonal). Applies
  only when `random_effects` is a formula; ignored for pre-constructed
  pdMat / list / pdBlocked inputs.

- factors:

  Character vector of factor variable names for group comparisons.

- factor_interaction:

  Logical. If `TRUE` and two factors provided, include their
  interaction.

- continuous_covariates:

  Character vector of continuous covariate names.

- collapse_levels:

  Named list for asymmetric factor collapsing. Structure:
  `list(Q0 = list(factor = list(new = c(old))), alpha = list(...))`.

- start_values:

  Named list of starting values. If `NULL`, data-driven defaults are
  used.

- tmb_control:

  List of control parameters for the optimizer:

  `optimizer`

  :   Character. `"nlminb"` (default) or `"L-BFGS-B"`. L-BFGS-B can
      recover from nlminb convergence failures (code 1 or 8).

  `iter_max`

  :   Maximum iterations (default 1000).

  `eval_max`

  :   Maximum function evaluations (default 2000). Only applies to
      nlminb; L-BFGS-B has no function evaluation limit.

  `rel_tol`

  :   Relative convergence tolerance (default 1e-10). Only applies to
      nlminb.

  `lower`

  :   Named numeric vector of lower bounds on optimizer-scale parameters
      (default NULL = no bounds). Names must match optimizer parameter
      names (e.g., `log_k`, `beta_q0`, `logsigma_b`). Note that most
      parameters are in log-space: e.g., to constrain k between 0.14 and
      55, use `lower = c(log_k = -2)`, `upper = c(log_k = 4)`. A bound
      name applies to *all* occurrences of that parameter (e.g., both
      elements of `beta_q0`).

  `upper`

  :   Named numeric vector of upper bounds (see `lower`).

  `warm_start`

  :   Named numeric vector of starting values in optimizer space (e.g.,
      from a previous `fit$opt$par`). When provided, `multi_start` is
      automatically disabled. This differs from `start_values`, which
      operates in parameter space before
      [`TMB::MakeADFun()`](https://rdrr.io/pkg/TMB/man/MakeADFun.html).
      Length must match the number of free parameters.

  `trace`

  :   Non-negative integer controlling optimizer trace output (default
      0). When not explicitly set, inherits from `verbose >= 2`.

- multi_start:

  Logical. If `TRUE` (default), try 3 starting value sets and select the
  best.

- validate_subject_pars:

  Logical. If `TRUE` (default), validate that every column of the
  fixed-effect design matrices is constant within each `id` before
  computing `subject_pars`. When a factor or continuous covariate varies
  within subject, Q0/alpha/Pmax/Omax are set to `NA_real_` for affected
  subjects and a warning names the offending columns. Set to `FALSE` to
  force row-order-dependent values (not recommended; proper
  factor-expanded RE support lands in TICKET-011 Phases 2-3).

- verbose:

  Integer. Verbosity level: 0 = silent, 1 = progress, 2 = debug.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `beezdemand_tmb` containing:

- model:

  List with coefficients, se, variance_components

- subject_pars:

  Data frame of subject-specific Q0, alpha, Pmax, Omax

- tmb_obj:

  TMB objective function object

- opt:

  Optimization result (normalized across optimizers)

- sdr:

  TMB sdreport object

- converged:

  Logical convergence indicator

- loglik:

  Log-likelihood at convergence

- AIC:

  Akaike Information Criterion

- BIC:

  Bayesian Information Criterion

- data:

  Original data (after any filtering)

- param_info:

  List of model metadata

- formula_details:

  Design matrix and formula information

- collapse_info:

  Collapse levels information (if used)

## Details

Traditional NLME approaches using
[`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html) universally
fail for demand equations because the PNLS algorithm with numerical
finite-difference gradients cannot navigate the likelihood surface. TMB
succeeds using exact automatic differentiation, Laplace approximation,
and joint marginal likelihood optimization.

When `estimate_k = TRUE`, k is estimated as a free parameter alongside
Q0 and alpha. This typically improves model fit substantially. The
conventional fixed-k approach (Hursh & Silberberg, 2008) often
overestimates k by 3-8x.

**Error model considerations:** The `exponentiated` and `simplified`
equations use a Gaussian error model on raw consumption (Q), which
assigns non-zero density to negative values. For data with many
near-zero observations, prefer `exponential` (Gaussian on log Q, zeros
dropped) or `zben` (Gaussian on LL4-transformed Q, zeros handled by the
transformation).

## See also

[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
for NLME-based fitting,
[`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
for two-part hurdle models,
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
for individual NLS curves.

Other demand-fitting:
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md),
[`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md),
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)

## Examples

``` r
# \donttest{
data(apt)

# Exponential (HS) on log(Q)
fit <- fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
                      equation = "exponential")
#> Fitting TMB mixed-effects demand model...
#>   Equation: exponential
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
#>   Subjects: 10, Observations: 146
#>   Random effects: 2 total RE columns per subject (pdSymm(Q0:1, alpha:1))
#>   Design matrices: X_q0 [146 x 1], X_alpha [146 x 1]
#>   Optimizing...
#>   Multi-start: best NLL = -40.65 (start set 3 of 3)
#>   Converged (NLL = -40.65)
#>   Computing standard errors...
#> Done.
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
plot(fit)
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.


# Exponentiated (Koffarnus) on raw Q
fit2 <- fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
                       equation = "exponentiated")
#> Fitting TMB mixed-effects demand model...
#>   Equation: exponentiated
#>   Subjects: 10, Observations: 160
#>   Random effects: 2 total RE columns per subject (pdSymm(Q0:1, alpha:1))
#>   Design matrices: X_q0 [160 x 1], X_alpha [160 x 1]
#>   Optimizing...
#>   Multi-start: best NLL = 171.12 (start set 2 of 3)
#>   WARNING: Did not converge (code 1: false convergence (8))
#>   Computing standard errors...
#> Warning: NaNs produced
#> Warning: ! Hessian is not positive definite (`pdHess = FALSE`).
#> ℹ Standard errors, p-values, and confidence intervals may be unreliable.
#> ℹ Run `check_demand_model()` for detailed diagnostics.
#> ℹ Consider simplifying the model (fewer random effects) or checking data
#>   quality.
#> Warning: NaNs produced
#> Done.

# With covariates
data(apt_full)
fit3 <- fit_demand_tmb(apt_full, y_var = "y", x_var = "x", id_var = "id",
                       equation = "exponential", factors = "gender")
#> Fitting TMB mixed-effects demand model...
#>   Equation: exponential
#>   equation='exponential': Dropped 5861 zero-consumption observations (12839 remaining).
#>   Subjects: 1090, Observations: 12839
#>   Random effects: 2 total RE columns per subject (pdSymm(Q0:1, alpha:1))
#>   Design matrices: X_q0 [12839 x 3], X_alpha [12839 x 3]
#>   Optimizing...
#>   Multi-start: best NLL = 3301.57 (start set 1 of 3)
#>   Converged (NLL = 3301.57)
#>   Computing standard errors...
#> Done.
get_demand_param_emms(fit3, param = "alpha")
#> # A tibble: 3 × 6
#>   level                       estimate estimate_log std.error conf.low conf.high
#>   <chr>                          <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Male                  0.00626        -5.07    0.0475  5.70e-3   0.00687
#> 2 gender=Female                0.00739        -4.91    0.0456  6.76e-3   0.00808
#> 3 gender=Would rather not say  0.00290        -5.84    2.92    9.43e-6   0.890  

# Factor-expanded random slopes on a within-subject factor (Phase 2):
# each subject contributes a Q0 / alpha RE per condition level.
# Replace `cond` below with your in-subject factor.
if (FALSE) { # \dontrun{
fit4 <- fit_demand_tmb(within_data, y_var = "y_ll4", x_var = "x", id_var = "id",
                       equation = "zben", factors = "cond",
                       random_effects = nlme::pdDiag(Q0 + alpha ~ cond))
} # }
# }
```
