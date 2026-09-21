# Check Demand Model Diagnostics

Performs diagnostic checks on fitted demand models, returning
information about convergence, boundary conditions, and residual
patterns.

## Usage

``` r
check_demand_model(object, ...)

# S3 method for class 'beezdemand_hurdle'
check_demand_model(object, ...)

# S3 method for class 'beezdemand_nlme'
check_demand_model(object, ...)

# S3 method for class 'beezdemand_fixed'
check_demand_model(object, ...)

# S3 method for class 'beezdemand_tmb'
check_demand_model(object, ...)
```

## Arguments

- object:

  A fitted model object of class `beezdemand_hurdle`, `beezdemand_nlme`,
  or `beezdemand_fixed`.

- ...:

  Additional arguments passed to methods.

## Value

An object of class `beezdemand_diagnostics` containing:

- convergence:

  List with convergence status and messages

- boundary:

  List with boundary condition warnings. For `beezdemand_tmb` fits it
  also carries `k_identification`: `NULL` unless k was estimated as a
  free parameter, otherwise the screen described in Details.

- residuals:

  Summary statistics for residuals

- random_effects:

  Summary of random effects (if applicable). `variances` holds
  random-effect variances: on the log10 scale for `beezdemand_tmb` fits,
  and as reported by
  [`nlme::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html) for
  `beezdemand_nlme` fits. `beezdemand_tmb` fits also carry `sd_log10`
  (the corresponding standard deviations, matching
  `summary(fit)$variance_components`) and `sd_internal_log` (the raw
  natural-log-scale standard deviations used by the near-zero degeneracy
  check).

- issues:

  Character vector of identified issues

- recommendations:

  Character vector of recommendations

## Details

The function checks for:

- Convergence status and optimization messages

- Parameters at or near boundaries

- Residual patterns (heteroscedasticity, outliers)

- Random effect variance estimates near zero

- For
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  fits with `estimate_k = TRUE`, whether the fit shows the signature of
  an unidentified k: an implausible k, a decay exponent
  `alpha * Q0 * price` that never leaves its linear regime over the
  observed prices, a degenerate alpha, or `log_k` resting on a
  user-supplied bound. This is a screen, not a formal test: no flag
  means nothing was detected, not that k is identified.

- Correlation matrices near singularity

## Note

This function is named `check_demand_model()` to avoid potential
conflicts with
[`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
from the performance package.

## See also

[`plot_residuals()`](https://brentkaplan.github.io/beezdemand/reference/plot_residuals.md),
[`plot_qq()`](https://brentkaplan.github.io/beezdemand/reference/plot_qq.md)

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
diagnostics <- check_demand_model(fit)
print(diagnostics)
#> 
#> Model Diagnostics
#> ================================================== 
#> Model class: beezdemand_hurdle 
#> 
#> Convergence:
#>   Status: Converged
#> 
#> Random Effects:
#> 
#> Residuals:
#>   Mean: 0.01173
#>   SD: 0.9277
#>   Range: [-2.862, 3.312]
#>   Outliers: 3 observations
#> 
#> --------------------------------------------------
#> Issues Detected (1):
#>   1. Detected 3 potential outliers (|resid| > 3)
#> 
#> Recommendations:
#>   - Investigate outlying observations
# }
```
