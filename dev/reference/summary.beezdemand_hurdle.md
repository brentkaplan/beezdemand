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
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
summary(fit)
} # }
```
