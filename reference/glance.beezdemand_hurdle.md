# Glance at a beezdemand_hurdle Model

Returns a one-row tibble of model-level statistics.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
glance(x, ...)
```

## Arguments

- x:

  An object of class `beezdemand_hurdle`.

- ...:

  Additional arguments (currently unused).

## Value

A one-row tibble with columns:

- model_class:

  "beezdemand_hurdle"

- backend:

  "TMB"

- nobs:

  Number of observations

- n_subjects:

  Number of subjects

- n_random_effects:

  Number of random effects

- converged:

  Convergence status

- logLik:

  Log-likelihood

- AIC:

  Akaike Information Criterion

- BIC:

  Bayesian Information Criterion
