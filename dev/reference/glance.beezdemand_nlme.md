# Glance method for beezdemand_nlme

Glance method for beezdemand_nlme

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
glance(x, ...)
```

## Arguments

- x:

  A beezdemand_nlme object

- ...:

  Additional arguments (ignored)

## Value

A one-row tibble of model statistics with columns:

- `model_class`: "beezdemand_nlme"

- `backend`: "nlme"

- `equation_form`: The equation form used

- `nobs`: Number of observations

- `n_subjects`: Number of subjects

- `n_random_effects`: Number of random-effect terms (e.g. 2 for
  `Q0 + alpha ~ 1`)

- `converged`: Convergence status

- `logLik`, `AIC`, `BIC`: Model fit statistics

- `sigma`: Residual standard error (NLME-only)

The canonical columns match
[`glance.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/glance.beezdemand_tmb.md),
so backend-agnostic code needs no dispatch glue.
