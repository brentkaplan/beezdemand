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

- `converged`: Convergence status

- `logLik`, `AIC`, `BIC`: Model fit statistics

- `sigma`: Residual standard error
