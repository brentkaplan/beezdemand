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

- `converged`: Operational convergence status. `TRUE` when the final fit
  is usable for inference, i.e., `apVar` (nlme's approximate covariance
  of the variance-covariance parameters) is positive-definite and there
  is no terminal error. Alias for `final_fit_ok`. It is not flipped to
  `FALSE` by iteration-level optimizer warnings (see `fit_warned`).

- `final_fit_ok`: The canonical usable-for-inference gate (`apVar` PD
  and no terminal error); identical to `converged`. NLME-only.

- `fit_warned`: Diagnostic flag that is `TRUE` when nlme emitted
  iteration-level convergence warnings (false convergence, singular,
  step-halving, iteration limit, ...) during PNLS-LME alternation.
  Informational only; does not gate `converged`. NLME-only.

- `logLik`, `AIC`, `BIC`: Model fit statistics

- `sigma`: Residual standard error (NLME-only)

The shared canonical columns (through `converged`, `logLik`, `AIC`,
`BIC`) match
[`glance.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/glance.beezdemand_tmb.md),
so backend-agnostic code needs no dispatch glue; `final_fit_ok` and
`fit_warned` are additive NLME-only diagnostics.
