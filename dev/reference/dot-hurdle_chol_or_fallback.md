# Cholesky Decomposition with Non-PD Fallback

Internal helper shared by the RE-transform and marginal-prediction code
paths. Attempts `chol(Sigma)`; if `Sigma` is not positive definite
([`chol()`](https://rdrr.io/r/base/chol.html) errors, e.g. from
near-boundary rhos, overflow, or `tanh(raw)` rounding to exactly +/-1),
falls back to the Cholesky factor of an uncorrelated diagonal covariance
built from `sigma_diag` and emits ONE classed warning, since the
returned/reported correlation estimates no longer describe the
transformed random effects or marginal draws that result (TICKET-061).

## Usage

``` r
.hurdle_chol_or_fallback(Sigma, sigma_diag)
```

## Arguments

- Sigma:

  A candidate covariance matrix.

- sigma_diag:

  Numeric vector of per-RE variances for the diagonal fallback (length
  must equal `nrow(Sigma)`).

## Value

The upper-triangular Cholesky factor of `Sigma`, or of the diagonal
fallback when `Sigma` is not positive definite.
