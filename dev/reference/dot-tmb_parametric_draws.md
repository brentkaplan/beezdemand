# Parametric Monte Carlo draws from a TMB fit's asymptotic posterior

Draws `R` samples of the internal-scale fixed-effect parameter vector
from the joint asymptotic Gaussian posterior \\N(\hat\beta,
\hat\Sigma)\\, where \\\hat\beta\\ is `object$model$coefficients` and
\\\hat\Sigma\\ is `vcov(object)` (the TMB `sdreport` fixed-effect
covariance, `sdr$cov.fixed`). The mean vector and covariance are
positionally aligned because both derive from the optimizer's `opt$par`.

## Usage

``` r
.tmb_parametric_draws(object, R = 1000L, seed = NULL)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- R:

  Integer number of Monte Carlo draws.

- seed:

  Optional integer seed. When supplied, the caller's RNG state is saved
  and restored so the global `.Random.seed` stream is left unperturbed.

## Value

Numeric matrix of dimension `R x p`, with columns named after the
internal coefficient vector (`names(object$model$coefficients)`).

## Details

This is the shared primitive behind
`confint(object, method = "simulate")` and is intended for reuse by
derived-metric bootstrap helpers. Draws are fixed-effect-only on the
internal scale; callers transform to the natural scale or evaluate
derived metrics as needed.

The symmetric matrix square root is formed via an eigendecomposition
with negative eigenvalues clamped to zero, matching
[`MASS::mvrnorm`](https://rdrr.io/pkg/MASS/man/mvrnorm.html)'s
robustness to a near-semidefinite covariance without taking a
dependency.
