# Back-transform predictions from model scale to natural consumption scale

Back-transform predictions from model scale to natural consumption scale

## Usage

``` r
.tmb_backtransform(fitted, equation, sigma_e = NULL, correction = TRUE)
```

## Arguments

- fitted:

  Numeric vector of predictions on model scale.

- equation:

  Character. The equation used for fitting.

- sigma_e:

  Numeric scalar. Residual standard deviation on the model scale. Used
  for lognormal retransformation correction when `correction = TRUE`.

- correction:

  Logical. If `TRUE`, applies the lognormal retransformation correction
  `exp(sigma_e^2 / 2)` for the exponential equation. Default `TRUE`.

## Value

Numeric vector of predictions on the natural (consumption) scale.
