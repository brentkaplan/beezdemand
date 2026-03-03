# Transform Random Effects to Original Scale

Internal function to transform standardized random effects (u) to the
original scale using the Cholesky decomposition of the covariance
matrix.

## Usage

``` r
.hurdle_transform_random_effects(u_hat, coefficients, n_re)
```

## Arguments

- u_hat:

  Matrix of standardized random effects.

- coefficients:

  Named vector of fixed effect coefficients.

- n_re:

  Number of random effects.

## Value

Matrix of random effects on original scale with named columns.
