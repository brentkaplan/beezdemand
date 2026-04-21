# Build Design Matrices for TMB Model

Build Design Matrices for TMB Model

## Usage

``` r
.tmb_build_design_matrices(
  data,
  factors_q0 = NULL,
  factors_alpha = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL
)
```

## Arguments

- data:

  Data frame with factors already applied.

- factors_q0:

  Character vector of factor names for Q0.

- factors_alpha:

  Character vector of factor names for alpha.

- factor_interaction:

  Logical.

- continuous_covariates:

  Character vector.

## Value

A list with X_q0, X_alpha design matrices and formula strings.
