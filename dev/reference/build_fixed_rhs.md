# Build Fixed-Effects RHS Formula String

Internal helper to construct the right-hand side of a fixed-effects
formula from factors, interaction flag, and continuous covariates.

## Usage

``` r
build_fixed_rhs(
  factors = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL,
  data = NULL
)
```

## Arguments

- factors:

  Character vector of factor names (can be NULL).

- factor_interaction:

  Logical. If TRUE and two factors provided, include their interaction.

- continuous_covariates:

  Character vector of continuous covariate names.

## Value

A character string representing the RHS (e.g., "~ 1 + dose + drug").
