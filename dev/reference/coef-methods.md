# Extract Coefficients from Cross-Price Demand Models

Methods to extract coefficients from various cross-price demand model
objects.

## Usage

``` r
# S3 method for class 'cp_model_nls'
coef(object, ...)

# S3 method for class 'cp_model_lm'
coef(object, ...)

# S3 method for class 'cp_model_lmer'
coef(object, fixed_only = FALSE, combine = TRUE, ...)
```

## Arguments

- object:

  A cp_model_lm object

- ...:

  Additional arguments (not used).

- fixed_only:

  Logical; if TRUE, returns only fixed effects. Default is FALSE.

- combine:

  Logical; if TRUE and fixed_only=FALSE, returns fixed + random effects
  combined. Default is TRUE.

## Value

Named vector of coefficients

## Functions

- `coef(cp_model_nls)`: Extract coefficients from a nonlinear
  cross-price model

- `coef(cp_model_lm)`: Extract coefficients from a linear cross-price
  model

- `coef(cp_model_lmer)`: Extract coefficients from a mixed-effects
  cross-price model
