# Summarize a Cross-Price Demand Model (Nonlinear)

Summarize a Cross-Price Demand Model (Nonlinear)

## Usage

``` r
# S3 method for class 'cp_model_nls'
summary(object, inv_fun = identity, inverse_fun = deprecated(), ...)
```

## Arguments

- object:

  A cross-price model object from fit_cp_nls with return_all=TRUE.

- inv_fun:

  Optional function to inverse-transform predictions (e.g., ll4_inv).
  Default is `identity`.

- inverse_fun:

  **\[deprecated\]** Use `inv_fun` instead.

- ...:

  Additional arguments (unused).

## Value

A list containing model summary information.
