# Augment a Cross-Price Demand Model (Nonlinear)

Augment a Cross-Price Demand Model (Nonlinear)

## Usage

``` r
# S3 method for class 'cp_model_nls'
augment(x, ...)
```

## Arguments

- x:

  A `cp_model_nls` object.

- ...:

  Additional arguments (unused).

## Value

A tibble with the original modelling data and added `.fitted` and
`.resid` columns.
