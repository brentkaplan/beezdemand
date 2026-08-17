# Augment a Cross-Price Demand Model (Linear)

Augment a Cross-Price Demand Model (Linear)

## Usage

``` r
# S3 method for class 'cp_model_lm'
augment(x, ...)
```

## Arguments

- x:

  A `cp_model_lm` object.

- ...:

  Additional arguments (unused).

## Value

A tibble with the original modelling data and added `.fitted` and
`.resid` columns.
