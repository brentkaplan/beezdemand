# Augment a Cross-Price Demand Model (Mixed-Effects)

Augment a Cross-Price Demand Model (Mixed-Effects)

## Usage

``` r
# S3 method for class 'cp_model_lmer'
augment(x, ...)
```

## Arguments

- x:

  A `cp_model_lmer` object.

- ...:

  Additional arguments (unused).

## Value

A tibble with the original modelling data and added `.fitted`, `.resid`,
and `.fixed` (population-level prediction with random effects set to
zero) columns.
