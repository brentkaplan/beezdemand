# Get model summaries from a mixed-effects cross-price model

Get model summaries from a mixed-effects cross-price model

## Usage

``` r
# S3 method for class 'cp_model_lmer'
glance(x, ...)
```

## Arguments

- x:

  A cp_model_lmer object.

- ...:

  Additional arguments passed to broom.mixed::glance.

## Value

A tibble with model summary statistics.
