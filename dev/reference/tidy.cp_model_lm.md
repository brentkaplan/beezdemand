# Extract coefficients from a linear cross-price model in tidy format

Extract coefficients from a linear cross-price model in tidy format

## Usage

``` r
# S3 method for class 'cp_model_lm'
tidy(x, ...)
```

## Arguments

- x:

  A cp_model_lm object.

- ...:

  Additional arguments (unused).

## Value

A tibble with columns: term, estimate, std.error, statistic, p.value.
