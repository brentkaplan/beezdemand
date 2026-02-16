# Extract Coefficients from Fixed-Effect Demand Fit

Extract Coefficients from Fixed-Effect Demand Fit

## Usage

``` r
# S3 method for class 'beezdemand_fixed'
coef(object, report_space = c("internal", "natural", "log10"), ...)
```

## Arguments

- object:

  A `beezdemand_fixed` object.

- report_space:

  One of `"internal"`, `"natural"`, or `"log10"`. Default `"internal"`.

- ...:

  Unused.

## Value

A tibble with columns `id`, `term`, `estimate`, `estimate_scale`,
`term_display`.
