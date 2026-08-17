# Confidence Intervals for a Cross-Price Demand Model (Linear)

Wald confidence intervals via
[`stats::confint.default()`](https://rdrr.io/r/stats/confint.html).

## Usage

``` r
# S3 method for class 'cp_model_lm'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  A `cp_model_lm` object.

- parm:

  Optional character vector of parameter names. `NULL` returns intervals
  for all parameters.

- level:

  Confidence level (default `0.95`).

- ...:

  Additional arguments (unused).

## Value

A tibble with columns `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `method`.
