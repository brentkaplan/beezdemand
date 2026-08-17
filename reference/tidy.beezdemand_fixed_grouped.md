# Tidy Method for beezdemand_fixed_grouped

Calls [`tidy()`](https://generics.r-lib.org/reference/tidy.html) on each
per-group child and prepends group columns.

## Usage

``` r
# S3 method for class 'beezdemand_fixed_grouped'
tidy(x, ...)
```

## Arguments

- x:

  A beezdemand_fixed_grouped object

- ...:

  Additional arguments passed to
  [`tidy.beezdemand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_fixed.md)

## Value

A tibble with group columns prepended
