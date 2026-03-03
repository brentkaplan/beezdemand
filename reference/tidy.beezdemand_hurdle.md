# Tidy a beezdemand_hurdle Model

Returns a tibble of model coefficients in tidy format.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
tidy(x, report_space = c("natural", "log10", "internal"), ...)
```

## Arguments

- x:

  An object of class `beezdemand_hurdle`.

- report_space:

  Character. Reporting space for core demand parameters. One of
  `"internal"`, `"natural"`, or `"log10"`.

- ...:

  Additional arguments (currently unused).

## Value

A tibble with columns:

- term:

  Parameter name

- estimate:

  Point estimate

- std.error:

  Standard error

- statistic:

  z-value

- p.value:

  P-value

- component:

  One of "zero_probability", "consumption", "variance", or "fixed"
