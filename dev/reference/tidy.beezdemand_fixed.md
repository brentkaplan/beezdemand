# Tidy Method for beezdemand_fixed

Tidy Method for beezdemand_fixed

## Usage

``` r
# S3 method for class 'beezdemand_fixed'
tidy(x, report_space = c("natural", "log10"), ...)
```

## Arguments

- x:

  A beezdemand_fixed object

- report_space:

  Character. Reporting space for core parameters. One of \`"natural"\`
  (default) or \`"log10"\`.

- ...:

  Additional arguments (ignored)

## Value

A tibble of model coefficients with columns: id, term, estimate,
std.error, statistic, p.value, component
