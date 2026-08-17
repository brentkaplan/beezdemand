# Plot Method for beezdemand_fixed_grouped

Calls [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on each
per-group child and combines with
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
if available, otherwise returns a list.

## Usage

``` r
# S3 method for class 'beezdemand_fixed_grouped'
plot(x, ...)
```

## Arguments

- x:

  A beezdemand_fixed_grouped object

- ...:

  Additional arguments passed to
  [`plot.beezdemand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/plot.beezdemand_fixed.md)

## Value

A combined patchwork plot if patchwork is available, otherwise a named
list of ggplot objects
