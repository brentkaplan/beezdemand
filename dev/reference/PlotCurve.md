# Plot Curve

Creates a single plot object

## Usage

``` r
PlotCurve(adf, dfrow, newdats, yscale = "log", style = c("modern", "apa"))
```

## Arguments

- adf:

  Data frame (long form) of purchase task data.

- dfrow:

  A row of results from FitCurves

- newdats:

  A newdat dataframe from FitCurves

- yscale:

  Scaling of y axis. Default is "log". Can also take "linear"

- style:

  Plot styling, passed to
  [`theme_beezdemand()`](https://brentkaplan.github.io/beezdemand/reference/theme_beezdemand.md).

## Value

ggplot2 graphical object

## Details

Creates individual demand curves

## Author

Shawn Gilroy \<shawn.gilroy@temple.edu\>

## Examples

``` r
## Creates a single plot from elements of an object created by FitCurves
if (interactive()) {
  fc <- FitCurves(apt, "hs", k = 2, detailed = TRUE)
  PlotCurve(fc$adfs[[1]], fc$dfres[1, ], fc$newdats[[1]])
}
```
