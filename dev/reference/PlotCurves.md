# Plot Curves

Creates plots

## Usage

``` r
PlotCurves(dat, outdir = NULL, device = "png", ending = NULL, ask = TRUE, ...)
```

## Arguments

- dat:

  FitCurves object with 4 elements (dfres, newdats, adfs, fits)

- outdir:

  Directory where plots are saved

- device:

  Type of file. Default is "png". Can be "pdf"

- ending:

  Optional. Can specify to only plot through a certain number of
  datasets

- ask:

  Can view plots one by one. If TRUE, plots will not save

- ...:

  Pass arguments to PlotCurve (for example yscale = c("log", "linear"))

## Value

Nothing

## Details

Creates and saves plots of individual demand curves

## Author

Brent Kaplan \<bkaplan.ku@gmail.com\>, Shawn Gilroy
\<shawn.gilroy@temple.edu\>

## Examples

``` r
## Interactively view plots from output from FitCurves
if (interactive()) {
  fc <- FitCurves(apt, "hs", k = 2, detailed = TRUE)
  PlotCurves(fc, ask = TRUE)
}
```
