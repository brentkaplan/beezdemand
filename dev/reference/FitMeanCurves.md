# Fit Pooled/Mean Curves

Fits curve to pooled or mean data

## Usage

``` r
FitMeanCurves(
  dat,
  equation,
  k,
  remq0e = FALSE,
  replfree = NULL,
  rem0 = FALSE,
  nrepl = NULL,
  replnum = NULL,
  plotcurves = FALSE,
  method = NULL,
  indpoints = TRUE,
  vartext = NULL
)
```

## Arguments

- dat:

  data frame (long form) of purchase task data.

- equation:

  Character vector of length one. Accepts either "hs" for Hursh and
  Silberberg (2008) or "koff" for Koffarnus, Franck, Stein, and Bickel
  (2015).

- k:

  A numeric vector of length one. Reflects the range of consumption in
  log10 units. If none provided, k will be calculated based on the
  max/min of the entire sample. If k = "fit", k will be a free parameter

- remq0e:

  If TRUE, removes consumption and price where price == 0. Default value
  is FALSE

- replfree:

  Optionally replaces price == 0 with specified value. Note, if fitting
  using equation == "hs", and 0 is first price, 0 gets replaced by
  replfree. Default value is .01

- rem0:

  If TRUE, removes all 0s in consumption data prior to analysis. Default
  value is FALSE.

- nrepl:

  Number of zeros to replace with replacement value (replnum). Can
  accept either a number or "all" if all zeros should be replaced.
  Default is to replace the first zero only.

- replnum:

  Value to replace zeros. Default is .01

- plotcurves:

  Boolean whether to create plot. If TRUE, a "plots/" directory is
  created one level above working directory. Default is FALSE.

- method:

  Character string of length 1. Accepts "Mean" to fit to mean data or
  "Pooled" to fit to pooled data

- indpoints:

  Boolean whether to plot individual points in gray. Default is TRUE.

- vartext:

  Character vector specifying indices to report on plots. Valid indices
  include "Q0d", "Alpha", "Q0e", "EV", "Pmaxe", "Omaxe", "Pmaxd",
  "Omaxd", "K", "Q0se", "Alphase", "R2", "AbsSS"

## Value

Data frame

## Details

**\[superseded\]**

`FitMeanCurves()` has been superseded by
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
with the `agg` parameter. `FitMeanCurves()` will continue to work but is
no longer recommended for new code. See
[`vignette("migration-guide")`](https://brentkaplan.github.io/beezdemand/articles/migration-guide.md)
for migration instructions.

## See also

[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
for the modern interface with `agg` parameter

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## Fit aggregated data (mean only) using Hursh & Silberberg, 2008 equation with a k fixed at 2
FitMeanCurves(apt[sample(apt$id, 5), ], "hs", k = 2, method = "Mean")
#> Warning: `FitMeanCurves()` was deprecated in beezdemand 0.2.0.
#> ℹ Please use the `agg` argument of `fit_demand_fixed()` instead.
#> ℹ Use fit_demand_fixed(data, agg = 'Mean') or fit_demand_fixed(data, agg =
#>   'Pooled')
#>   for the modern interface with summary(), tidy(), glance(), and predict().
#> ℹ See vignette('migration-guide') for migration instructions.
#>     id Intensity BP0 BP1 Omaxe Pmaxe Equation      Q0d K          R2
#> 1 Mean         3  NA 2.5  13.5   1.5       hs 4.414472 2 0.003999712
#>          Alpha     Q0se    Alphase N     AbsSS     SdRes     Q0Low   Q0High
#> 1 -0.002217055 5.765361 0.03808427 3 0.1219393 0.3491981 -68.84139 77.67033
#>     AlphaLow AlphaHigh        EV     Omaxd     Pmaxd     Omaxa     Pmaxa
#> 1 -0.4861235 0.4816894 -1.594699 -41.03392 -29.47746 -41.03448 -29.65978
#>       Notes
#> 1 converged
```
