# FitCurves

Analyzes purchase task data

## Usage

``` r
FitCurves(
  dat,
  equation,
  k,
  agg = NULL,
  detailed = FALSE,
  xcol = "x",
  ycol = "y",
  idcol = "id",
  groupcol = NULL,
  lobound,
  hibound,
  constrainq0 = NULL,
  startq0 = NULL,
  startalpha = NULL,
  param_space = c("natural", "log10")
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

  A numeric (or character) vector of length one. Reflects the range of
  consumption in log10 units. If none provided, k will be calculated
  based on the max/min of the entire sample + .5. If k = "ind", k will
  be calculated per individual using max/min + .5. If k = "fit", k will
  be a free parameter on an individual basis. If k = "range", k will be
  calculated based on the max/min of the entire sample + .5.

- agg:

  Character vector of length one accepts either "Mean" or "Pooled". If
  not NULL (default), data will be aggregrated appropriately and
  analyzed in the specified way.

- detailed:

  If TRUE, output will be a 3 element list including (1) dataframe of
  results, (2) list of model objects, (3) list of individual dataframes
  used in fitting. Default value is FALSE, which returns only the
  dataframe of results.

- xcol:

  The column name that should be treated as "x" data

- ycol:

  The column name that should be treated as "y" data

- idcol:

  The column name that should be treated as dataset identifier

- groupcol:

  The column name that should be treated as the groups

- lobound:

  Optional. A named vector of length 2 ("q0", "alpha") or 3 ("q0", "k",
  "alpha"), the latter length if k = "fit", specifying the lower bounds.

- hibound:

  Optional. A named vector of length 2 ("q0", "alpha") or 3 ("q0", "k",
  "alpha"), the latter length if k = "fit", specifying the upper bounds.

- constrainq0:

  Optional. A number that will be used to constrain Q0 in the fitting
  process. Currently experimental and only works with a fixed k value.

- startq0:

  Optional. A number that will be used to start Q0 in the fitting
  process. Currently experimental.

- startalpha:

  Optional. A number that will be used to start Alpha in the fitting
  process. Currently experimental.

- param_space:

  Character. One of "natural" (default) or "log10". Specifies whether
  parameters (Q0, alpha) are estimated in natural space or
  log10-transformed space.

## Value

If detailed == FALSE (default), a dataframe of results. If detailed ==
TRUE, a 3 element list consisting of (1) dataframe of results, (2) list
of model objects, (3) list of individual dataframes used in fitting

## Details

**\[superseded\]**

`FitCurves()` has been superseded by
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md),
which provides a modern S3 interface with standardized methods
([`summary()`](https://rdrr.io/r/base/summary.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html),
[`predict()`](https://rdrr.io/r/stats/predict.html)). `FitCurves()` will
continue to work but is no longer recommended for new code. See
[`vignette("migration-guide")`](https://brentkaplan.github.io/beezdemand/articles/migration-guide.md)
for migration instructions.

## See also

[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
for the modern interface

## Author

Brent Kaplan <bkaplan.ku@gmail.com> Shawn Gilroy
<shawn.gilroy@temple.edu>

## Examples

``` r
## Analyze using Hursh & Silberberg, 2008 equation with a k fixed to 2
FitCurves(apt[sample(apt$id, 5), ], "hs", k = 2)
#> Warning: `FitCurves()` was deprecated in beezdemand 0.2.0.
#> ℹ Please use `fit_demand_fixed()` instead.
#> ℹ FitCurves() returns raw data frames; fit_demand_fixed() returns a
#>   structured S3 object with summary(), tidy(), glance(), and predict().
#> ℹ See vignette('migration-guide') for migration instructions.
#> Error in GetEmpirical(dat): Duplicates found where id = 68. Check data and rerun.
```
