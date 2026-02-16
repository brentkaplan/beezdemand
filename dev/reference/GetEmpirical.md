# GetEmpirical

Calculates empirical measures for purchase task data

## Usage

``` r
GetEmpirical(dat, xcol = "x", ycol = "y", idcol = "id")
```

## Arguments

- dat:

  data frame (long form) of purchase task data.

- xcol:

  The column name that should be treated as "x" data

- ycol:

  The column name that should be treated as "y" data

- idcol:

  The column name that should be treated as dataset identifier

## Value

Data frame of empirical measures

## Details

**\[superseded\]**

`GetEmpirical()` has been superseded by
[`get_empirical_measures()`](https://brentkaplan.github.io/beezdemand/reference/get_empirical_measures.md),
which provides a modern S3 interface with standardized methods
([`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html)).
`GetEmpirical()` will continue to work but is no longer recommended for
new code.

Will calculate and return the following empirical measures: Intensity,
BP0, BP1, Omax, and Pmax

## See also

[`get_empirical_measures()`](https://brentkaplan.github.io/beezdemand/reference/get_empirical_measures.md)
for the modern interface

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## Obtain empirical measures
GetEmpirical(apt)
#> Warning: `GetEmpirical()` was deprecated in beezdemand 0.3.0.
#> ℹ Please use `get_empirical_measures()` instead.
#>     id Intensity BP0 BP1 Omaxe Pmaxe
#> 1   19        10  NA  20    45    15
#> 2   30         3  NA  20    20    20
#> 3   38         4  15  10    21     7
#> 4   60        10  15  10    24     8
#> 5   68        10  15  10    36     9
#> 6  106         5   8   7    15     5
#> 7  113         6  NA  20    45    15
#> 8  142         8  NA  20    60    20
#> 9  156         7  20  15    21     7
#> 10 188         5  15  10    15     5
```
