# Get Shared K

Finds shared k among selected datasets using global regression

## Usage

``` r
GetSharedK(dat, equation, sharecol = "group")
```

## Arguments

- dat:

  Dataframe (longform)

- equation:

  Character vector. Accepts either "hs" or "koff"

- sharecol:

  Character for column to find shared k. Default to "group" but can loop
  based on id.

## Value

Numeric value of shared k

## Details

Uses global regression to fit a shared k among datasets. Assumes the
dataset is in its final form. Used within FitCurves

## Author

Brent Kaplan <bkaplan.ku@gmail.com> Shawn P Gilroy
<shawn.gilroy@temple.edu>

## Examples

``` r
## Find a shared k value across datasets indicated by id
# \donttest{
GetSharedK(apt, "hs", sharecol = "id")
#> Beginning search for best-starting k
#> Best k fround at 0.93813356574003 = err: 0.744881846162718
#> Searching for shared K, this can take a while...
#>       k 
#> 3.31833 
# }
```
