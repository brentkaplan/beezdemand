# Recode Outliers

Recodes outliers

## Usage

``` r
RecodeOutliers(df, outval = 3.29, unitshigher = 0)
```

## Arguments

- df:

  A dataframe of consumption values

- outval:

  Values greater/less than or equal to this number (specified in
  standard deviations) will be recoded. Default is 3.29SD as specified
  by Tabachnick and Fidell (2013)

- unitshigher:

  Outliers identified by outval will be coded to a certain number of
  units higher/lower than the greatest nonoutlier value. Default is 0
  units.

## Value

Invisibly, a dataframe with original and recoded (if any) values

## Details

Recodes outliers using Tabachnick and Fidell's (2013) criteria. A
variable is standardized and values that are greater/less than or equal
to a specified outlier value (specified in standard deviations; default
3.29SD) are recoded to a certain number of units (default 0)
higher/lower than the greatest nonoutlier value. Disregards 'NA' values.

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## If any outliers are detected, they would be coded as 1 unit higher
# \donttest{
emp <- GetEmpirical(apt)
RecodeOutliers(emp[, c(2:6)], unitshigher = 1)
#> [1] "No outliers detected in column: Intensity."
#> [1] "No outliers detected in column: BP0."
#> [1] "No outliers detected in column: BP1."
#> [1] "No outliers detected in column: Omaxe."
#> [1] "No outliers detected in column: Pmaxe."
#> [1] "A total of 0 outlying values were replaced"
# }
```
