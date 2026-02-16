# Systematic Purchase Task Data Checker

Applies Stein, Koffarnus, Snider, Quisenberry, & Bickel's (2015)
criteria for identification of nonsystematic purchase task data.

## Usage

``` r
CheckUnsystematic(dat, deltaq = 0.025, bounce = 0.1, reversals = 0, ncons0 = 2)
```

## Arguments

- dat:

  Dataframe in long form. Colums are id, x, y.

- deltaq:

  Numeric vector of length equal to one. The criterion by which the
  relative change in quantity purchased will be compared. Relative
  changes in quantity purchased below this criterion will be flagged.
  Default value is 0.025.

- bounce:

  Numeric vector of length equal to one. The criterion by which the
  number of price-to-price increases in consumption that exceed 25% of
  initial consumption at the lowest price, expressed relative to the
  total number of price increments, will be compared. The relative
  number of price-to-price increases above this criterion will be
  flagged. Default value is 0.10.

- reversals:

  Numeric vector of length equal to one. The criterion by which the
  number of reversals from number of consecutive (see ncons0) 0s will be
  compared. Number of reversals above this criterion will be flagged.
  Default value is 0.

- ncons0:

  Numer of consecutive 0s prior to a positive value is used to flag for
  a reversal. Value can be either 1 (relatively more conservative) or 2
  (default; as recommended by Stein et al., (2015).

## Value

Dataframe

## Details

This function applies the 3 criteria proposed by Stein et al., (2015)
for identification of nonsystematic purchase task data. The three
criteria include trend (deltaq), bounce, and reversals from 0. Also
reports number of positive consumption values.

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## Using all default values
CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 2)
#>     id TotalPass DeltaQ DeltaQPass Bounce BouncePass Reversals ReversalsPass
#> 1   19         3 0.2112       Pass      0       Pass         0          Pass
#> 2   30         3 0.1437       Pass      0       Pass         0          Pass
#> 3   38         3 0.7885       Pass      0       Pass         0          Pass
#> 4   60         3 0.9089       Pass      0       Pass         0          Pass
#> 5   68         3 0.9089       Pass      0       Pass         0          Pass
#> 6  106         3 0.8178       Pass      0       Pass         0          Pass
#> 7  113         3 0.1441       Pass      0       Pass         0          Pass
#> 8  142         3 0.1288       Pass      0       Pass         0          Pass
#> 9  156         3 0.8620       Pass      0       Pass         0          Pass
#> 10 188         3 0.8178       Pass      0       Pass         0          Pass
#>    NumPosValues
#> 1            16
#> 2            16
#> 3            14
#> 4            14
#> 5            14
#> 6            11
#> 7            16
#> 8            16
#> 9            15
#> 10           14
## Specifying just 1 zero to flag as reversal
CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 1)
#>     id TotalPass DeltaQ DeltaQPass Bounce BouncePass Reversals ReversalsPass
#> 1   19         3 0.2112       Pass      0       Pass         0          Pass
#> 2   30         3 0.1437       Pass      0       Pass         0          Pass
#> 3   38         3 0.7885       Pass      0       Pass         0          Pass
#> 4   60         3 0.9089       Pass      0       Pass         0          Pass
#> 5   68         3 0.9089       Pass      0       Pass         0          Pass
#> 6  106         3 0.8178       Pass      0       Pass         0          Pass
#> 7  113         3 0.1441       Pass      0       Pass         0          Pass
#> 8  142         3 0.1288       Pass      0       Pass         0          Pass
#> 9  156         3 0.8620       Pass      0       Pass         0          Pass
#> 10 188         3 0.8178       Pass      0       Pass         0          Pass
#>    NumPosValues
#> 1            16
#> 2            16
#> 3            14
#> 4            14
#> 5            14
#> 6            11
#> 7            16
#> 8            16
#> 9            15
#> 10           14
```
