# Get Values for SimulateDemand

Gets values used in SimulateDemand

## Usage

``` r
GetValsForSim(dat)
```

## Arguments

- dat:

  Dataframe (long form)

## Value

List of 3: setaparams, sdindex, x

## Details

Gets values used in SimulateDemand

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
GetValsForSim(apt)
#> $setparams
#>    alphalm   alphalsd       q0lm      q0lsd          k    yvalssd 
#> -2.0264070  0.2188239  0.8339768  0.1771590  1.0000000  2.5333002 
#> 
#> $sdindex
#>  [1] 0.3265536 0.3491036 0.3885667 0.5468517 0.3535432 0.4704263 0.5011136
#>  [8] 0.3380656 0.4938148 0.3827152 0.3481552 0.6195088 0.5778922 0.5048588
#> [15] 1.0094167 0.9725017
#> 
#> $x
#>  [1]  0.0  0.5  1.0  1.5  2.0  2.5  3.0  4.0  5.0  6.0  7.0  8.0  9.0 10.0 15.0
#> [16] 20.0
#> 
```
