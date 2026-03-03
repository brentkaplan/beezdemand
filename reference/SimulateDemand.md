# Simulate Demand Data

Simulate demand data

## Usage

``` r
SimulateDemand(nruns = 10, setparams, sdindex, x, outdir = NULL, fn = NULL)
```

## Arguments

- nruns:

  Number of runs. Default value is 10

- setparams:

  A 6x1 matrix (or 6 element vector) containing (in order) mean
  log10alpha, sd log10alpha, mean log10q0, sd log10q0, k, sd of
  consumption values across all prices

- sdindex:

  A vector of n length of sd consumption values for n prices

- x:

  A vector of n prices

- outdir:

  Optional. Directory to save results. Must end with a "/"

- fn:

  Optional. Filename of saved RData object

## Value

Invisibly a list consisting of: rounded consumption values, unrounded
consumption values, simulation parameters, and inState and outState of
seeds.

## Details

Generates and saves simulated datasets in the manner specified in
Koffarnus, Franck, Stein, & Bickel (2015).

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## set values
setparams <- vector(length = 4)
setparams <- c(-2.5547, .702521, 1.239893, .320221, 3.096, 1.438231)
names(setparams) <- c("alphalm", "alphalsd", "q0lm", "q0lsd", "k", "yvalssd")
sdindex <- c(2.1978, 1.9243, 1.5804, 1.2465, 0.8104, 0.1751, 0.0380, 0.0270)
x <- c(.1, 1, 3, 10, 30, 100, 300, 1000)
set.seed(1234)
sim <- SimulateDemand(nruns = 1, setparams = setparams, sdindex = sdindex, x = x)
sim
#> $simvalues
#>   id     x  y
#> 1  1 1e-01 25
#> 2  1 1e+00 14
#> 3  1 3e+00 19
#> 4  1 1e+01 13
#> 5  1 3e+01  4
#> 6  1 1e+02  0
#> 7  1 3e+02  0
#> 8  1 1e+03  0
#> 
#> $simvaluesraw
#>   id     x             y
#> 1  1 1e-01 24.6173989397
#> 2  1 1e+00 13.5864831879
#> 3  1 3e+00 18.8146915275
#> 4  1 1e+01 12.8834104715
#> 5  1 3e+01  3.6613886149
#> 6  1 1e+02  0.2294081472
#> 7  1 3e+02 -0.0006984269
#> 8  1 1e+03 -0.0174460736
#> 
#> $simparams
#>     alphalr     q0lr       alphar      q0r
#> 1 -3.402689 1.328732 0.0003956498 21.31727
#> 
#> $seeds
#>   inState     outState   
#> 1 integer,626 integer,626
#> 
```
