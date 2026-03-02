# Analytic Pmax Fallback

Fallback method for Analytic Pmax

## Usage

``` r
GetAnalyticPmaxFallback(K_, A_, Q0_)
```

## Arguments

- K\_:

  k parameter

- A\_:

  alpha parameter

- Q0\_:

  q0 parameter

## Value

numeric

## Details

Derivative-based optimization strategy

## Author

Shawn Gilroy <sgilroy1@lsu.edu>

## Examples

``` r
GetAnalyticPmaxFallback(K_ = 1, A_ = 0.001, Q0_ = 10)
#> [1] 99.99979
```
