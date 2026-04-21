# Augment a beezdemand_tmb Model

Augment a beezdemand_tmb Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
augment(x, newdata = NULL, ...)
```

## Arguments

- x:

  A `beezdemand_tmb` object.

- newdata:

  Optional data frame.

- ...:

  Additional arguments.

## Value

A tibble with original data plus `.fitted`, `.resid`, and `.std_resid`
columns. Residuals are computed on the model's native scale (log scale
for `"exponential"`, natural/LL4 scale for others) to match the C++
likelihood.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(augment(fit))
#> # A tibble: 6 × 6
#>   id        x     y .fitted   .resid .std_resid
#>   <fct> <dbl> <dbl>   <dbl>    <dbl>      <dbl>
#> 1 19      0      10    2.31 -0.00581    -0.0408
#> 2 19      0.5    10    2.26  0.0404      0.284 
#> 3 19      1      10    2.22  0.0863      0.606 
#> 4 19      1.5     8    2.17 -0.0914     -0.642 
#> 5 19      2       8    2.13 -0.0463     -0.325 
#> 6 19      2.5     8    2.08 -0.00156    -0.0110
# }
```
