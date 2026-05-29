# Formula for a beezdemand_tmb fit

Returns the fixed-effect RHS formulas for Q0 and alpha plus the original
random-effect specification preserved at fit time. The Q0 and alpha
formulas may differ when `collapse_levels` was used to collapse factor
levels asymmetrically.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
formula(x, ...)
```

## Arguments

- x:

  A `beezdemand_tmb` object.

- ...:

  Unused.

## Value

Named list `list(Q0, alpha, random)`. `Q0` and `alpha` are one-sided
formulas built from `fit$formula_details`. `random` is the original
`random_effects` argument value (round-trippable back to
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)).

## See also

[`model.matrix.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/model.matrix.beezdemand_tmb.md),
[`update.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/update.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
formula(fit)
#> $Q0
#> ~1
#> <environment: 0x56161c648a70>
#> 
#> $alpha
#> ~1
#> <environment: 0x56161c648a70>
#> 
#> $random
#> Q0 + alpha ~ 1
#> <environment: 0x5616395126a0>
#> 
# }
```
