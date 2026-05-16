# Variance-covariance matrix for a beezdemand_tmb fit

Returns the fixed-effect VCOV from the TMB sdreport, i.e., the inverse
of the negative Hessian at the MLE after Laplace-marginalizing the
random effects. Row/column names follow the optimizer's internal
parameterization (matching `names(coef(object, type = "internal"))`).

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
vcov(object, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Unused.

## Value

Numeric symmetric matrix of dimension p x p.

## See also

[`coef.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/coef.beezdemand_tmb.md),
[`confint.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/confint.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
V <- vcov(fit)
isSymmetric(V)
#> [1] TRUE
# }
```
