# Update a beezdemand_tmb fit

Re-fits with named arguments substituted into the original call. Pass
any argument of
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
(e.g., `factors = NULL`, `random_effects = ~ 1`,
`equation = "simplified"`). Does NOT support formula-update syntax
(`. - term`) because
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
is argument-driven, not formula-driven.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
update(object, ..., evaluate = TRUE)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Named arguments to substitute into the original
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  call.

- evaluate:

  If `TRUE` (default), re-evaluate the updated call and return the new
  fit. If `FALSE`, return the unevaluated call.

## Value

A new `beezdemand_tmb` object, or an unevaluated call.

## See also

[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md),
[`formula.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/formula.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
update(fit, equation = "simplified", evaluate = FALSE)
#> fit_demand_tmb(data = apt, equation = "simplified", verbose = 0)
# }
```
