# Calculate Population-Level Demand Metrics for TMB Model

Calculate Population-Level Demand Metrics for TMB Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
calc_group_metrics(object, at = NULL, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- at:

  Named list of factor-level filters or continuous-covariate value
  overrides (e.g. `list(condition = "C1", FTND_z = 0.5)`). When `NULL`
  (default), continuous covariates are evaluated at their training mean
  and factors are marginalized across observed levels (equal weights).
  When supplied, conditions the parameter EMMs to the specified factor
  levels and/or covariate values before deriving Pmax/Omax. Same shape
  as the `at` argument of
  [`get_demand_param_emms.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.beezdemand_tmb.md)
  and
  [`get_demand_comparisons.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.beezdemand_tmb.md).

- ...:

  Additional arguments (currently unused).

## Value

A list with `Pmax`, `Omax`, `Qmax`, `elasticity_at_pmax`, `method`, and
`conditioned_on` describing the reference point used. The
`conditioned_on` field reports the actual conditioning applied
(covariate values used, factor treatment per factor) so programmatic
consumers do not have to re-derive it.

## Marginalization order

For derived metrics (Pmax/Omax/Qmax) that depend nonlinearly on `Q0` and
`alpha` jointly, this function marginalizes parameters first then
derives metrics:

1.  Compute log-Q0 and log-alpha EMMs at each cell of the reference grid
    produced by `.tmb_build_emm_ref_grid()`.

2.  Marginalize each parameter across factor cells with equal weights
    (matches the emmeans default).

3.  Derive Pmax/Omax/Qmax from the marginalized log-parameters at the
    user-supplied (or training-mean default) covariate point.

The result is "metrics evaluated at the average parameter values" rather
than "average metrics across cells". The two answers differ for
nonlinear transforms. The convention matches the parameter-level
marginalization used by
[`get_demand_param_emms()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.md).

## See also

[`fit_demand_tmb`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md),
[`get_demand_param_emms.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.beezdemand_tmb.md)

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
calc_group_metrics(fit)
#> $Pmax
#> [1] 11.23768
#> 
#> $Omax
#> [1] 23.89412
#> 
#> $Qmax
#> [1] 2.126251
#> 
#> $elasticity_at_pmax
#> [1] -1
#> 
#> $method
#> [1] "analytic_lambert_w"
#> 
#> $pmax_at_bound
#> [1] FALSE
#> 
#> $conditioned_on
#> NULL
#> 
# Conditioned at a specific covariate value:
# calc_group_metrics(fit_with_cov, at = list(FTND_z = 1))
# }
```
