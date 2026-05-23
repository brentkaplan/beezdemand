# Population-level demand metrics for a mixed-effects NLME fit

Computes parameter-first-marginalized Pmax, Omax, Qmax, and
elasticity-at-Pmax for a
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
model, mirroring the return contract of
[`calc_group_metrics()`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md)
for `beezdemand_tmb` fits: a flat scalar list, NOT a tibble.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
calc_group_metrics(object, at = NULL, ...)
```

## Arguments

- object:

  A `beezdemand_nlme` object from
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md).

- at:

  Optional named list conditioning continuous covariates / factor levels
  (same shape as the `beezdemand_tmb` method). Covariates default to
  their training mean; factors are marginalized with equal weights
  unless a level is supplied.

- ...:

  Unused.

## Value

A flat list with scalar `Pmax`, `Omax`, `Qmax`, `elasticity_at_pmax`,
character `method`, and `conditioned_on` (a list of `$covariates` and/or
`$factors`, or `NULL` when the fit has neither).

## Details

Fixed-effect log-Q0 and log-alpha estimated marginal means are averaged
across the reference grid (continuous covariates at their training mean
by default, factor levels equally weighted) on the natural scale (a
geometric mean), then the scalar metrics are derived from the
marginalized parameters via
[`beezdemand_calc_pmax_omax()`](https://brentkaplan.github.io/beezdemand/reference/beezdemand_calc_pmax_omax.md).
`model_type` follows the equation form: `"exponentiated"` (which carries
a range parameter `k`) uses the Hursh & Silberberg solution;
`"zben"`/`"simplified"` use the simplified (SND) solution.

## See also

[`calc_group_metrics()`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md),
[`get_demand_param_emms()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.md)

## Examples

``` r
# \donttest{
data(apt_full, package = "beezdemand")
apt_full$y_ll4 <- ll4(apt_full$y, lambda = 4)
fit <- fit_demand_mixed(
  apt_full, equation_form = "zben", factors = "gender",
  y_var = "y_ll4", x_var = "x", id_var = "id")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=0.699, alpha_int=-3
#> Number of fixed parameters: 6 (Q0: 3, alpha: 3)
calc_group_metrics(fit)
#> $Pmax
#> [1] 5.67404
#> 
#> $Omax
#> [1] 8.97433
#> 
#> $Qmax
#> [1] 1.581647
#> 
#> $elasticity_at_pmax
#> [1] -1
#> 
#> $method
#> [1] "analytic_snd"
#> 
#> $conditioned_on
#> $conditioned_on$factors
#> $conditioned_on$factors$gender
#> [1] "marginal"
#> 
#> 
#> 
calc_group_metrics(fit, at = list(gender = "Male"))
#> $Pmax
#> [1] 8.453721
#> 
#> $Omax
#> [1] 19.98975
#> 
#> $Qmax
#> [1] 2.364609
#> 
#> $elasticity_at_pmax
#> [1] -1
#> 
#> $method
#> [1] "analytic_snd"
#> 
#> $conditioned_on
#> $conditioned_on$factors
#> $conditioned_on$factors$gender
#> [1] "Male"
#> 
#> 
#> 
# }
```
