# Get Pairwise Comparisons for Demand Parameters

Conducts pairwise comparisons for Q0 and/or alpha parameters from a
`beezdemand_nlme` model across levels of specified factors. Comparisons
are performed on the log10 scale of the parameters. Results include
estimates of differences (on log10 scale) and optionally, ratios (on the
natural scale by applying 10^difference).

## Usage

``` r
get_demand_comparisons(fit_obj, ...)

# Default S3 method
get_demand_comparisons(fit_obj, ...)

# S3 method for class 'beezdemand_nlme'
get_demand_comparisons(
  fit_obj,
  param = c("Q0", "alpha"),
  compare_specs = NULL,
  contrast_type = "pairwise",
  contrast_by = NULL,
  adjust = "holm",
  at = NULL,
  ci_level = 0.95,
  report_ratios = TRUE,
  params_to_compare = lifecycle::deprecated(),
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_nlme` object.

- ...:

  Additional arguments passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  or
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- param:

  Character vector: "Q0", "alpha", or `c("Q0", "alpha")`. Default
  `c("Q0", "alpha")` (both). This is the canonical argument name, shared
  with the TMB backend
  ([`get_demand_comparisons.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.beezdemand_tmb.md)).

- compare_specs:

  A formula specifying the factors whose levels are to be included in
  the EMM calculation prior to contrasting. This defines the "cells" of
  your design for EMMs. E.g., `~ factor1` (EMMs for levels of factor1,
  averaging over others), `~ factor1 * factor2` (EMMs for all cells of
  factor1 x factor2). If `NULL`, it defaults to an interaction of all
  factors in `fit_obj$param_info$factors`.

- contrast_type:

  Character string specifying the type of contrast (passed to `method`
  in
  [`emmeans::contrast`](https://rvlenth.github.io/emmeans/reference/contrast.html)).
  Commonly `"pairwise"`, `"revpairwise"`, `"eff"`, `"consec"`, `"poly"`.
  Default `"pairwise"`.

- contrast_by:

  Optional character vector of factor names to condition the contrasts
  by (passed to `by` in
  [`emmeans::contrast`](https://rvlenth.github.io/emmeans/reference/contrast.html)).
  If `NULL` (default), contrasts are performed over the primary terms
  implied by `compare_specs` and `contrast_type`. Example: If
  `compare_specs = ~ dose * drug`, `contrast_type = "pairwise"`, and
  `contrast_by = "dose"`, this will perform pairwise comparisons of
  `drug` levels within each level of `dose`. **Note:** If the original
  `fit_obj` model is additive for the factors involved (i.e., no
  interaction term was fitted), specifying `contrast_by` will result in
  identical contrast estimates across the levels of the `contrast_by`
  variable(s). In such cases, consider analyzing main effects directly
  (e.g., `compare_specs = ~drug`, `contrast_by = NULL`).

- adjust:

  P-value adjustment method. Default `"holm"` (changed from `"tukey"` in
  0.3.0 for cross-backend reproducibility; pass `adjust = "tukey"` to
  retain the previous default).

- at:

  Optional named list for
  [`emmeans::ref_grid()`](https://rvlenth.github.io/emmeans/reference/ref_grid.html).

- ci_level:

  Confidence level. Default 0.95.

- report_ratios:

  Logical. If TRUE, reports contrasts as ratios. Default `TRUE`.

- params_to_compare:

  **\[deprecated\]** Use `param` instead (deprecated in 0.3.0 to
  harmonize with the TMB backend).

## Value

A list named by parameter. Each element contains:

- emmeans:

  Tibble of EMMs (log10 scale) with CIs.

- contrasts_log10:

  Tibble of comparisons (log10 differences) with CIs and p-values.

- contrasts_ratio:

  (If `report_ratios=TRUE` and successful) Tibble of comparisons as
  ratios (natural scale), with CIs for ratios.

S3 class `beezdemand_comparison` is assigned. When `contrast_by` is
active, the nested contrast tables carry leading by-column(s) named with
the user-requested *original* factor name (e.g. `dose`, not the
collapse-mapped `dose_alpha`), harmonized with the TMB backend and the
flat
[tidy()](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_comparison.md)
output (TICKET-033).

## Examples

``` r
# \donttest{
data(ko, package = "beezdemand")
ko$y_ll4 <- ll4(ko$y, lambda = 4)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
  id_var = "monkey", factors = "dose", equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 10 (Q0: 5, alpha: 5)
get_demand_comparisons(fit)
#> Using default 'compare_specs': ~ dose for EMMs.
#> 
#> --- Processing comparisons for parameter: Q0 ---
#> 
#> --- Processing comparisons for parameter: alpha ---
#> Demand Parameter Comparisons (nlme backend)
#> EMMs computed over: ~dose 
#> Contrast type: pairwise
#> P-value adjustment method: holm 
#> ================================================== 
#> 
#> Q0 (log10-scale contrasts):
#>                   contrast estimate std.error conf.low conf.high p.value
#>  (dose3e-05) - (dose1e-04)    0.199     0.136   -0.189     0.588   0.290
#>  (dose3e-05) - (dose3e-04)    0.363     0.129   -0.004     0.731   0.022
#>    (dose3e-05) - dose0.001    0.670     0.131    0.295     1.046   0.000
#>    (dose3e-05) - dose0.003    0.675     0.145    0.261     1.089   0.000
#>  (dose1e-04) - (dose3e-04)    0.164     0.091   -0.097     0.425   0.226
#>    (dose1e-04) - dose0.001    0.471     0.095    0.199     0.743   0.000
#>    (dose1e-04) - dose0.003    0.476     0.113    0.153     0.799   0.000
#>    (dose3e-04) - dose0.001    0.307     0.084    0.067     0.548   0.002
#>    (dose3e-04) - dose0.003    0.312     0.104    0.015     0.609   0.016
#>      dose0.001 - dose0.003    0.004     0.107   -0.302     0.311   0.967
#> 
#> alpha (log10-scale contrasts):
#>                   contrast estimate std.error conf.low conf.high p.value
#>  (dose3e-05) - (dose1e-04)    0.071     0.092   -0.193     0.336       1
#>  (dose3e-05) - (dose3e-04)    0.015     0.088   -0.235     0.265       1
#>    (dose3e-05) - dose0.001    0.042     0.094   -0.226     0.310       1
#>    (dose3e-05) - dose0.003    0.086     0.109   -0.224     0.396       1
#>  (dose1e-04) - (dose3e-04)   -0.056     0.065   -0.243     0.131       1
#>    (dose1e-04) - dose0.001   -0.030     0.073   -0.240     0.180       1
#>    (dose1e-04) - dose0.003    0.015     0.092   -0.247     0.276       1
#>    (dose3e-04) - dose0.001    0.027     0.067   -0.165     0.219       1
#>    (dose3e-04) - dose0.003    0.071     0.087   -0.176     0.319       1
#>      dose0.001 - dose0.003    0.044     0.093   -0.221     0.310       1
# }
```
