# Get Pairwise Comparisons for Demand Parameters

Conducts pairwise comparisons for Q0 and/or alpha parameters from a
`beezdemand_nlme` model across levels of specified factors. Comparisons
are performed on the log10 scale of the parameters. Results include
estimates of differences (on log10 scale) and optionally, ratios (on the
natural scale by applying 10^difference).

## Usage

``` r
get_demand_comparisons(
  fit_obj,
  params_to_compare = c("Q0", "alpha"),
  compare_specs = NULL,
  contrast_type = "pairwise",
  contrast_by = NULL,
  adjust = "tukey",
  at = NULL,
  ci_level = 0.95,
  report_ratios = TRUE,
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_nlme` object.

- params_to_compare:

  Character vector: "Q0", "alpha", or `c("Q0", "alpha")`. Default
  `c("Q0", "alpha")`.

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

  P-value adjustment method. Default "tukey".

- at:

  Optional named list for
  [`emmeans::ref_grid()`](https://rvlenth.github.io/emmeans/reference/ref_grid.html).

- ci_level:

  Confidence level. Default 0.95.

- report_ratios:

  Logical. If TRUE, reports contrasts as ratios. Default `TRUE`.

- ...:

  Additional arguments passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  or
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

## Value

A list named by parameter. Each element contains:

- emmeans:

  Tibble of EMMs (log10 scale) with CIs.

- contrasts_log10:

  Tibble of comparisons (log10 differences) with CIs and p-values.

- contrasts_ratio:

  (If `report_ratios=TRUE` and successful) Tibble of comparisons as
  ratios (natural scale), with CIs for ratios.

S3 class `beezdemand_comparison` is assigned.

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
#> Note: adjust = "tukey" was changed to "sidak"
#> because "tukey" is only appropriate for one set of pairwise comparisons
#> Note: adjust = "tukey" was changed to "sidak"
#> because "tukey" is only appropriate for one set of pairwise comparisons
#> 
#> --- Processing comparisons for parameter: alpha ---
#> Note: adjust = "tukey" was changed to "sidak"
#> because "tukey" is only appropriate for one set of pairwise comparisons
#> Note: adjust = "tukey" was changed to "sidak"
#> because "tukey" is only appropriate for one set of pairwise comparisons
#> Demand Parameter Comparisons (from beezdemand_nlme fit)
#> EMMs computed over: ~dose 
#> Contrast type: pairwise
#> P-value adjustment method: tukey 
#> ================================================== 
#> 
# }
```
