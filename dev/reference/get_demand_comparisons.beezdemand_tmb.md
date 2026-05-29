# Get Demand Parameter Comparisons for TMB Model

Computes factor-level contrasts for demand parameters from a
`beezdemand_tmb` model. Returns a classed `beezdemand_comparison` object
(the same container the NLME backend returns), so
[`tidy.beezdemand_comparison()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_comparison.md)
gives a backend-agnostic flat frame.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
get_demand_comparisons(
  fit_obj,
  param = c("Q0", "alpha"),
  compare_specs = NULL,
  contrast_type = c("pairwise", "trt.vs.ctrl"),
  contrast_by = NULL,
  adjust = "holm",
  at = NULL,
  ci_level = 0.95,
  report_ratios = TRUE,
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_tmb` object.

- param:

  Character vector. Which parameter(s) to compare: any of `"Q0"`,
  `"alpha"`. Default `c("Q0", "alpha")` (both).

- compare_specs:

  Optional one-sided formula naming the factor subset to contrast (e.g.
  `~ gender`). Omitted fitted factors are marginalized over with equal
  weights across the full crossing of their levels (matching the NLME
  backend). If `NULL` (default), all fitted factors are retained. Under
  asymmetric `collapse_levels`, name the **original** factor (e.g.
  `~ age_group`); it resolves to that parameter's collapsed column
  (`age_group_Q0` / `age_group_alpha`), as on the NLME backend.

- contrast_type:

  Character. `"pairwise"` (all pairs, factor-level order) or
  `"trt.vs.ctrl"` (each level vs. the first/reference level).

- contrast_by:

  Optional `NULL` (default) or character vector of factor name(s) within
  `compare_specs` to condition the contrasts on. Within each observed
  combination of by-level(s), pairwise (or `trt.vs.ctrl`) contrasts are
  computed over the remaining (non-by) factors, with p-value adjustment
  applied **per by-cell**. The by-variable(s) must be named in
  `compare_specs` (per parameter after collapse-mapping); a
  `contrast_by` factor absent from `compare_specs` aborts. Unlike
  `compare_specs` (which aborts on an unresolvable name), `contrast_by`
  **soft-skips** a parameter for which it does not resolve under
  asymmetric `collapse_levels`. Numeric results match the NLME backend
  in shape and direction (TMB uses asymptotic *z* vs. NLME's *t*).
  Continuous covariates are held at the global training mean within
  by-levels (the same convention as `at`), not recomputed per by-level.
  Multi-by is accepted but currently untested on TMB (its two-factor
  fixed-effect cap precludes a `compare_specs` with the 3+ factors a
  multi-by Cartesian would require).

- adjust:

  Character. P-value adjustment method; must be one of
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html)
  (default `"holm"`). emmeans-only methods such as `"tukey"`/`"sidak"`
  are rejected (the TMB backend uses asymptotic z +
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)).

- at:

  Named list specifying factor levels and/or continuous-covariate values
  to condition on, as in
  [`get_demand_param_emms.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.beezdemand_tmb.md).

- ci_level:

  Numeric. Confidence level for intervals. Default 0.95.

- report_ratios:

  Logical. If `TRUE` (default), include a `contrasts_ratio` block
  (multiplicative ratios) per parameter.

- ...:

  Additional arguments (reserved; `factors_in_emm` is accepted as a
  lower-level alternative to `compare_specs`).

## Value

A `beezdemand_comparison` object: a list named by parameter, each
element a list with `emmeans` (native cell means), `contrasts_log10`
(log10-scale contrasts with `contrast`, `estimate`, `std.error`,
`statistic`, `df`, `conf.low`, `conf.high`, `p.value`), and (if
`report_ratios`) `contrasts_ratio`. When `contrast_by` is active, the
contrast tables gain leading by-column(s) (user-requested original
names) before `contrast`. Attributes `backend`, `adjustment_method`,
`compare_specs_used`, `contrast_type_used`, `contrast_by_used`, and
`contrast_by_map` (per-parameter original -\> effective by-name map)
describe the call.

## See also

[`tidy.beezdemand_comparison()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_comparison.md)
for the backend-agnostic frame.

## Examples

``` r
# \donttest{
data(apt_full)
dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
fit <- fit_demand_tmb(dat, equation = "exponential",
                      factors = "gender", verbose = 0)
#>   equation='exponential': Dropped 5839 zero-consumption observations (12827 remaining).
#> Warning: NaNs produced
#> Warning: NaNs produced
res <- get_demand_comparisons(fit, param = "Q0")
tidy(res)
#> # A tibble: 1 × 9
#>   param contrast   estimate std.error statistic    df conf.low conf.high p.value
#>   <chr> <chr>         <dbl>     <dbl>     <dbl> <dbl>    <dbl>     <dbl>   <dbl>
#> 1 Q0    Female - …   -0.105    0.0177     -5.89   Inf   -0.139   -0.0697 3.84e-9
# }
```
