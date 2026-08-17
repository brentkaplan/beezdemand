# Fit Fixed-Effect Demand Curves

Modern interface for fitting individual demand curves via nonlinear
least squares. Returns a structured S3 object with standard methods
including [`summary()`](https://rdrr.io/r/base/summary.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html).

## Usage

``` r
fit_demand_fixed(
  data,
  equation = c("hs", "koff", "simplified", "linear", "exponential", "exponentiated"),
  k = 2,
  agg = NULL,
  x_var = "x",
  y_var = "y",
  id_var = "id",
  param_space = c("natural", "log10"),
  by = NULL,
  multistart = TRUE,
  S = NULL,
  ...
)
```

## Arguments

- data:

  Data frame in long format with columns: `id`, `x` (price), `y`
  (consumption).

- equation:

  Character. Equation type: `"hs"` (Hursh & Silberberg, 2008), `"koff"`
  (Koffarnus et al., 2015), `"simplified"` (Rzeszutek et al., 2025;
  simplified exponential with normalized decay, no `k` parameter), or
  `"linear"`. The modern aliases `"exponential"` (equivalent to `"hs"`)
  and `"exponentiated"` (equivalent to `"koff"`) are also accepted.
  Default `"hs"`.

- k:

  Scaling constant. Numeric value (fixed), `"ind"` (individual), `"fit"`
  (free parameter), or `"range"` (data-driven). Default `2`.

- agg:

  Character. Aggregation method: `"Mean"`, `"Pooled"`, or `NULL` for
  individual fits. Default `NULL`.

- x_var:

  Character. Name of the price column. Default `"x"`.

- y_var:

  Character. Name of the consumption column. Default `"y"`.

- id_var:

  Character. Name of the subject identifier column. Default `"id"`.

- param_space:

  Character. Parameterization used for fitting. One of:

  - `"natural"`: fit `Q0`, `alpha` (and `k` if `k = "fit"`) on their
    natural scale

  - `"log10"`: fit `log10(Q0)`, `log10(alpha)` (and `log10(k)` if
    `k = "fit"`)

- by:

  Optional character vector of column names to group by. When supplied,
  fits are run separately within each unique combination of the `by`
  columns. Returns a `beezdemand_fixed_grouped` object with per-group
  child fits. Default `NULL` (no grouping).

- multistart:

  Logical. If `TRUE` (the default), subjects whose production-heuristic
  fit is not strict-converged (`converged_strict`; see Details) are
  automatically re-fit from `S - 1` additional sampled starting values.
  Subjects that strict-converge on the production start are never refit,
  so their results are byte-identical whether `multistart` is `TRUE` or
  `FALSE`. Set to `FALSE` (or `S = 1`) to reproduce the legacy
  single-start behavior exactly. Not applicable to `equation = "linear"`
  (closed-form; never multistarted).

- S:

  Integer or `NULL`. Total number of starts to try per subject
  (including the production start), when `multistart = TRUE`. Default
  `NULL` uses a tiered budget: 8 for 2-parameter forms
  (hs/koff/simplified with a fixed `k`), 32 when `k = "fit"`. Ignored
  for `equation = "linear"`. If supplied, must be a single finite
  integer `>= 1`.

  Note: `multistart` and `S` were added AFTER `by` in the argument list
  (Codex 2F review fold, TICKET-047 item 1) specifically so that
  pre-existing positional calls – e.g.
  `fit_demand_fixed(data, "hs", 2, NULL, "x", "y", "id", "natural", "group_col")`,
  where the 9th positional argument is `by` – continue to bind
  correctly. Always pass `multistart`/`S` by name.

- ...:

  Additional arguments passed to the underlying
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  engine.

## Value

An object of class `beezdemand_fixed` with components:

- results:

  Data frame of fitted parameters for each subject. Gains
  `n_starts_tried`, `n_starts_converged`, and `start_source`
  (`"production"`, `"sampled"`, or `"none"`) from the multi-start
  protocol; see Details.

- fits:

  List of model fit objects (if `detailed = TRUE` internally)

- predictions:

  List of prediction data frames

- data_used:

  List of data frames used for each fit

- multistart:

  List describing the multi-start protocol: `multistart`, `S` (resolved
  budget), `equation`, `eligible` (whether this equation supports
  rescue), and `summary` (per-subject start metadata, or `NULL` when not
  applicable)

- call:

  The original function call

- equation:

  The equation form used

- k_spec:

  Description of k specification

- agg:

  Aggregation method used

- n_total:

  Total number of subjects/fits attempted

- n_success:

  Number of successful fits

- n_fail:

  Number of failed fits

## Details

This function is a modern wrapper around the legacy
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
function. It provides the same fitting capabilities but returns a
structured S3 object with standardized methods for model interrogation.

### Multi-start rescue protocol (TICKET-047)

`fit_demand_fixed()` always runs
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)'s
existing heuristic start exactly as before – the "production start". A
subject whose production fit is strict-converged (`converged_strict`:
the optimizer's own convergence flag AND finite coefficients/objective
AND not sitting on a user-supplied bound) is accepted immediately; no
sampled starts are ever run for it, so its row, fitted model,
predictions, and data are byte-identical to the `multistart = FALSE` /
`S = 1` protocol by construction. Only subjects whose production fit is
NOT strict-converged are re-fit from `S - 1` additional starts, sampled
log-uniformly in interpretable (Q0, Pmax) coordinates and mapped to each
equation's native (Q0, alpha) parameterization via the same closed forms
used by
[`beezdemand_calc_pmax_omax()`](https://brentkaplan.github.io/beezdemand/reference/beezdemand_calc_pmax_omax.md).
Among the sampled attempts that themselves strict-converge, the
minimum-residual-SS start wins (ties broken by draw order). If none of
the sampled starts strict-converge, the original (non-converged)
production row is kept. `equation = "linear"` is a closed-form fit and
is never multistarted.
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
itself is unchanged; sampling draws from the ambient RNG stream (call
[`set.seed()`](https://rdrr.io/r/base/Random.html) before
`fit_demand_fixed()` for reproducibility – the helpers never call
[`set.seed()`](https://rdrr.io/r/base/Random.html) themselves).

## See also

[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
for TMB mixed-effects models,
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
for NLME mixed-effects models,
[`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
for hurdle models.

Other demand-fitting:
[`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md),
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md),
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
print(fit)
#> 
#> Fixed-Effect Demand Model
#> ==========================
#> 
#> Call:
#> fit_demand_fixed(data = apt, equation = "hs", k = 2)
#> 
#> Equation: hs 
#> k: fixed (2) 
#> Subjects: 10 ( 10 converged, 0 failed)
#> 
#> Use summary() for parameter summaries, tidy() for tidy output.
summary(fit)
#> 
#> Fixed-Effect Demand Model Summary
#> ================================================== 
#> 
#> Equation: hs 
#> k: fixed (2) 
#> 
#> Fit Summary:
#>   Total subjects: 10 
#>   Converged: 10 
#>   Failed: 0 
#>   Total observations: 146 
#> 
#> Parameter Summary (across subjects):
#>   Q0:
#>     Median: 6.2498 
#>     Range: [ 2.8074 , 10.3904 ]
#>   alpha:
#>     Median: 0.004251 
#>     Range: [ 0.001987 , 0.00785 ]
#> 
#> Per-subject coefficients:
#> -------------------------
#> # A tibble: 40 × 10
#>    id    term      estimate std.error statistic p.value component estimate_scale
#>    <chr> <chr>        <dbl>     <dbl>     <dbl>   <dbl> <chr>     <chr>         
#>  1 106   Q0         5.68     0.300           NA      NA fixed     natural       
#>  2 106   alpha      0.00628  0.000432        NA      NA fixed     natural       
#>  3 106   alpha_st…  0.0257   0.00176         NA      NA fixed     natural       
#>  4 106   k          2       NA               NA      NA fixed     natural       
#>  5 113   Q0         6.20     0.174           NA      NA fixed     natural       
#>  6 113   alpha      0.00199  0.000109        NA      NA fixed     natural       
#>  7 113   alpha_st…  0.00812  0.000447        NA      NA fixed     natural       
#>  8 113   k          2       NA               NA      NA fixed     natural       
#>  9 142   Q0         6.17     0.641           NA      NA fixed     natural       
#> 10 142   alpha      0.00237  0.000400        NA      NA fixed     natural       
#> # ℹ 30 more rows
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
tidy(fit)
#> # A tibble: 40 × 10
#>    id    term  estimate std.error statistic p.value component estimate_scale
#>    <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl> <chr>     <chr>         
#>  1 19    Q0       10.2      0.269        NA      NA fixed     natural       
#>  2 30    Q0        2.81     0.226        NA      NA fixed     natural       
#>  3 38    Q0        4.50     0.215        NA      NA fixed     natural       
#>  4 60    Q0        9.92     0.459        NA      NA fixed     natural       
#>  5 68    Q0       10.4      0.329        NA      NA fixed     natural       
#>  6 106   Q0        5.68     0.300        NA      NA fixed     natural       
#>  7 113   Q0        6.20     0.174        NA      NA fixed     natural       
#>  8 142   Q0        6.17     0.641        NA      NA fixed     natural       
#>  9 156   Q0        8.35     0.411        NA      NA fixed     natural       
#> 10 188   Q0        6.30     0.564        NA      NA fixed     natural       
#> # ℹ 30 more rows
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
glance(fit)
#> # A tibble: 1 × 12
#>   model_class      backend equation k_spec     nobs n_subjects n_success n_fail
#>   <chr>            <chr>   <chr>    <chr>     <int>      <int>     <int>  <int>
#> 1 beezdemand_fixed legacy  hs       fixed (2)   146         10        10      0
#> # ℹ 4 more variables: converged <lgl>, logLik <dbl>, AIC <dbl>, BIC <dbl>

# Grouped analysis -- fit separately by gender (subset keeps it fast)
data(apt_full)
ids <- unique(apt_full[c("id", "gender")])
ids <- ids[ids$gender %in% c("Male", "Female"), ]
keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
dat <- apt_full[apt_full$id %in% keep, ]
fit_g <- fit_demand_fixed(dat, equation = "hs", k = 2, by = "gender")
#> Data casted as data.frame
#> Warning: FitCurves: subject '484' reported as converged with a non-positive Alpha (Q0d = 5, Alpha = -4.345028e-10); this estimate may be domain-invalid -- inspect before use.
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in numericDeriv(form[[3L]], names(ind), env, central = nDcentral) : 
#>   Missing value or an infinity produced when evaluating the model
#> Warning: FitCurves: subject '491' reported as converged with a non-positive Alpha (Q0d = 0.3572, Alpha = -0.3130247); this estimate may be domain-invalid -- inspect before use.
#> Warning: FitCurves: subject '493' reported as converged with a non-positive Alpha (Q0d = 1.963794, Alpha = -0.01196218); this estimate may be domain-invalid -- inspect before use.
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Warning: FitCurves: subject '496' reported as converged with a non-positive Alpha (Q0d = 1.795118, Alpha = -0.008168063); this estimate may be domain-invalid -- inspect before use.
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   singular gradient
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   singular gradient
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nls(formula = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha *  : 
#>   number of iterations exceeded maximum of 50
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Error in nlsModel(formula, mf, start, wts, scaleOffset = scOff, nDcentral = nDcntr) : 
#>   singular gradient matrix at initial parameter estimates
#> Data casted as data.frame
#> Warning: FitCurves: subject '11' reported as converged with a non-positive Alpha (Q0d = 5, Alpha = -4.345028e-10); this estimate may be domain-invalid -- inspect before use.
#> Error in numericDeriv(form[[3L]], names(ind), env, central = nDcentral) : 
#>   Missing value or an infinity produced when evaluating the model
tidy(fit_g)   # group column prepended
#> # A tibble: 308 × 11
#>    gender id    term  estimate    std.error statistic p.value component
#>    <chr>  <chr> <chr>    <dbl>        <dbl>     <dbl>   <dbl> <chr>    
#>  1 Female 475   Q0       11.0  1.19                NA      NA fixed    
#>  2 Female 476   Q0        1.86 0.162               NA      NA fixed    
#>  3 Female 477   Q0        3.93 0.903               NA      NA fixed    
#>  4 Female 478   Q0        8.18 1.21                NA      NA fixed    
#>  5 Female 479   Q0        4.89 0.461               NA      NA fixed    
#>  6 Female 480   Q0        5.02 0.542               NA      NA fixed    
#>  7 Female 481   Q0        3.57 0.297               NA      NA fixed    
#>  8 Female 482   Q0        3.11 0.302               NA      NA fixed    
#>  9 Female 483   Q0        5.89 0.428               NA      NA fixed    
#> 10 Female 484   Q0        5.00 0.0000000250        NA      NA fixed    
#> # ℹ 298 more rows
#> # ℹ 3 more variables: estimate_scale <chr>, term_display <chr>,
#> #   estimate_internal <dbl>
glance(fit_g)  # one row per group
#> # A tibble: 2 × 13
#>   gender model_class   backend equation k_spec  nobs n_subjects n_success n_fail
#>   <chr>  <chr>         <chr>   <chr>    <chr>  <int>      <int>     <int>  <int>
#> 1 Female beezdemand_f… legacy  hs       fixed…   378         38        33      5
#> 2 Male   beezdemand_f… legacy  hs       fixed…   481         39        38      1
#> # ℹ 4 more variables: converged <lgl>, logLik <dbl>, AIC <dbl>, BIC <dbl>
# }
```
