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

- ...:

  Additional arguments passed to the underlying
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  engine.

## Value

An object of class `beezdemand_fixed` with components:

- results:

  Data frame of fitted parameters for each subject

- fits:

  List of model fit objects (if `detailed = TRUE` internally)

- predictions:

  List of prediction data frames

- data_used:

  List of data frames used for each fit

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
# }
```
