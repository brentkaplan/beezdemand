# Check Demand Data for Unsystematic Responding

Modern interface for screening purchase task data using Stein et al.
(2015) criteria. Returns a structured object with standardized output
vocabulary that is consistent with
[`check_systematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_cp.md).

## Usage

``` r
check_systematic_demand(
  data,
  trend_threshold = 0.025,
  bounce_threshold = 0.1,
  max_reversals = 0,
  consecutive_zeros = 2,
  x_var = "x",
  y_var = "y",
  id_var = "id",
  by = NULL
)
```

## Arguments

- data:

  Data frame in long format with columns: `id`, `x` (price), `y`
  (consumption).

- trend_threshold:

  Numeric. Threshold for trend detection (log-log slope). Default
  `0.025`.

- bounce_threshold:

  Numeric. Threshold for bounce proportion. Default `0.10`.

- max_reversals:

  Integer. Maximum allowed reversals from zero. Default `0`.

- consecutive_zeros:

  Integer. Consecutive zeros required for reversal detection. Default
  `2` (per Stein et al. 2015).

- x_var:

  Character. Name of the price column. Default `"x"`.

- y_var:

  Character. Name of the consumption column. Default `"y"`.

- id_var:

  Character. Name of the subject identifier column. Default `"id"`.

- by:

  Optional character vector of column names to group by. When supplied,
  the check is run separately within each unique combination of the `by`
  columns. Group columns are prepended to `$results`. Default `NULL` (no
  grouping).

## Value

An object of class `beezdemand_systematicity` with components:

- results:

  Tibble with one row per subject containing systematicity metrics

- type:

  "demand"

- call:

  The original function call

- n_total:

  Total number of subjects

- n_systematic:

  Number of subjects passing all criteria

- n_unsystematic:

  Number of subjects failing at least one criterion

## Details

The `results` tibble contains standardized columns for both demand and
cross-price systematicity checks:

- id:

  Subject identifier

- type:

  "demand" for this function

- trend_stat:

  DeltaQ statistic (log-log slope)

- trend_threshold:

  Threshold used

- trend_direction:

  "down", "up", or "none"

- trend_pass:

  Logical: passed trend criterion

- bounce_stat:

  Bounce proportion

- bounce_threshold:

  Threshold used

- bounce_direction:

  "significant" or "none"

- bounce_pass:

  Logical: passed bounce criterion

- reversals:

  Count of reversals from zero

- reversals_pass:

  Logical: passed reversals criterion

- returns:

  NA for demand (CP-specific)

- n_positive:

  Count of positive values

- systematic:

  Logical: passed all criteria

## Examples

``` r
# \donttest{
data(apt)
check <- check_systematic_demand(apt)
print(check)
#> 
#> Systematicity Check (demand)
#> ------------------------------ 
#> Total patterns: 10 
#> Systematic: 10 ( 100 %)
#> Unsystematic: 0 ( 0 %)
#> 
#> Use summary() for details, tidy() for per-subject results.
summary(check)
#> 
#> Systematicity Check Summary (demand)
#> ================================================== 
#> 
#> Total patterns: 10 
#> Systematic: 10 ( 100 %)
#> Unsystematic: 0 ( 0 %)
#> 
#> Failures by Criterion:
#> # A tibble: 4 × 3
#>   criterion n_fail pct_fail
#>   <chr>      <int>    <dbl>
#> 1 trend          0        0
#> 2 bounce         0        0
#> 3 reversals      0        0
#> 4 overall        0        0
#> 
tidy(check)
#> # A tibble: 10 × 15
#>    id    type  trend_stat trend_threshold trend_direction trend_pass bounce_stat
#>    <chr> <chr>      <dbl>           <dbl> <chr>           <lgl>            <dbl>
#>  1 19    dema…      0.211           0.025 down            TRUE                 0
#>  2 30    dema…      0.144           0.025 down            TRUE                 0
#>  3 38    dema…      0.788           0.025 down            TRUE                 0
#>  4 60    dema…      0.909           0.025 down            TRUE                 0
#>  5 68    dema…      0.909           0.025 down            TRUE                 0
#>  6 106   dema…      0.818           0.025 down            TRUE                 0
#>  7 113   dema…      0.144           0.025 down            TRUE                 0
#>  8 142   dema…      0.129           0.025 down            TRUE                 0
#>  9 156   dema…      0.862           0.025 down            TRUE                 0
#> 10 188   dema…      0.818           0.025 down            TRUE                 0
#> # ℹ 8 more variables: bounce_threshold <dbl>, bounce_direction <chr>,
#> #   bounce_pass <lgl>, reversals <int>, reversals_pass <lgl>, returns <int>,
#> #   n_positive <int>, systematic <lgl>

# Grouped check — results include group column
data(apt_full)
check_g <- check_systematic_demand(apt_full, by = "gender")
check_g$results
#> # A tibble: 1,100 × 16
#>    gender id    type   trend_stat trend_threshold trend_direction trend_pass
#>    <chr>  <chr> <chr>       <dbl>           <dbl> <chr>           <lgl>     
#>  1 Female 475   demand     0.909            0.025 down            TRUE      
#>  2 Female 476   demand     0.0905           0.025 down            TRUE      
#>  3 Female 477   demand     0.818            0.025 down            TRUE      
#>  4 Female 478   demand     0.842            0.025 down            TRUE      
#>  5 Female 479   demand     0.788            0.025 down            TRUE      
#>  6 Female 480   demand     0.818            0.025 down            TRUE      
#>  7 Female 481   demand     0.751            0.025 down            TRUE      
#>  8 Female 482   demand     0.144            0.025 down            TRUE      
#>  9 Female 483   demand     0.818            0.025 down            TRUE      
#> 10 Female 484   demand     0.818            0.025 down            TRUE      
#> # ℹ 1,090 more rows
#> # ℹ 9 more variables: bounce_stat <dbl>, bounce_threshold <dbl>,
#> #   bounce_direction <chr>, bounce_pass <lgl>, reversals <int>,
#> #   reversals_pass <lgl>, returns <int>, n_positive <int>, systematic <lgl>
# }
```
