# Check Cross-Price Data for Unsystematic Responding

Modern interface for screening cross-price data with standardized output
vocabulary aligned with
[`check_systematic_demand()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_demand.md).

## Usage

``` r
check_systematic_cp(
  data,
  trend_threshold = 0.025,
  bounce_threshold_down = 0.1,
  bounce_threshold_up = 0.1,
  bounce_threshold_none = 0.1,
  consecutive_zeros = 2,
  consecutive_nonzeros = 2,
  expected_down = FALSE,
  x_var = "x",
  y_var = "y",
  id_var = "id",
  by = NULL
)
```

## Arguments

- data:

  Data frame with columns: `id` (optional), `x` (price), `y`
  (consumption).

- trend_threshold:

  Numeric. Threshold for trend detection. Default `0.025`.

- bounce_threshold_down:

  Numeric. Bounce threshold for upward trends. Default `0.1`.

- bounce_threshold_up:

  Numeric. Bounce threshold for downward trends. Default `0.1`.

- bounce_threshold_none:

  Numeric. Bounce threshold when no trend. Default `0.1`.

- consecutive_zeros:

  Integer. Zeros for reversal detection. Default `2`.

- consecutive_nonzeros:

  Integer. Non-zeros for return detection. Default `2`.

- expected_down:

  Logical. Suppress reversal detection if TRUE. Default `FALSE`.

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

An object of class `beezdemand_systematicity` with the same structure as
[`check_systematic_demand()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_demand.md),
with `type = "cp"`.

## Details

If the data contains an `id` column (or column specified by `id_var`),
each unique ID is checked separately. Otherwise, the entire dataset is
treated as a single pattern.

For cross-price data, the wrapper preserves the legacy meaning of
[`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md):

- `trend_direction` and `bounce_direction` are taken directly from the
  legacy function outputs.

- `trend_pass` is set to `NA` because cross-price systematicity does not
  use a separate trend “pass/fail” criterion in the same way as
  purchase-task screening; instead, trend classification determines
  which bounce rule applies.

- `bounce_stat` is reported as the proportion relevant to the legacy
  bounce rule for the detected `trend_direction` (or `expected_down`
  case), computed from the legacy bounce counts and the number of price
  steps.

## Examples

``` r
data(etm)
check <- check_systematic_cp(etm)
```
