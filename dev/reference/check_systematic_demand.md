# Check Demand Data for Unsystematic Responding

Modern interface for screening purchase task data using Stein et al.
(2015) criteria. Returns a structured object with standardized output
vocabulary that is consistent with \`check_systematic_cp()\`.

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
  id_var = "id"
)
```

## Arguments

- data:

  Data frame in long format with columns: \`id\`, \`x\` (price), \`y\`
  (consumption).

- trend_threshold:

  Numeric. Threshold for trend detection (log-log slope). Default
  \`0.025\`.

- bounce_threshold:

  Numeric. Threshold for bounce proportion. Default \`0.10\`.

- max_reversals:

  Integer. Maximum allowed reversals from zero. Default \`0\`.

- consecutive_zeros:

  Integer. Consecutive zeros required for reversal detection. Default
  \`2\` (per Stein et al. 2015).

- x_var:

  Character. Name of the price column. Default \`"x"\`.

- y_var:

  Character. Name of the consumption column. Default \`"y"\`.

- id_var:

  Character. Name of the subject identifier column. Default \`"id"\`.

## Value

An object of class \`beezdemand_systematicity\` with components:

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

The \`results\` tibble contains standardized columns for both demand and
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
if (FALSE) { # \dontrun{
data(apt)
check <- check_systematic_demand(apt)
print(check)
summary(check)
tidy(check)
} # }
```
