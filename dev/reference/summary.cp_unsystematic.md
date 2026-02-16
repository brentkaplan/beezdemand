# Summarize Cross-Price Unsystematic Data Check Results

Summarizes systematic and unsystematic patterns from multiple calls to
[`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md).
This includes overall proportions, trend and bounce direction counts,
and optionally summaries by subject or group.

## Usage

``` r
# S3 method for class 'cp_unsystematic'
summary(object, ...)
```

## Arguments

- object:

  A data frame containing results from multiple
  [`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md)
  calls, with at minimum the columns 'delta_direction',
  'bounce_direction', and 'bounce_any'. Columns 'id', 'group',
  'reversals', and 'returns' are optional but allow extended summaries.

- ...:

  Additional arguments (currently unused)

## Value

A list of class `summary.cp_unsystematic` with the following elements:

- total_patterns:

  Number of total patterns examined.

- systematic_count:

  Count of systematic patterns (no bounce).

- unsystematic_count:

  Count of unsystematic patterns (bounce detected).

- systematic_percent:

  Proportion of systematic patterns.

- unsystematic_percent:

  Proportion of unsystematic patterns.

- trend_counts:

  Breakdown of trend directions.

- bounce_counts:

  Breakdown of bounce directions.

- reversal_summary:

  (Optional) Summary of zero-reversal patterns, if present in input.

- return_summary:

  (Optional) Summary of zero-return patterns, if present in input.

- group_summary:

  (Optional) Summary stats by 'group'.

- problem_ids:

  (Optional) Top IDs with unsystematic patterns.
