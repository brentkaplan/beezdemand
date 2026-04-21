# Split Data by Grouping Variables and Apply Function

Internal helper that splits a data frame by the columns in `by`, applies
`FUN(slice, key_row)` to each group, and returns a named list of
results.

## Usage

``` r
beezdemand_split_by(data, by, FUN, call = rlang::caller_env())
```

## Arguments

- data:

  A data frame.

- by:

  Character vector of column names to split by.

- FUN:

  Function taking two arguments: the data subset and a one-row data
  frame of group key values. Must return the per-group result.

- call:

  Caller environment for error reporting.

## Value

A list with:

- results:

  Named list of per-group FUN outputs

- group_keys:

  Data frame of unique group combinations
