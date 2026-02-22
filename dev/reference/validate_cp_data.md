# Validate and Filter Cross-Price Demand Data

This function performs validation checks on cross-price demand data and
applies filtering if specified. It ensures the data meets the
requirements for analysis by checking for required columns, filtering by
target type if needed, and confirming ID column presence when required.

## Usage

``` r
validate_cp_data(
  data,
  required_cols = c("x", "y"),
  filter_target = TRUE,
  require_id = FALSE
)
```

## Arguments

- data:

  A data frame containing cross-price demand data.

- required_cols:

  Character vector of required column names. Default is c("x", "y").

- filter_target:

  Logical; if TRUE and data contains a "target" column, filters to keep
  only rows where target == "alt". Default is TRUE.

- require_id:

  Logical; if TRUE, validates that an "id" column exists in the data.
  Default is FALSE.

## Value

A validated (and potentially filtered) data frame.
