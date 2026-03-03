# Validate and Filter Cross-Price Demand Data

This function performs validation checks on cross-price demand data and
applies filtering if specified. It ensures the data meets the
requirements for analysis by checking for required columns (after
optional column renaming), filtering by target type if needed, and
confirming ID column presence when required.

## Usage

``` r
validate_cp_data(
  data,
  x_var = "x",
  y_var = "y",
  id_var = "id",
  group_var = "group",
  target_var = "target",
  required_cols = c("x", "y"),
  filter_target = TRUE,
  target_level = "alt",
  require_id = FALSE
)
```

## Arguments

- data:

  A data frame containing cross-price demand data.

- x_var:

  Character string; name of the price column. Default is `"x"`. If
  non-default, the column is renamed to `"x"` internally.

- y_var:

  Character string; name of the consumption column. Default is `"y"`. If
  non-default, the column is renamed to `"y"` internally.

- id_var:

  Character string; name of the subject identifier column. Default is
  `"id"`. If non-default, the column is renamed to `"id"` internally.

- group_var:

  Character string; name of the group column. Default is `"group"`. If
  non-default, the column is renamed to `"group"` internally.

- target_var:

  Character string; name of the target indicator column. Default is
  `"target"`. If non-default, the column is renamed to `"target"`
  internally.

- required_cols:

  Character vector of canonical column names to check after renaming.
  Default is `c("x", "y")`.

- filter_target:

  Logical; if TRUE and data contains a `"target"` column, filters to
  keep only rows where `target == target_level`. Default is TRUE.

- target_level:

  Character string; the value of the `target` column to retain when
  `filter_target = TRUE`. Default is `"alt"`.

- require_id:

  Logical; if TRUE, validates that an `"id"` column exists in the data
  (after renaming). Default is FALSE.

## Value

A validated (and potentially filtered) data frame with canonical column
names.

## Details

Column renaming uses a collision-safe approach: if a non-default `*_var`
mapping is requested but the canonical target name already exists as a
different column in `data`, the function stops with an informative error
rather than silently overwriting data. After normalization, `$data` on
returned objects always uses canonical names (`x`, `y`, `id`, `group`,
`target`), which is required for S3 methods to work correctly.
