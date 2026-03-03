# Collapse Factor Levels for a Specific Parameter

Internal helper to apply level collapsing for a single parameter (Q0 or
alpha). Creates new columns with suffix to avoid modifying original
factor columns.

## Usage

``` r
collapse_factor_levels(data, collapse_spec, factors, suffix)
```

## Arguments

- data:

  A data frame.

- collapse_spec:

  Named list of factor collapse specifications. Structure:
  `list(factor_name = list(new_level = c(old_levels), ...))`.

- factors:

  Character vector of factor names in the model.

- suffix:

  Character suffix for new column names (e.g., "Q0" or "alpha").

## Value

A list with:

- `data`: Modified data frame with new collapsed factor columns

- `new_factor_names`: Character vector of new factor column names to use

- `info`: List with original and new levels for each collapsed factor
