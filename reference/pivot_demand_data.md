# Reshape Demand Data Between Wide and Long Formats

Converts demand data between wide format (one row per subject, prices as
columns) and long format (one row per observation with `id`, `x`, `y`
columns). This is a convenience wrapper around
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
and
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
tailored for behavioral economic purchase task data.

## Usage

``` r
pivot_demand_data(
  data,
  format = c("long", "wide"),
  id_var = "id",
  x_var = "x",
  y_var = "y",
  price_cols = NULL,
  x_values = NULL,
  drop_na = TRUE
)
```

## Arguments

- data:

  A data frame or tibble to reshape.

- format:

  Character. Direction of reshaping: `"long"` (wide to long) or `"wide"`
  (long to wide). Default `"long"`.

- id_var:

  Character. Name of the subject/series identifier column. Default
  `"id"`.

- x_var:

  Character. Name of the price column in long-format data. Used when
  `format = "wide"`. Default `"x"`.

- y_var:

  Character. Name of the consumption column in long-format data. Used
  when `format = "wide"`. Default `"y"`.

- price_cols:

  Character vector of column names in wide-format data that represent
  prices. Used when `format = "long"`. Default `NULL`, which
  auto-detects: columns whose names parse as numeric become price
  columns, and remaining columns (besides `id_var`) are preserved as
  identifiers. If no column names parse as numeric, all non-`id_var`
  columns are treated as price columns (and `x_values` must be
  supplied).

- x_values:

  Numeric vector of actual price values corresponding to each price
  column in wide-format data. Used when `format = "long"`. Must be the
  same length as the number of price columns. Default `NULL`, which
  parses prices from column names.

- drop_na:

  Logical. When `format = "long"`, drop rows where consumption (`y`) is
  `NA` after pivoting? Default `TRUE`. A warning is issued when rows are
  dropped.

## Value

A tibble. When `format = "long"`: columns `id`, any extra identifier
columns, `x` (numeric price), and `y` (numeric consumption). When
`format = "wide"`: one row per subject with prices as column names.

## Details

### Wide to Long (`format = "long"`)

The function determines which columns are price columns by:

1.  If `price_cols` is provided, those columns are used directly.

2.  If `price_cols` is `NULL`, column names are tested with
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html). Columns whose
    names successfully parse as numbers (e.g., `"0"`, `"0.5"`, `"10"`)
    are treated as price columns. Remaining non-`id_var` columns are
    preserved as extra identifiers.

3.  If no column names parse as numeric, all non-`id_var` columns become
    price columns and `x_values` must be supplied.

Actual numeric price values come from `x_values` if supplied, or from
parsing column names. If column names cannot be parsed and `x_values` is
not supplied, an error is raised.

### Long to Wide (`format = "wide"`)

Pivots long data so that each unique value in `x_var` becomes a column,
with values from `y_var`. All columns except `x_var` and `y_var` are
used as identifiers.

## Examples

``` r
# --- Wide to long ---
# Columns named as prices (auto-parsed)
wide_num <- data.frame(
  id = 1:3,
  "0" = c(10, 8, 12), "0.5" = c(9, 7, 11), "1" = c(8, 6, 9),
  check.names = FALSE
)
pivot_demand_data(wide_num, format = "long")
#> # A tibble: 9 × 3
#>      id     x     y
#>   <int> <dbl> <dbl>
#> 1     1   0      10
#> 2     1   0.5     9
#> 3     1   1       8
#> 4     2   0       8
#> 5     2   0.5     7
#> 6     2   1       6
#> 7     3   0      12
#> 8     3   0.5    11
#> 9     3   1       9

# Columns with non-numeric names require x_values
wide_named <- data.frame(id = 1:2, price_1 = c(10, 8), price_2 = c(5, 4))
pivot_demand_data(wide_named, format = "long",
                  x_values = c(0, 0.5))
#> # A tibble: 4 × 3
#>      id     x     y
#>   <int> <dbl> <dbl>
#> 1     1   0      10
#> 2     1   0.5     5
#> 3     2   0       8
#> 4     2   0.5     4

# --- Long to wide ---
data(apt, package = "beezdemand")
wide_apt <- pivot_demand_data(apt, format = "wide")
head(wide_apt)
#> # A tibble: 6 × 17
#>      id   `0` `0.5`   `1` `1.5`   `2` `2.5`   `3`   `4`   `5`   `6`   `7`   `8`
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    19    10    10    10     8     8     8     7     7     7     6     6     5
#> 2    30     3     3     3     3     2     2     2     2     2     2     2     2
#> 3    38     4     4     4     4     4     4     4     3     3     3     3     2
#> 4    60    10    10     8     8     6     6     5     5     4     4     3     3
#> 5    68    10    10     9     9     8     8     7     6     5     5     5     4
#> 6   106     5     5     5     5     4     4     4     3     3     2     2     0
#> # ℹ 4 more variables: `9` <dbl>, `10` <dbl>, `15` <dbl>, `20` <dbl>
```
