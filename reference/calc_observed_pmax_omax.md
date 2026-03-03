# Calculate Observed Pmax/Omax Grouped by ID

Calculate Observed Pmax/Omax Grouped by ID

## Usage

``` r
calc_observed_pmax_omax(
  data,
  id_var = "id",
  price_var = "x",
  consumption_var = "y"
)
```

## Arguments

- data:

  Data frame with id, price, and consumption columns

- id_var:

  Name of ID column

- price_var:

  Name of price column

- consumption_var:

  Name of consumption column

## Value

Data frame with observed pmax/omax for each subject

## Examples

``` r
# \donttest{
data(apt, package = "beezdemand")
calc_observed_pmax_omax(apt, id_var = "id", price_var = "x", consumption_var = "y")
#>     id pmax_obs omax_obs   method_obs tie_break_obs n_obs_rows n_unique_prices
#> 1   19        9       45 row_wise_max     min_price         16              16
#> 2   30       20       20 row_wise_max     min_price         16              16
#> 3   38        7       21 row_wise_max     min_price         16              16
#> 4   60        6       24 row_wise_max     min_price         16              16
#> 5   68        9       36 row_wise_max     min_price         16              16
#> 6  106        5       15 row_wise_max     min_price         16              16
#> 7  113       15       45 row_wise_max     min_price         16              16
#> 8  142       20       60 row_wise_max     min_price         16              16
#> 9  156        7       21 row_wise_max     min_price         16              16
#> 10 188        5       15 row_wise_max     min_price         16              16
#>    has_duplicate_prices n_max_ties note_obs
#> 1                 FALSE          2     <NA>
#> 2                 FALSE          1     <NA>
#> 3                 FALSE          1     <NA>
#> 4                 FALSE          2     <NA>
#> 5                 FALSE          1     <NA>
#> 6                 FALSE          1     <NA>
#> 7                 FALSE          1     <NA>
#> 8                 FALSE          1     <NA>
#> 9                 FALSE          1     <NA>
#> 10                FALSE          1     <NA>
# }
```
