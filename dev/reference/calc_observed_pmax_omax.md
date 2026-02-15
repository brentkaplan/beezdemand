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
