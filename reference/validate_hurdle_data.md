# Validate Hurdle Demand Data

Internal function to validate and prepare data for hurdle demand model
fitting.

## Usage

``` r
validate_hurdle_data(data, y_var, x_var, id_var)
```

## Arguments

- data:

  A data frame.

- y_var:

  Character string, name of consumption variable.

- x_var:

  Character string, name of price variable.

- id_var:

  Character string, name of subject ID variable.

## Value

A cleaned data frame ready for model fitting.
