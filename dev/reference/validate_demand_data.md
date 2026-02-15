# Validate and Prepare Demand Data

Internal helper function to validate required columns in demand data and
ensure that specified factor columns are correctly formatted and have
unused levels dropped.

## Usage

``` r
validate_demand_data(data, y_var, x_var, id_var, factors = NULL)
```

## Arguments

- data:

  A data frame.

- y_var:

  Character string, the name of the dependent variable column.

- x_var:

  Character string, the name of the independent variable column.

- id_var:

  Character string, the name of the subject/group identifier column.

- factors:

  Character vector of factor names (can be NULL).

## Value

The validated and prepared data frame.
