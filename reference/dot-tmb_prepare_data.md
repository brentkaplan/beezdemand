# Prepare Data for TMB Mixed-Effects Demand Model

Equation-aware data preparation for TMB continuous-only demand models.
Handles zero filtering, log transformation, and subject ID mapping.

## Usage

``` r
.tmb_prepare_data(data, y_var, x_var, id_var, equation)
```

## Arguments

- data:

  A validated data frame.

- y_var:

  Character string, name of consumption variable.

- x_var:

  Character string, name of price variable.

- id_var:

  Character string, name of subject ID variable.

- equation:

  Character string, one of "exponential", "exponentiated", "simplified",
  "zben".

## Value

A list containing prepared data vectors and subject mapping.
