# Prepare Hurdle Model Data

Internal function to prepare data structures for TMB hurdle model
fitting. Converts subject IDs to 0-indexed integers and creates derived
variables.

## Usage

``` r
.hurdle_prepare_data(data, y_var, x_var, id_var)
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

## Value

A list containing:

- price:

  Numeric vector of prices

- consumption:

  Numeric vector of consumption values

- delta:

  Integer vector (1 if consumption == 0, else 0)

- logQ:

  Log consumption (0 for zeros)

- subject_id:

  0-indexed subject IDs for C++

- subject_levels:

  Unique subject IDs (original)

- n_subjects:

  Number of unique subjects
