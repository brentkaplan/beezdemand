# Compute Subject-Specific Parameters

Internal function to compute subject-specific demand parameters from
fixed effects and random effects.

## Usage

``` r
.hurdle_compute_subject_pars(
  coefficients,
  random_effects_mat,
  subject_levels,
  n_re,
  part2,
  price,
  subject_id,
  epsilon,
  id_var
)
```

## Arguments

- coefficients:

  Named vector of fixed effect coefficients.

- random_effects_mat:

  Matrix of transformed random effects.

- subject_levels:

  Unique subject IDs.

- n_re:

  Number of random effects.

- part2:

  Part II equation form.

- price:

  Numeric vector of prices.

- subject_id:

  0-indexed subject IDs.

- epsilon:

  Epsilon for breakpoint calculation.

- id_var:

  Name of ID variable for output.

## Value

Data frame of subject-specific parameters.
