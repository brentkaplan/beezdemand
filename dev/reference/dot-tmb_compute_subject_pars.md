# Compute Subject-Specific Parameters

Compute Subject-Specific Parameters

## Usage

``` r
.tmb_compute_subject_pars(
  coefficients,
  u_hat,
  subject_levels,
  n_re,
  has_k,
  equation,
  price,
  subject_id,
  k_fixed = NULL,
  X_q0 = NULL,
  X_alpha = NULL
)
```

## Arguments

- coefficients:

  Named coefficient vector.

- u_hat:

  Random effects matrix.

- subject_levels:

  Character vector of subject IDs.

- n_re:

  Integer.

- has_k:

  Logical.

- equation:

  Character.

- price:

  Numeric vector of prices.

- subject_id:

  Integer vector of 0-indexed subject IDs.

## Value

Data frame of subject-specific parameters.
