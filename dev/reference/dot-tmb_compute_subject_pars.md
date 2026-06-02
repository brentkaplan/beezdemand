# Compute Subject-Specific Parameters

Compute Subject-Specific Parameters

## Usage

``` r
.tmb_compute_subject_pars(
  coefficients,
  u_hat,
  subject_levels,
  re_parsed,
  has_k,
  equation,
  price,
  subject_id,
  k_fixed = NULL,
  X_q0 = NULL,
  X_alpha = NULL,
  Z_q0 = NULL,
  Z_alpha = NULL,
  validate_subject_pars = TRUE
)
```

## Arguments

- coefficients:

  Named coefficient vector.

- u_hat:

  Random effects matrix; columns ordered \[block1_q0, block1_alpha,
  block2_q0, ...\].

- subject_levels:

  Character vector of subject IDs.

- re_parsed:

  Canonical RE block structure.

- has_k:

  Logical.

- equation:

  Character.

- price:

  Numeric vector of prices.

- subject_id:

  Integer vector of 0-indexed subject IDs.

- k_fixed:

  Numeric or NULL.

- X_q0, X_alpha:

  Fixed-effect design matrices.

- Z_q0, Z_alpha:

  Random-effect design matrices.

- validate_subject_pars:

  Logical (default `TRUE`); see the `validate_subject_pars` argument of
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  for the NA fallback.

## Value

Data frame of subject-specific parameters.

## Note

For factor-expanded RE specs (e.g. `pdDiag(Q0+alpha~condition)`),
subject-level Q0 / alpha here use the first observed row of `Z_q0` /
`Z_alpha` per subject – which encodes the subject's first observed
condition only. This helper does not emit per-(subject, condition) rows;
use [`predict()`](https://rdrr.io/r/stats/predict.html) for cell-level
values.
