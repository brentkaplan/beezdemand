# Generate Default Starting Values

Generate Default Starting Values

## Usage

``` r
.tmb_default_starts(
  prepared,
  design,
  equation,
  re_parsed,
  has_k,
  k_fixed = NULL
)
```

## Arguments

- prepared:

  Output from .tmb_prepare_data().

- design:

  Output from .tmb_build_design_matrices().

- equation:

  Character string.

- re_parsed:

  Canonical RE block structure.

- has_k:

  Logical.

- k_fixed:

  Numeric or NULL.

## Value

Named list of starting values shaped for the Phase-2 template:
`logsigma` is a vector of length `sum(block dims)`, `rho_raw` is a
vector of length `sum(over pdSymm: d*(d-1)/2)`, and `u` has columns
ordered \[block1_q0, block1_alpha, block2_q0, ...\].
