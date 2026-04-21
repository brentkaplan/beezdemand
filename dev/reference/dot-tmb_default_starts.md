# Generate Default Starting Values

Generate Default Starting Values

## Usage

``` r
.tmb_default_starts(prepared, design, equation, n_re, has_k, k_fixed = NULL)
```

## Arguments

- prepared:

  Output from .tmb_prepare_data().

- design:

  Output from .tmb_build_design_matrices().

- equation:

  Character string.

- n_re:

  Integer.

- has_k:

  Logical.

- k_fixed:

  Numeric or NULL.

## Value

Named list of starting values.
