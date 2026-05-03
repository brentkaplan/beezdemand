# Build subject-level random-effects design matrices from a parsed RE spec

Consumes the canonical block representation produced by
`.normalize_re_input()` (Phase 1) and emits the per-observation design
matrices `Z_q0` and `Z_alpha` that the Phase-2 generalized
`src/MixedDemand.h` template will consume.

## Usage

``` r
.tmb_build_z_matrices(re_parsed, data, id_var = "id")
```

## Arguments

- re_parsed:

  Output of `.normalize_re_input()`.

- data:

  Long-format data frame the model is fit on.

- id_var:

  Subject id column (currently unused — reserved for future per-subject
  reductions).

## Value

A list with components:

- Z_q0:

  Numeric matrix, `n_obs` rows by `re_dim_q0` columns.

- Z_alpha:

  Numeric matrix, `n_obs` rows by `re_dim_alpha` columns.

- re_dim_q0:

  Integer; total Q0 RE columns.

- re_dim_alpha:

  Integer; total alpha RE columns.

## Details

For each block, the helper extracts the
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) columns
corresponding to that block's `terms_q0` / `terms_alpha` from `data` and
stacks them column-wise. Order: block 1 q0 cols, block 2 q0 cols, ...,
then block 1 alpha cols, etc.

Intercept-only blocks (`terms_*` is `"(Intercept)"`) produce a column of
1s; factor-expanded blocks reproduce the model.matrix expansion.
