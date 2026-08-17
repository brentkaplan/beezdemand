# Build TMB Data List

Build TMB Data List

## Usage

``` r
.tmb_build_tmb_data(prepared, design, equation, re_parsed, data, id_var = "id")
```

## Arguments

- prepared:

  Output from .tmb_prepare_data().

- design:

  Output from .tmb_build_design_matrices().

- equation:

  Character string, equation type.

- re_parsed:

  Canonical RE block structure from `.normalize_re_input()`.

- data:

  The (possibly cleaned) data frame the model is fit on.

- id_var:

  Subject id column name.

## Value

A list suitable for TMB::MakeADFun data argument, including the
per-observation RE design matrices (`Z_q0`, `Z_alpha`) and block-map
metadata vectors consumed by the Phase-2 generalized template.
