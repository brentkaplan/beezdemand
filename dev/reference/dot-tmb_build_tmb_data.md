# Build TMB Data List

Build TMB Data List

## Usage

``` r
.tmb_build_tmb_data(prepared, design, equation, n_re)
```

## Arguments

- prepared:

  Output from .tmb_prepare_data().

- design:

  Output from .tmb_build_design_matrices().

- equation:

  Character string, equation type.

- n_re:

  Integer, number of random effects.

## Value

A list suitable for TMB::MakeADFun data argument.
