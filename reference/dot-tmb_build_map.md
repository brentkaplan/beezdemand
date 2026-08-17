# Build TMB Map List

Phase 2 expresses pdDiag-vs-pdSymm via the dimensionality of `rho_raw`
(zero-length for pdDiag blocks, length d\*(d-1)/2 for pdSymm blocks of
size d). The map argument therefore only needs to handle `log_k` (mapped
out for simplified / zben).

## Usage

``` r
.tmb_build_map(has_k)
```

## Arguments

- has_k:

  Logical, whether k is estimated.

## Value

Named list for TMB map argument.
