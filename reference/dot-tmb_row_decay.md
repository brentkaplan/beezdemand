# Per-observation decay exponent of a TMB demand fit

Recomputes `alpha_ij * Q0_ij * price_ij` at every modelled row,
mirroring the likelihood in `src/MixedDemand.h`, so factor levels,
covariates, subject random effects and each row's own price all enter.
The maximum over rows says how far the fitted curve travels down its
decay before the observed prices run out.

## Usage

``` r
.tmb_row_decay(object)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

## Value

A list with the per-row `alpha` and the row-wise decay exponent `u`, or
`NULL` when the stored design cannot be reassembled.
