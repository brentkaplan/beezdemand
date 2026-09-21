# Raw (pre-tanh partial-correlation) parameter for rho_bc

Inverts the LKJ-Cholesky mapping used by `src/HurdleDemand3RE.h`,
`rho_bc = rho_ab * rho_ac + tanh(rho_bc_raw) * sqrt((1 - rho_ab^2) * (1 - rho_ac^2))`,
so that a Monte Carlo truth table compares the fitted `rho_bc_raw` to
the value that actually generates the requested correlations.

## Usage

``` r
.hurdle_rho_bc_raw_from_corr(rho_ab, rho_ac, rho_bc)
```

## Arguments

- rho_ab, rho_ac, rho_bc:

  Actual (final) correlations.

## Value

Numeric scalar; `NA` when the implied partial correlation is not inside
(-1, 1).
