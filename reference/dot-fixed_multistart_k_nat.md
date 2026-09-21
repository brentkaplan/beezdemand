# Resolve a natural-scale k value for the (Q0, Pmax) -\> alpha mapping

Prefers the production fit's own recorded `K` (natural scale, already
back-transformed by `ExtractCoefs()`) since that is the exact value used
for this subject regardless of the caller's `k` mode (`"ind"`, `"fit"`,
`"range"`, `"share"`, or a plain number). Falls back to the caller's `k`
if numeric, else to
[`GetK()`](https://brentkaplan.github.io/beezdemand/reference/GetK.md)
computed on the subject's own data. Both are only used to seed a
*starting* alpha, so approximate is acceptable.

## Usage

``` r
.fixed_multistart_k_nat(k_production, k_arg, adf)
```

## Arguments

- k_production:

  Numeric scalar; the production row's `K` value (may be `NA` if the
  production fit was a total failure).

- k_arg:

  The `k` argument as supplied by the caller of
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).

- adf:

  Single-subject data frame (post
  [`CheckCols()`](https://brentkaplan.github.io/beezdemand/reference/CheckCols.md),
  columns `id`/`x`/`y`) used as a last-resort fallback for
  [`GetK()`](https://brentkaplan.github.io/beezdemand/reference/GetK.md).

## Value

Numeric scalar natural-scale k (may be `NA_real_` if nothing works).
