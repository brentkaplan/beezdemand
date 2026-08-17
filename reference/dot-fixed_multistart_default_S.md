# Default multi-start budget for an equation/k combination

Tiered by parameter count: 2-parameter forms (hs/koff/simplified with a
fixed `k`) get `S = 8`; 3-parameter forms (`k = "fit"`) get `S = 32`.
`equation = "linear"` is never multistarted (`S = 1`, forced).

## Usage

``` r
.fixed_multistart_default_S(equation, k)
```

## Arguments

- equation:

  Character. Canonical (post `normalize_equation()`) equation name:
  `"hs"`, `"koff"`, `"simplified"`, or `"linear"`.

- k:

  The `k` argument as supplied by the caller of
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
  (numeric, or a character mode string such as `"fit"`, `"ind"`,
  `"range"`, `"share"`).

## Value

Integer scalar default budget.
