# Analytic search domain for the zben expenditure maximum

The zben curve is `y_ll4(p) = q * exp(-t)` with `q = log10(Q0)` and the
dimensionless price `t = alpha * Q0 * p / q`; the natural-scale
expenditure is `E = p * (10^(4 q exp(-t)) - 1)^(1/4)`. Writing
`u = 4 ln(Q0) * exp(-t)`, the stationarity condition `dE/dp = 0` reduces
to `t * u / (4 (1 - exp(-u))) = 1`, and because `u / (1 - exp(-u)) >= 1`
for every `u > 0`, every stationary point satisfies `t < 4`. All local
(hence the global) expenditure maxima therefore lie at prices below
`4 * log10(Q0) / (alpha * Q0)`, whatever prices were observed; the curve
has at most two of them (Codex source review, release-correctness audit
2026-09-06). Searching this domain makes the reported `Pmax`/`Omax`
independent of the observed price grid, which a doubling expansion that
stops at the first interior peak cannot guarantee. Verified against a
brute-force global search on 2 000 random `(Q0, alpha)` pairs (0
violations).

## Usage

``` r
.zben_search_domain(alpha_nat, q0_nat)
```

## Arguments

- alpha_nat, q0_nat:

  Natural-scale zben parameters.

## Value

Numeric `c(lower, upper)` search interval, or `NULL` when the bound
cannot be formed (non-finite or non-positive inputs).
