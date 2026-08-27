# Map sampled (Q0, Pmax) to alpha using the package's own closed forms

Mirrors
[`beezdemand_calc_pmax_omax()`](https://brentkaplan.github.io/beezdemand/reference/beezdemand_calc_pmax_omax.md)
(`R/pmax-omax-engine.R`), verified by round-trip: fixed-effect `"hs"`
and `"koff"` fit the identical mean function
(`Q(p) = Q0 * 10^(k * (exp(-alpha * Q0 * p) - 1))`, just on different
scales) and share the Lambert-W relation used by
[`.pmax_analytic_hs()`](https://brentkaplan.github.io/beezdemand/reference/dot-pmax_analytic_hs.md):
`Pmax = -W_0(-1 / (k * ln(10))) / (alpha * Q0)`
`=> alpha = -W_0(-1 / (k * ln(10))) / (Pmax * Q0)`. `"simplified"`/SND
uses
[`.pmax_analytic_snd()`](https://brentkaplan.github.io/beezdemand/reference/dot-pmax_analytic_snd.md)'s
closed form: `Pmax = 1 / (alpha * Q0) => alpha = 1 / (Pmax * Q0)`.

## Usage

``` r
.fixed_multistart_qp_to_alpha(equation, k_nat, q0, pmax)
```

## Arguments

- equation:

  `"hs"`, `"koff"`, or `"simplified"`.

- k_nat:

  Natural-scale `k` (ignored for `"simplified"`).

- q0:

  Numeric vector (or scalar, recycled) of sampled/fixed Q0.

- pmax:

  Numeric vector of sampled Pmax.

## Value

Numeric vector of alpha starting values (same length as `pmax`).

## Details

Low-k fallback: if `k` is too small for a real principal-branch
Lambert-W solution to exist at this scale (`k <= exp(1) / log(10)`,
mirroring
[`.pmax_analytic_hs()`](https://brentkaplan.github.io/beezdemand/reference/dot-pmax_analytic_hs.md)'s
own existence check, or if
[`lambertW()`](https://brentkaplan.github.io/beezdemand/reference/lambertW.md)
itself fails), hs/koff have no interior Pmax at all; the sampled Pmax is
not achievable by any alpha via the closed form. The function does not
silently fall back to the (unrelated) SND point formula
`alpha = 1 / (Pmax * Q0)` in this case. Instead it draws alpha directly
and independently, log-uniform over a range implied by the same sampled
(Q0, Pmax) bounds: an explicit, documented, genuinely stochastic
low-k/degenerate-k sampler (two calls with the same inputs but different
RNG state give different alpha).
