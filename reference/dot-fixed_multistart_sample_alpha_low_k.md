# Explicit alpha sampler for the low-k / no-interior-Pmax case

Draws alpha log-uniformly over a range implied by the sampled (Q0, Pmax)
bounds, independent of any particular (Q0, Pmax) pair – see
[`.fixed_multistart_qp_to_alpha()`](https://brentkaplan.github.io/beezdemand/reference/dot-fixed_multistart_qp_to_alpha.md)
Details for why this is a distinct code path from the SND point-mapping
formula.

## Usage

``` r
.fixed_multistart_sample_alpha_low_k(q0, pmax)
```

## Arguments

- q0:

  Numeric vector (or scalar) of sampled/fixed Q0.

- pmax:

  Numeric vector of sampled Pmax.

## Value

Numeric vector of alpha starting values (same length as `pmax`).
