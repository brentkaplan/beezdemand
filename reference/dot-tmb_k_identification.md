# Screen a free-k TMB fit for the signature of an unidentified k

For the k-bearing equations the response depends on k through
`k * (exp(-alpha * Q0 * price) - 1)`. While `alpha * Q0 * price` stays
small that term is linear in price with slope `k * alpha`, so only the
product is identified: k is pinned down by the curvature that appears as
consumption approaches its floor. On data that never get there a free k
can drift arbitrarily far with a compensating alpha.

This is a heuristic screen for that signature rather than a formal
identification test. A `"none"` severity means only that nothing was
detected.

## Usage

``` r
.tmb_k_identification(object)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

## Value

`NULL` when k was not estimated as a free parameter. Otherwise a list
with the fitted `k` and `log_k`, the largest decay exponent
`alpha * Q0 * price` over the modelled rows (`decay_exponent_max`), the
smallest fitted `alpha_min`, a character vector of `reasons`,
`at_boundary` (`"log_k"` when it rests on a user-supplied optimizer
bound, otherwise `character(0)`), and a `severity` of `"warn"`,
`"suspect"` or `"none"`.

## Details

Two findings point at the ridge directly and grade `"warn"`: a fitted k
outside 0.001 to 1000, and a decay exponent that never reaches 0.05 at
any modelled row. Three weaker findings grade `"suspect"`, because each
has innocent explanations: a fitted alpha below 1e-8 (which also follows
from a price unit rescaling, since alpha is not scale-free), `log_k`
resting on a user-supplied optimizer bound (which may simply be a tight
bound), and a non-positive-definite Hessian with a free k (which says
the surface is flat somewhere without localising that to k).
