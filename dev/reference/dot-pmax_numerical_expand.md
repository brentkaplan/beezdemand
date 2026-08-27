# Numerical Pmax via Optimization with Adaptive Domain Expansion

Some demand curves (notably zben's LL4-scale exponential decay
back-transformed to the natural expenditure curve) can have an
unconstrained expenditure-maximizing price well beyond the subject's
observed price range.
[`.pmax_numerical()`](https://brentkaplan.github.io/beezdemand/reference/dot-pmax_numerical.md)
alone then silently returns the domain edge as "Pmax", which is neither
the curve's true maximizer nor stable across subjects/fits observed
through different price ranges. This wraps
[`.pmax_numerical()`](https://brentkaplan.github.io/beezdemand/reference/dot-pmax_numerical.md)
with an adaptive, doubling-decade search: starting from `price_range`,
if the optimum sits within 1% of the current upper bound, the upper
bound is multiplied by 10 and the search repeated, up to
`max_expansions` times.

A wider search interval is only ever adopted when it does not regress
`omax` relative to the best result found so far. This guards against a
known [`stats::optimize()`](https://rdrr.io/r/stats/optimize.html)
failure mode on this curve shape: when the search interval becomes very
large relative to the true (interior) peak, golden-section search can
converge to the interval's right edge with a near-zero objective instead
of the real peak, which would otherwise look identical to "boundary,
keep expanding" and drive the search away from the already-found correct
answer.

## Usage

``` r
.pmax_numerical_expand(expenditure_fn, price_range, max_expansions = 6L)
```

## Arguments

- expenditure_fn:

  Function E(p) returning expenditure at price p.

- price_range:

  Numeric vector c(min, max); the observed/starting domain.

- max_expansions:

  Integer; maximum number of 10x expansions of the upper bound (default
  6, i.e. up to a 10^6 increase over the starting upper bound).

## Value

List with pmax, omax, method, is_boundary, success, note, and
n_expansions (count of 10x expansions actually adopted/attempted).
`method` is `"numerical_optimize_expanded"` whenever at least one
expansion was attempted; `is_boundary` is TRUE only when the maximum
number of expansions was reached and the optimum still sits at the
(expanded) upper bound, i.e. the true maximizer was not found.
