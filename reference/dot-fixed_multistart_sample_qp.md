# Draw S - 1 log-uniform (Q0, Pmax) sampled starts for one subject

Sampling region mirrors the qualtrics-panel-discounting-2 Tier-1
reference implementation
(`R/battery/11-multistart-protocol.R::sample_starts()`):
`Q0 ~ log-uniform[0.25 * max(y+), 4 * max(y+)]`,
`Pmax ~ log-uniform[min(x+) / 2, 4 * max(x+)]`, where `y+`/`x+` are the
subject's strictly-positive observed consumption/price values.

## Usage

``` r
.fixed_multistart_sample_qp(x, y, n)
```

## Arguments

- x:

  Numeric price vector for the subject.

- y:

  Numeric consumption vector for the subject.

- n:

  Number of starts to draw (`S - 1`).

## Value

List with elements `q0` and `pmax`, each length `n` (or length 0 if
sampling is not possible, e.g. no strictly-positive observations).
