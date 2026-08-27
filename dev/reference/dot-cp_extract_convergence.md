# Extract Backend Convergence Info from an NLS-family Fit

Internal helper (TICKET-065). Reads `model$convInfo` into a plain list.
This works generically for any backend that populates `convInfo`:
`nls`/`nlsLM`-class fits always do, and
[`nlsr::wrapnlsr()`](https://rdrr.io/pkg/nlsr/man/wrapnlsr.html) fits do
too when `wrapnlsr()` returns a plain `nls`-class object (its usual
successful case; verified via
`nlsr::wrapnlsr(y ~ a*exp(-b*x), ...)$convInfo`). `isConv` is only `NA`
(unknown), not `FALSE`, when the winning object carries no `convInfo` at
all. The two must stay distinguishable so downstream gates only warn on
an explicit, known non-convergence.

## Usage

``` r
.cp_extract_convergence(model)
```

## Arguments

- model:

  A fitted model object (`nls`, `nlsLM`, `nlsr`, or similar).

## Value

A list with `isConv`, `finIter`, `stopCode`, `stopMessage`.
