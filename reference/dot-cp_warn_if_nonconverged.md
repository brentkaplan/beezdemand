# Warn When a cp_model_nls Fit's Winning Backend Did Not Converge

Internal helper (TICKET-065). `object$convergence$isConv` is `FALSE`
only when the winning backend explicitly reported non-convergence
(`nls`/ `nlsLM`-class `convInfo`); it is `NA` when no such diagnostic
exists (`wrapnlsr`) or the object predates this field. Only the explicit
`FALSE` case warns (an unknown convergence status is not itself an
error).

## Usage

``` r
.cp_warn_if_nonconverged(object)
```

## Arguments

- object:

  A `cp_model_nls` object.

## Value

Invisible `NULL`; called for the warning side effect.
