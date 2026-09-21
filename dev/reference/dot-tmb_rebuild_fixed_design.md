# Rebuild a fixed-effect design matrix pinned to the fitted contrasts

[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) honours
`options("contrasts")` at call time. If that option changed after
fitting, a rebuilt design keeps the same column count but encodes a
different basis, so `beta` silently multiplies the wrong columns
(F-BD6-2). This helper passes the fitted matrix's `contrasts` attribute,
verifies the rebuilt columns match the fitted ones, reorders to the
fitted order, and aborts loudly otherwise. The EMM grid builder applies
the same rule (TICKET-016, F1).

## Usage

``` r
.tmb_rebuild_fixed_design(fitted_X, rhs, data, param = "Q0")
```

## Arguments

- fitted_X:

  The design matrix stored on the fit (`formula_details$X_*`).

- rhs:

  A one-sided formula or its character form.

- data:

  Data frame to build the design from.

- param:

  `"Q0"` or `"alpha"`, for messages only.

## Value

The rebuilt design matrix with the fitted column order and the `assign`
/ `contrasts` attributes
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) produced.
