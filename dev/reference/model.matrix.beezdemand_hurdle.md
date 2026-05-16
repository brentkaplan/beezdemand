# Design matrices for a beezdemand_hurdle fit

Hurdle currently has intercept-only Part I and Part II linear
predictors, so `X_binary` and `X_consumption` are each a single column
of ones with `nobs(fit)` rows. Returned for parity with
[`model.matrix.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/model.matrix.beezdemand_tmb.md).
Future support for factor / covariate effects on hurdle components will
enrich these matrices without changing the API.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
model.matrix(object, what = NULL, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` object.

- what:

  `NULL` (default) returns the full named list. Otherwise one of
  `"X_binary"` or `"X_consumption"`.

- ...:

  Unused.

## Value

Named list of numeric matrices, or a single matrix when `what` is set.
