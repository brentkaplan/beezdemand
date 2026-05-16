# Fitted values for a beezdemand_hurdle fit

Returns marginal expected consumption `P(y > 0 | x) * E(y | y > 0, x)`
by default (the `.fitted` column of `predict(fit, type = "demand")`).
With `marginal = FALSE`, returns the conditional-on-positive expectation
(the `.fitted` column of `predict(fit, type = "response")`).

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
fitted(object, marginal = TRUE, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` object.

- marginal:

  If `TRUE` (default), returns marginal expected consumption
  (`type = "demand"` in predict). If `FALSE`, returns
  conditional-on-positive consumption (`type = "response"`).

- ...:

  Unused.

## Value

Numeric vector of length `nobs(object)`.

## See also

[`predict.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_hurdle.md),
[`augment.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_hurdle.md).
