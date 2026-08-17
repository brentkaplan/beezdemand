# Residuals for a beezdemand_hurdle fit

Response-scale residuals against the marginal (default) or conditional
fitted values. `type = "pearson"` divides by the residual SD
`exp(coef[["logsigma_e"]])`.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
residuals(object, type = c("response", "pearson"), marginal = TRUE, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` object.

- type:

  One of `"response"` (default) or `"pearson"`.

- marginal:

  Passed to
  [`fitted.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fitted.beezdemand_hurdle.md).
  Default `TRUE`.

- ...:

  Unused.

## Value

Numeric vector of length `nobs(object)`.

## See also

[`fitted.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fitted.beezdemand_hurdle.md).
