# Residuals for a beezdemand_hurdle fit

Response-scale residuals against the marginal (default) or conditional
fitted values. `type = "pearson"` returns the Part-II standardized
residual on the model (log-consumption) scale,
`(log(y) - mu_i) / sigma_e`, where `mu_i` is the subject-conditional
linear predictor (`predict(type = "link")`) and `sigma_e` is
`exp(coef[["logsigma_e"]])`; observations with `y = 0` have no Part-II
residual and are `NA`. (Before 0.3.0 the raw-scale residual was divided
by the log-scale `sigma_e`, which changed with the consumption unit.)

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
