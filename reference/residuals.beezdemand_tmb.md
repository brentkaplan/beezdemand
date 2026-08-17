# Residuals for a beezdemand_tmb fit

Default returns response residuals (`y_on_scale - fitted`) on the
model's native scale. `type = "pearson"` divides by the residual SD on
the model scale (`exp(coef[["logsigma_e"]])`). Requesting
`type = "pearson"` with `scale = "natural"` falls back to
`type = "response"` with a message because a response-scale residual SD
is not identified for the exponential/zben variants without a separate
variance assumption.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
residuals(
  object,
  type = c("response", "pearson"),
  scale = c("model", "natural"),
  level = c("subject", "population"),
  ...
)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- type:

  One of `"response"` (default) or `"pearson"`.

- scale:

  One of `"model"` (default) or `"natural"`.

- level:

  One of `"subject"` (default; conditions on the subject random effects)
  or `"population"` (random effects set to zero, giving the
  population-mean values). See
  [`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md).

- ...:

  Unused.

## Value

Numeric vector of length `nobs(object)`.

## See also

[`fitted.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fitted.beezdemand_tmb.md),
[`augment.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(residuals(fit))
#> [1] -0.005808747  0.040421837  0.086273319 -0.091394740 -0.046292157
#> [6] -0.001559425
head(residuals(fit, type = "pearson"))
#> [1] -0.04082061  0.28406198  0.60628047 -0.64227094 -0.32531530 -0.01095876
# }
```
