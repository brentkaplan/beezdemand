# Fitted values for a beezdemand_tmb fit

Default returns fitted values on the model's native likelihood scale
(log scale for `"exponential"`, natural/LL4 scale for others), matching
`augment(fit)$.fitted`. Set `scale = "natural"` to back-transform.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
fitted(
  object,
  scale = c("model", "natural"),
  level = c("subject", "population"),
  ...
)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- scale:

  One of `"model"` (default) or `"natural"`.

- level:

  Reserved for TICKET-014. Currently `"subject"` only.

- ...:

  Unused.

## Value

Numeric vector of length `nobs(object)`.

## See also

[`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md),
[`augment.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_tmb.md),
[`residuals.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/residuals.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(fitted(fit))
#> [1] 2.308394 2.262163 2.216312 2.170836 2.125734 2.081001
# }
```
