# Confidence Intervals for Mixed-Effects Demand Model Parameters

Computes confidence intervals for fixed effect parameters from an
NLME-based mixed-effects demand model.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
confint(object, parm = NULL, level = 0.95, method = c("wald", "profile"), ...)
```

## Arguments

- object:

  A `beezdemand_nlme` object from
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md).

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all fixed effect parameters.

- level:

  Confidence level (default 0.95).

- method:

  Character. Method for computing intervals:

  - `"wald"`: Wald-type intervals using asymptotic normality (default,
    fast)

  - `"profile"`: Profile likelihood intervals via
    [`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html)
    (slower but more accurate for small samples)

- ...:

  Additional arguments passed to
  [`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html)
  when `method = "profile"`.

## Value

A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `component`.

## Details

For Wald intervals, confidence bounds are computed as estimate ± z \* SE
using standard errors from the model summary.

For profile intervals,
[`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html) is
called on the underlying nlme model object. This method provides more
accurate intervals but can be computationally intensive for complex
models.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_mixed(data, y_var = "y", x_var = "x", id_var = "id")
confint(fit)
confint(fit, level = 0.90)
confint(fit, method = "profile")  # More accurate but slower
} # }
```
