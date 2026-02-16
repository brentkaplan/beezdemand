# Confidence Intervals for Hurdle Demand Model Parameters

Computes confidence intervals for fixed effect parameters from a
TMB-based hurdle demand model using the asymptotic normal approximation.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
confint(
  object,
  parm = NULL,
  level = 0.95,
  report_space = c("internal", "natural"),
  ...
)
```

## Arguments

- object:

  A `beezdemand_hurdle` object from
  [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md).

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all fixed effect parameters.

- level:

  Confidence level (default 0.95).

- report_space:

  Character. Reporting space for parameters:

  - `"internal"`: parameters on internal/fitting scale (log for Q0,
    alpha)

  - `"natural"`: back-transformed to natural scale

- ...:

  Additional arguments (ignored).

## Value

A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `component`, `estimate_scale`.

## Details

Confidence intervals are computed using the asymptotic normal
approximation based on standard errors from
[`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html). For
parameters estimated on the log scale (Q0, alpha, k), intervals can be
back-transformed to the natural scale using `report_space = "natural"`.

The transformation uses:

- For log-scale parameters: exp(estimate +/- z \* SE)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
confint(fit)
confint(fit, level = 0.90)
confint(fit, report_space = "natural")
} # }
```
