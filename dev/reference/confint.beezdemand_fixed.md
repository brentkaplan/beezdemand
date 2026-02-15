# Confidence Intervals for Fixed-Effect Demand Model Parameters

Computes confidence intervals for Q0, alpha, and k parameters from
individual demand curve fits. Uses asymptotic normal approximation based
on standard errors when available.

## Usage

``` r
# S3 method for class 'beezdemand_fixed'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  A \`beezdemand_fixed\` object from \[fit_demand_fixed()\].

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all available parameters.

- level:

  Confidence level (default 0.95).

- ...:

  Additional arguments (ignored).

## Value

A tibble with columns: \`id\`, \`term\`, \`estimate\`, \`conf.low\`,
\`conf.high\`, \`level\`.

## Details

For \`beezdemand_fixed\` objects, confidence intervals are computed
using the asymptotic normal approximation: estimate +/- z \* SE. If
standard errors are not available for a parameter, the confidence bounds
will be \`NA\`.

When the underlying NLS fit objects are available (from \`detailed =
TRUE\`), this method attempts to use \`nlstools::confint2()\` for more
accurate profile-based intervals.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
confint(fit)
confint(fit, level = 0.90)
confint(fit, parm = "Q0")
} # }
```
