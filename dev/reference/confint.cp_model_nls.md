# Confidence Intervals for Cross-Price NLS Model Parameters

Computes confidence intervals for parameters from a nonlinear
cross-price demand model using
[`nlstools::confint2()`](https://rdrr.io/pkg/nlstools/man/confint2.html).

## Usage

``` r
# S3 method for class 'cp_model_nls'
confint(
  object,
  parm = NULL,
  level = 0.95,
  method = c("asymptotic", "profile"),
  ...
)
```

## Arguments

- object:

  A `cp_model_nls` object from
  [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md).

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all parameters.

- level:

  Confidence level (default 0.95).

- method:

  Character. Method for computing intervals passed to
  [`nlstools::confint2()`](https://rdrr.io/pkg/nlstools/man/confint2.html):

  - `"asymptotic"` (default): Wald-type asymptotic intervals

  - `"profile"`: Profile-t confidence intervals

- ...:

  Additional arguments passed to
  [`nlstools::confint2()`](https://rdrr.io/pkg/nlstools/man/confint2.html).

## Value

A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `method`.

## Details

This method wraps
[`nlstools::confint2()`](https://rdrr.io/pkg/nlstools/man/confint2.html)
to provide confidence intervals for the log10-parameterized coefficients
(`log10_qalone`, `I`, `log10_beta`).

For back-transformed natural-scale confidence intervals, apply the
transformation: `10^conf.low` and `10^conf.high` for log10-scale
parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE)
confint(fit)
confint(fit, level = 0.90)
confint(fit, method = "profile")  # Profile-t intervals
} # }
```
