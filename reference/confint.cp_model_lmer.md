# Confidence Intervals for a Cross-Price Demand Model (Mixed-Effects)

Wraps
[`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).
Defaults to Wald intervals; `method = "profile"` or `method = "boot"`
use the corresponding lme4 backends and are slower.

## Usage

``` r
# S3 method for class 'cp_model_lmer'
confint(
  object,
  parm = NULL,
  level = 0.95,
  method = c("Wald", "profile", "boot"),
  ...
)
```

## Arguments

- object:

  A `cp_model_lmer` object.

- parm:

  Optional character vector of parameter names. `NULL` returns intervals
  for all parameters.

- level:

  Confidence level (default `0.95`).

- method:

  One of `"Wald"` (default), `"profile"`, or `"boot"`.

- ...:

  Additional arguments passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).

## Value

A tibble with columns `term`, `estimate`, `conf.low`, `conf.high`,
`level`, `method`.
