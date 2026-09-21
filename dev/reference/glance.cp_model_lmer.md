# Get model summaries from a mixed-effects cross-price model

Get model summaries from a mixed-effects cross-price model

## Usage

``` r
# S3 method for class 'cp_model_lmer'
glance(x, ...)
```

## Arguments

- x:

  A cp_model_lmer object.

- ...:

  Additional arguments passed to broom.mixed::glance.

## Value

A tibble with model summary statistics: the columns of
[`broom.mixed::glance()`](https://generics.r-lib.org/reference/glance.html)
for the underlying `merMod` fit, followed by `converged` (logical;
`TRUE` when lme4 reported no convergence problem, `FALSE` when it did,
`NA` when the fit predates the stored metadata or failed). `converged`
mirrors
[`broom::glance.nls()`](https://broom.tidymodels.org/reference/glance.nls.html)'s
`isConv` so a batch of fits can be screened programmatically;
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) report the same flag.
