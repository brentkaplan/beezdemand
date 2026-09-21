# Extract coefficients from a mixed-effects cross-price model in tidy format

Extract coefficients from a mixed-effects cross-price model in tidy
format

## Usage

``` r
# S3 method for class 'cp_model_lmer'
tidy(x, effects = c("fixed", "ran_vals", "ran_pars", "random"), ...)
```

## Arguments

- x:

  A cp_model_lmer object.

- effects:

  Which effects to return: `"fixed"` (default), `"ran_vals"`
  (conditional modes of the random effects), or `"ran_pars"`
  (random-effect standard deviations and correlations). `"random"` is
  accepted as an alias for `"ran_vals"`.

- ...:

  Additional arguments passed to broom.mixed::tidy.

## Value

A tibble with tidy coefficient information. When the model is `NULL`
(the fit failed) a zero-row tibble with the same columns as the
corresponding successful call is returned.
