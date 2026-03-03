# Extract coefficients from a mixed-effects cross-price model in tidy format

Extract coefficients from a mixed-effects cross-price model in tidy
format

## Usage

``` r
# S3 method for class 'cp_model_lmer'
tidy(x, effects = c("fixed", "random", "ran_pars"), ...)
```

## Arguments

- x:

  A cp_model_lmer object.

- effects:

  Which effects to return: "fixed" (default), "random", or "ran_pars".

- ...:

  Additional arguments passed to broom.mixed::tidy.

## Value

A tibble with tidy coefficient information.
