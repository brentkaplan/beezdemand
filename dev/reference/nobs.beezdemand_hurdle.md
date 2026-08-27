# Sample size for a beezdemand_hurdle fit

Universal-accessor parity with `nobs.beezdemand_tmb()` and the
cross-price classes. `broom::glance(fit)$nobs` and `BIC(fit)` were
already correct via their own paths (`param_info$n_obs` and the `nobs`
attribute on [`logLik()`](https://rdrr.io/r/stats/logLik.html),
respectively); this method closes the gap for any caller that consumes
[`nobs()`](https://rdrr.io/r/stats/nobs.html) directly.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
nobs(object, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` object.

- ...:

  Unused.

## Value

Integer scalar, the number of observations the model was fit on.
