# Population-mean response predictions for a TMB demand fit

Rebuilds per-row Q0 and alpha at the fixed-effect coefficients with all
random effects set to zero on the fitting scale (the population mean),
evaluates the demand equation, and (optionally) back-transforms. Does
not require an `id` column in `newdata`.

## Usage

``` r
.tmb_predict_population(object, newdata, scale = "model", correction = TRUE)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

- newdata:

  Data frame with the model's `id`, price, factor and covariate columns.

- scale:

  Character, `"model"` or `"natural"` (see
  [`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md)).

- correction:

  Logical lognormal retransformation flag (see
  [`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md)).

## Value

Numeric vector of fitted values, one per row of `newdata`.
