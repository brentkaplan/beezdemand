# Evaluate the demand equation from rebuilt per-row parameters

Shared back end for
[`.tmb_predict_subject()`](https://brentkaplan.github.io/beezdemand/reference/dot-tmb_predict_subject.md)
and
[`.tmb_predict_population()`](https://brentkaplan.github.io/beezdemand/reference/dot-tmb_predict_population.md):
evaluates the fit's equation at the supplied per-row parameters and
applies the natural-scale back-transformation when requested.

## Usage

``` r
.tmb_response_from_pars(object, newdata, bp, scale, correction)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

- newdata:

  Data frame with the model's `id`, price, factor and covariate columns.

- bp:

  List with `Q0`, `alpha`, and `log_q0` (the output of
  [`.tmb_build_predicted_pars()`](https://brentkaplan.github.io/beezdemand/reference/dot-tmb_build_predicted_pars.md)).

- scale:

  Character, `"model"` or `"natural"` (see
  [`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md)).

- correction:

  Logical lognormal retransformation flag (see
  [`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md)).

## Value

Numeric vector of fitted values, one per row of `newdata`.
