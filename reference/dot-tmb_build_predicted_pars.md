# Rebuild per-row Q0 and alpha from newdata for predict.beezdemand_tmb

For each row of `newdata`, reconstruct the fixed-effect linear predictor
from the stored formula RHS and beta coefficients, add the subject's
random-effect deviate (or zero throughout when `level = "population"`),
and return `Q0 = exp(eta_q0)` and `alpha = exp(eta_alpha)`. This is what
makes [`predict()`](https://rdrr.io/r/stats/predict.html) respect factor
and continuous-covariate values in `newdata`.

## Usage

``` r
.tmb_build_predicted_pars(object, newdata, level = c("subject", "population"))
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

- newdata:

  A data frame with the modeling columns used at fit time (`id_var`,
  `x_var`, factor columns, continuous covariate columns). The `id_var`
  column is not required when `level = "population"`.

- level:

  Character, `"subject"` (default) adds each subject's random-effect
  deviate; `"population"` sets every random effect to zero.

## Value

A list with elements `Q0`, `alpha`, and `log_q0` (each of length
`nrow(newdata)`).
