# Predict Method for beezdemand_nlme Objects

Generates point predictions from a fitted `beezdemand_nlme` model.
Predictions can be made at the population level (fixed effects only) or
group/subject level (fixed + random effects). The output scale depends
on the `equation_form` used during model fitting and whether `inv_fun`
is applied.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
predict(
  object,
  newdata = NULL,
  type = c("response", "link", "population", "individual"),
  level = 0,
  inv_fun = identity,
  se.fit = FALSE,
  interval = c("none", "confidence"),
  interval_level = 0.95,
  ...
)
```

## Arguments

- object:

  A `beezdemand_nlme` object.

- newdata:

  Optional data frame for which to make predictions. Must contain
  `x_var` and all `factors` specified in the original model. If
  group-level predictions are desired (`level=1`), the `id_var` column
  from the original fit must also be present in `newdata` and its levels
  should correspond to those in the original data for meaningful random
  effect application. If `NULL`, predictions are made for the data used
  in fitting the model.

- type:

  One of `"response"` (default), `"link"`, `"population"`, or
  `"individual"`. `"population"` and `"individual"` are aliases that set
  `level` to `0` or `1`, respectively.

- level:

  Integer, prediction level for
  [`nlme::predict.nlme()`](https://rdrr.io/pkg/nlme/man/predict.nlme.html):

  - `0`: Population predictions (based on fixed effects only).

  - `1` (or higher, up to number of grouping levels in model):
    Group-specific predictions (fixed effects + random effects for the
    specified `id_var` level).

  Default is `0`.

- inv_fun:

  Optional function to inverse-transform the predictions. Example: If
  `y_var` was log10-transformed during fitting and `equation_form` like
  "zben" produces predictions on that log10 scale,
  `inv_fun = function(x) 10^x` would convert predictions back to the
  original consumption scale. If `equation_form` was "simplified" (which
  models raw Y), `inv_fun` might be `identity` or not needed if
  predictions are already on the desired scale.

- se.fit:

  Logical; if `TRUE`, includes a `.se.fit` column (currently `NA`
  because standard errors are not implemented for `beezdemand_nlme`
  predictions).

- interval:

  One of `"none"` (default) or `"confidence"`. When requested,
  `.lower`/`.upper` are returned as `NA`.

- interval_level:

  Confidence level when `interval = "confidence"`. Currently used only
  for validation.

- ...:

  Additional arguments passed to
  [`nlme::predict.nlme()`](https://rdrr.io/pkg/nlme/man/predict.nlme.html).

## Value

A tibble containing the original `newdata` columns plus `.fitted`. When
requested, `.se.fit` and `.lower`/`.upper` are included (currently
`NA`).

## See also

[`predict.nlme`](https://rdrr.io/pkg/nlme/man/predict.nlme.html)

## Examples

``` r
# \donttest{
data(ko)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
                        id_var = "monkey", equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 2 (Q0: 1, alpha: 1)
# Population-level predictions
preds <- predict(fit, level = 0)

# Subject-level predictions
preds_subj <- predict(fit, level = 1)
# }
```
