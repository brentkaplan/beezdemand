# Predict Method for beezdemand_nlme Objects

Generates point predictions from a fitted \`beezdemand_nlme\` model.
Predictions can be made at the population level (fixed effects only) or
group/subject level (fixed + random effects). The output scale depends
on the \`equation_form\` used during model fitting and whether
\`inv_fun\` is applied.

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

  A \`beezdemand_nlme\` object.

- newdata:

  Optional data frame for which to make predictions. Must contain
  \`x_var\` and all \`factors\` specified in the original model. If
  group-level predictions are desired (\`level=1\`), the \`id_var\`
  column from the original fit must also be present in \`newdata\` and
  its levels should correspond to those in the original data for
  meaningful random effect application. If \`NULL\`, predictions are
  made for the data used in fitting the model.

- type:

  One of \`"response"\` (default), \`"link"\`, \`"population"\`, or
  \`"individual"\`. \`"population"\` and \`"individual"\` are aliases
  that set \`level\` to \`0\` or \`1\`, respectively.

- level:

  Integer, prediction level for \`nlme::predict.nlme()\`:

  - \`0\`: Population predictions (based on fixed effects only).

  - \`1\` (or higher, up to number of grouping levels in model):
    Group-specific predictions (fixed effects + random effects for the
    specified \`id_var\` level).

  Default is \`0\`.

- inv_fun:

  Optional function to inverse-transform the predictions. Example: If
  \`y_var\` was log10-transformed during fitting and \`equation_form\`
  like "zben" produces predictions on that log10 scale, \`inv_fun =
  function(x) 10^x\` would convert predictions back to the original
  consumption scale. If \`equation_form\` was "simplified" (which models
  raw Y), \`inv_fun\` might be \`identity\` or not needed if predictions
  are already on the desired scale.

- se.fit:

  Logical; if \`TRUE\`, includes a \`.se.fit\` column (currently \`NA\`
  because standard errors are not implemented for \`beezdemand_nlme\`
  predictions).

- interval:

  One of \`"none"\` (default) or \`"confidence"\`. When requested,
  \`.lower\`/\`.upper\` are returned as \`NA\`.

- interval_level:

  Confidence level when \`interval = "confidence"\`. Currently used only
  for validation.

- ...:

  Additional arguments passed to \`nlme::predict.nlme()\`.

## Value

A tibble containing the original \`newdata\` columns plus \`.fitted\`.
When requested, \`.se.fit\` and \`.lower\`/\`.upper\` are included
(currently \`NA\`).

## See also

[`predict.nlme`](https://rdrr.io/pkg/nlme/man/predict.nlme.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'fit_one_factor' is a successfully fitted beezdemand_nlme object
# (e.g., using equation_form = "zben", y_var = "y_ll4")

if (!is.null(fit_one_factor$model)) {
  # Population-level predictions for the original data
  preds_pop_log_scale <- predict(fit_one_factor, level = 0)

  # Back-transform to natural scale
  preds_pop_natural_scale <- predict(fit_one_factor, level = 0, inv_fun = function(x) 10^x)

  # Create some new data for prediction
  # Ensure all necessary columns (x, factors, id if level=1) are present
  # and factors have levels consistent with the model fit.

  # Example: Predict for the first few rows of original data but at group level
  # Make sure the id and factor levels in new_data_subset exist in original data
  new_data_subset <- fit_one_factor$data[1:5, ]

  preds_group_log_scale <- predict(fit_one_factor, newdata = new_data_subset, level = 1)

  # If your model was, for example:
  # fit_simplified_raw_y <- fit_demand_mixed(data=ko, y_var="y", x_var="x",
  #                                          id_var="id", factors="dose",
  #                                          equation_form="simplified")
  # if (!is.null(fit_simplified_raw_y$model)) {
  #   preds_simplified_raw <- predict(fit_simplified_raw_y) # Already on raw y scale
  # }
}
} # }
```
