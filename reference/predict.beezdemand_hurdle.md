# Predict Method for Hurdle Demand Models

Returns predictions from a fitted hurdle demand model.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
predict(
  object,
  newdata = NULL,
  type = c("response", "link", "parameters", "probability", "demand"),
  prices = NULL,
  se.fit = FALSE,
  interval = c("none", "confidence"),
  level = 0.95,
  ...
)
```

## Arguments

- object:

  An object of class `beezdemand_hurdle`.

- newdata:

  Optional data frame containing a price column matching the fitted
  object's `x_var`. If `newdata` includes the id column,
  subject-specific predictions are returned; otherwise population
  predictions are returned. If `newdata` is `NULL`, returns predictions
  for all subjects across a price grid.

- type:

  One of:

  `"response"`

  :   Predicted consumption (part II)

  `"link"`

  :   Predicted log-consumption (linear predictor of part II)

  `"probability"`

  :   Predicted probability of zero consumption (part I)

  `"demand"`

  :   Predicted expected consumption = (1 - P0) \* response

  `"parameters"`

  :   Subject-specific parameters (no `.fitted` column)

- prices:

  Optional numeric vector of prices used only when `newdata = NULL`.

- se.fit:

  Logical; if `TRUE`, includes a `.se.fit` column (delta-method via
  `sdreport` when available).

- interval:

  One of `"none"` (default) or `"confidence"`.

- level:

  Confidence level when `interval = "confidence"`.

- ...:

  Unused.

## Value

For `type = "parameters"`, a tibble of subject-level parameters.
Otherwise, a tibble containing the `newdata` columns plus `.fitted` and
helper columns `predicted_log_consumption`, `predicted_consumption`,
`prob_zero`, and `expected_consumption`. When requested, `.se.fit` and
`.lower`/`.upper` are included.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   Converged in 81 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 32.81

# Get subject-specific parameters
pars <- predict(fit, type = "parameters")

# Predict demand at specific prices
demand <- predict(fit, type = "demand", prices = c(0, 0.5, 1, 2, 5))
# }
```
