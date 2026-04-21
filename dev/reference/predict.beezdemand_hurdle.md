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
  marginal = FALSE,
  marginal_method = c("kde", "normal", "empirical"),
  correction = TRUE,
  seed = 42L,
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

- marginal:

  Logical; if `TRUE`, computes population-averaged (marginal)
  predictions by integrating over the random effects distribution. For
  `type = "probability"`, uses KDE/Normal/Empirical integration of the
  binary component. For `type = "response"` and `type = "demand"`, uses
  Monte Carlo integration over all random effects, producing the
  **population-average** demand curve (accounting for Jensen's
  inequality). Default is `FALSE`, which gives conditional (RE = 0)
  predictions representing a "typical" subject at the center of the RE
  distribution.

- marginal_method:

  Character. Method for marginal integration; one of `"kde"` (default,
  kernel density estimate of BLUPs), `"normal"` (integrate over the
  model-assumed N(0, sigma_a) distribution), or `"empirical"` (simple
  average over BLUPs). Ignored when `marginal = FALSE`.

- correction:

  Logical; if `TRUE` (default), applies the lognormal retransformation
  correction `exp(sigma_e^2 / 2)` when back-transforming from the log
  scale to the natural consumption scale. This produces the **arithmetic
  mean** (conditional on Q \> 0). Set to `FALSE` to obtain the
  **median** (geometric mean), which is useful for individual-level
  "most likely" predictions. Only applies to `type = "response"` and
  `type = "demand"`.

- seed:

  Integer or `NULL`. Random seed for Monte Carlo marginal predictions
  (default `42L`). Set to `NULL` to use current RNG state. The global
  RNG state is preserved and restored after the call.

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

For `type = "parameters"`, a tibble of subject-level parameters. For
`type = "probability"` with `marginal = TRUE`, a tibble with columns for
price, `prob_zero`, and `.fitted` (no subject column). Otherwise, a
tibble containing the `newdata` columns plus `.fitted` and helper
columns `predicted_log_consumption`, `predicted_consumption`,
`prob_zero`, and `expected_consumption`. When requested, `.se.fit` and
`.lower`/`.upper` are included.

## Details

### Retransformation correction

The hurdle model specifies Gaussian errors on log-consumption (Part II):
`log(Q) ~ N(mu, sigma_e^2)`. The conditional distribution of Q given Q
\> 0 is therefore lognormal. The arithmetic mean of a lognormal is
`exp(mu + sigma_e^2/2)`, not `exp(mu)`. Using `exp(mu)` returns the
**median** (geometric mean), which systematically underestimates the
arithmetic mean by a factor of `exp(sigma_e^2/2)`. This correction is
applied by default when `type = "response"` or `type = "demand"`. Set
`correction = FALSE` to obtain the median instead.

This is a parametric correction assuming normality of log-scale
residuals (Duan, 1983). Under the model's normality assumption, this is
equivalent to Duan's nonparametric smearing estimator.

### Marginal P(zero)

The conditional P(zero) curve (when `marginal = FALSE`) sets the random
intercept to zero, which produces a near step-function that
misrepresents the observed fraction of zero responses. The marginal
curve integrates over the random effect distribution, answering "what
fraction of the population has stopped buying at this price?"

The `"kde"` and `"empirical"` methods integrate over empirical Bayes
estimates (BLUPs) of the random intercepts. BLUPs are shrunk toward zero
compared to the true random effects, so these methods slightly
underestimate the RE variance. In practice, this shrinkage bias is often
smaller than the bias from assuming normality when the true RE
distribution is non-normal. The `"normal"` method integrates over the
model-assumed N(0, sigma_a) distribution, which is correct under the
model but may be wrong if the normality assumption is violated. Use
[`plot_qq()`](https://brentkaplan.github.io/beezdemand/reference/plot_qq.md)
to assess RE normality.

### Conditional vs. marginal demand predictions

Population-level demand predictions (when no subject ID is provided) can
be computed in two ways:

- **Conditional (default, `marginal = FALSE`):** Sets all random effects
  to zero and evaluates the demand function at the fixed-effect
  (population) parameters. For nonlinear models, this corresponds to the
  **conditional mode**, not the population-average mean.

- **Marginal (`marginal = TRUE`):** Integrates the prediction over the
  estimated random-effects distribution via Monte Carlo sampling. This
  gives the **population-average** demand curve. Due to Jensen's
  inequality, this curve lies above the conditional curve when the
  demand function is convex in the random effects (which it is for
  exponential demand with log-normal Q0 and alpha).

The conditional prediction is appropriate for characterizing a "typical"
subject. The marginal prediction is appropriate for predicting aggregate
consumption in a population.

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
