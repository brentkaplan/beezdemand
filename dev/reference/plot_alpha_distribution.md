# Plot Distribution of Subject-Level Alpha Estimates

Visualizes the distribution of individual-level alpha (elasticity)
estimates from a fitted hurdle model, with the population MLE as a
reference.

## Usage

``` r
plot_alpha_distribution(object, ...)

# S3 method for class 'beezdemand_hurdle'
plot_alpha_distribution(
  object,
  type = c("density", "histogram"),
  log_scale = TRUE,
  bins = 30,
  style = c("modern", "apa"),
  ...
)

# S3 method for class 'beezdemand_tmb'
plot_alpha_distribution(
  object,
  type = c("density", "histogram"),
  log_scale = TRUE,
  bins = 30,
  style = c("modern", "apa"),
  ...
)
```

## Arguments

- object:

  A fitted model object with subject-level parameters.

- ...:

  Additional arguments passed to methods.

- type:

  Character; `"density"` (default) or `"histogram"`.

- log_scale:

  Logical; plot on log10 scale (default `TRUE`).

- bins:

  Integer; histogram bins (default 30).

- style:

  Character; `"modern"` or `"apa"`.

## Value

A ggplot2 object.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
  random_effects = c("zeros", "q0", "alpha"))
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
plot_alpha_distribution(fit)

# }
```
