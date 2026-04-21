# Plot Loss Profile for a Single Parameter

Plots 1D slices of the SSR surface, fixing one parameter at the MLE and
varying the other.

## Usage

``` r
plot_loss_profile(object, ...)

# S3 method for class 'beezdemand_hurdle'
plot_loss_profile(
  object,
  parameter = c("both", "q0", "alpha"),
  resolution = 200,
  range = c(-3, 3),
  style = c("modern", "apa"),
  ...
)

# S3 method for class 'beezdemand_tmb'
plot_loss_profile(
  object,
  parameter = c("both", "q0", "alpha"),
  resolution = 200,
  range = c(-3, 3),
  style = c("modern", "apa"),
  ...
)

# S3 method for class 'beezdemand_nlme'
plot_loss_profile(
  object,
  parameter = c("both", "q0", "alpha"),
  resolution = 200,
  range = c(-3, 3),
  style = c("modern", "apa"),
  type = c("ssr", "marginal"),
  ...
)
```

## Arguments

- object:

  A fitted model object.

- ...:

  Additional arguments passed to methods.

- parameter:

  Character; which parameter to profile: `"q0"`, `"alpha"`, or `"both"`
  (default).

- resolution:

  Integer; number of grid points (default 200).

- range:

  Numeric vector of length 2; range in log10 units relative to MLE
  (default `c(-3, 3)`).

- style:

  Character; plot style, `"modern"` or `"apa"`.

- type:

  Character; loss profile to plot for NLME models. `"ssr"` (default)
  profiles sum-of-squared-residuals on price-aggregated means;
  `"marginal"` profiles a linearized marginal negative log-likelihood.

## Value

A ggplot2 object. If `parameter = "both"` and patchwork is available,
returns a combined patchwork object.

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
plot_loss_profile(fit, parameter = "q0")

plot_loss_profile(fit, parameter = "both")

# }
```
