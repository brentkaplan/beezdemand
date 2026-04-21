# Plot Own-Price Elasticity Curve

Computes and plots the own-price point elasticity of demand across
prices. Elasticity is computed numerically via central differences on
the unconditional demand curve.

## Usage

``` r
plot_elasticity(object, ...)

# S3 method for class 'beezdemand_hurdle'
plot_elasticity(
  object,
  prices = NULL,
  n_points = 200,
  show_unit = TRUE,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  free_trans = 0.01,
  x_lab = "Price",
  y_lab = "Elasticity",
  style = c("modern", "apa"),
  ...
)

# S3 method for class 'beezdemand_tmb'
plot_elasticity(
  object,
  prices = NULL,
  n_points = 200,
  show_unit = TRUE,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  free_trans = 0.01,
  x_lab = "Price",
  y_lab = "Elasticity",
  style = c("modern", "apa"),
  ...
)
```

## Arguments

- object:

  A fitted model object.

- ...:

  Additional arguments passed to methods.

- prices:

  Numeric vector of prices. If `NULL`, uses a smooth grid spanning the
  observed price range.

- n_points:

  Integer; number of grid points (default 200).

- show_unit:

  Logical; show unit elasticity reference line at -1 (default `TRUE`).

- x_trans:

  Character; x-axis transformation (default `"log10"`).

- free_trans:

  Numeric; replacement for price = 0 on log scales.

- x_lab, y_lab:

  Axis labels.

- style:

  Character; `"modern"` or `"apa"`.

## Value

A ggplot2 object.

## Details

Point elasticity is computed as: \$\$\eta(P) = \frac{dQ}{dP} \cdot
\frac{P}{Q(P)}\$\$

This uses the unconditional demand Q(P) for hurdle models (which
includes the probability of zero consumption), providing the
economically relevant total elasticity.

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
plot_elasticity(fit)

# }
```
