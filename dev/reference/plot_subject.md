# Plot Demand Curve for a Single Subject

Creates a demand curve plot for a single subject with optional observed
data and population reference curve.

## Usage

``` r
plot_subject(
  object,
  subject_id,
  prices = NULL,
  show_data = TRUE,
  show_population = TRUE,
  style = c("modern", "apa")
)
```

## Arguments

- object:

  An object of class `beezdemand_hurdle`.

- subject_id:

  The ID of the subject to plot.

- prices:

  Numeric vector of prices for plotting. If `NULL`, uses a sequence from
  0 to max observed price.

- show_data:

  Logical; if `TRUE`, overlay observed data points. Default is `TRUE`.

- show_population:

  Logical; if `TRUE`, show population curve. Default is `TRUE`.

- style:

  Plot styling, passed to
  [`theme_beezdemand()`](https://brentkaplan.github.io/beezdemand/reference/theme_beezdemand.md).

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
plot_subject(fit, subject_id = "1")
} # }
```
