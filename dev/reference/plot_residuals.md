# Plot Residual Diagnostics

Creates diagnostic plots for model residuals including residuals vs
fitted, scale-location, and histogram of residuals.

## Usage

``` r
plot_residuals(object, type = c("all", "fitted", "histogram", "qq"), ...)
```

## Arguments

- object:

  A fitted model object.

- type:

  Character; type of residual plot. One of: - \`"fitted"\`: Residuals vs
  fitted values - \`"histogram"\`: Histogram of residuals - \`"qq"\`:
  Q-Q plot of residuals - \`"all"\`: All plots combined (default)

- ...:

  Additional arguments passed to plotting functions.

## Value

A ggplot2 object or list of ggplot2 objects.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
plot_residuals(fit)
plot_residuals(fit, type = "qq")
} # }
```
