# Plot Random Effects Q-Q

Creates Q-Q plots for random effects to assess normality assumptions.

## Usage

``` r
plot_qq(object, which = NULL, ...)

# S3 method for class 'beezdemand_hurdle'
plot_qq(object, which = NULL, ...)

# S3 method for class 'beezdemand_nlme'
plot_qq(object, which = NULL, ...)
```

## Arguments

- object:

  A fitted model object with random effects (\`beezdemand_hurdle\` or
  \`beezdemand_nlme\`).

- which:

  Character vector; which random effects to plot. Default is all.

- ...:

  Additional arguments (ignored).

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
plot_qq(fit)
plot_qq(fit, which = "Q0")
} # }
```
