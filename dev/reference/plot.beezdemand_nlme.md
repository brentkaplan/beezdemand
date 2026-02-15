# Plot Method for beezdemand_nlme Objects

Creates a ggplot2 visualization of a fitted \`beezdemand_nlme\` model,
showing observed data points and/or model prediction lines.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
plot(
  x,
  type = c("demand", "population", "individual", "both"),
  ids = NULL,
  show_observed = TRUE,
  observed_point_alpha = 0.6,
  show_pred = "population",
  n_points = 200,
  inv_fun = identity,
  facet = NULL,
  at = NULL,
  color_by = NULL,
  linetype_by = NULL,
  shape_by = NULL,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  free_trans = 0.01,
  x_limits = NULL,
  y_limits = NULL,
  style = c("modern", "apa"),
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  xlab = NULL,
  ylab = NULL,
  observed_point_size = 2,
  pop_line_size = 1,
  ind_line_size = 0.6,
  pop_line_alpha = 0.9,
  ind_line_alpha = 0.3,
  ...
)
```

## Arguments

- x:

  A \`beezdemand_nlme\` object.

- type:

  Plot type: "demand", "population", "individual", or "both".

- ids:

  Optional vector of subject IDs to plot.

- show_observed:

  Logical. If TRUE, plots the original data points. Default \`TRUE\`.

- observed_point_alpha:

  Alpha for observed points. Default \`0.6\`.

- show_pred:

  Which prediction layers to plot: "population", "individual", or
  "both".

- n_points:

  Integer. Number of points for prediction lines. Default \`100\`.

- inv_fun:

  Optional function to inverse-transform y-axis and predictions. Default
  \`identity\`.

- facet:

  Optional faceting formula (e.g., \`~ dose\`).

- at:

  Optional named list giving values for continuous covariates used in
  the fixed-effects RHS. When building prediction grids for population-
  or individual- level lines, these values will be used. If not
  provided, the function will default to the median of each continuous
  covariate found in the original model data. Factor variables are
  always handled as grids (population) or observed combinations
  (individual) as before.

- color_by:

  Optional character string: name of a factor to color lines and/or
  points by. Must be a column in \`x\$data\`.

- linetype_by:

  Optional character string: name of a factor for linetypes of
  population prediction lines if individual lines are also shown
  (otherwise applies to the shown lines). Must be a model factor in
  \`x\$param_info\$factors\`.

- shape_by:

  Optional character string: name of a factor for shapes of observed
  points. Must be a column in \`x\$data\`.

- x_trans:

  Character. Transformation for x-axis. Default "log".

- y_trans:

  Character. Transformation for y-axis. Default "log".

- free_trans:

  Value used to display free (x = 0) on log scales. Use NULL to drop x
  \<= 0 values instead.

- x_limits:

  Optional numeric vector of length 2 for x-axis limits.

- y_limits:

  Optional numeric vector of length 2 for y-axis limits.

- style:

  Plot styling, passed to
  [`theme_beezdemand()`](https://brentkaplan.github.io/beezdemand/reference/theme_beezdemand.md).

- title:

  Optional plot title.

- subtitle:

  Optional subtitle for the plot.

- x_lab:

  Optional x-axis label.

- y_lab:

  Optional y-axis label.

- xlab:

  Deprecated alias for `x_lab`.

- ylab:

  Deprecated alias for `y_lab`.

- observed_point_size:

  Size for observed points. Default \`2\`.

- pop_line_size:

  Size for population prediction lines. Default \`1\`.

- ind_line_size:

  Size for individual prediction lines. Default \`0.6\`.

- pop_line_alpha:

  Alpha for population prediction lines. Default \`0.9\`.

- ind_line_alpha:

  Alpha for individual prediction lines. Default \`0.3\`.

- ...:

  Additional arguments (currently unused).

## Value

A ggplot2 object.
