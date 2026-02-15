# Plot Method for beezdemand_fixed

Plot Method for beezdemand_fixed

## Usage

``` r
# S3 method for class 'beezdemand_fixed'
plot(
  x,
  type = c("demand", "population", "individual", "both"),
  ids = NULL,
  style = c("modern", "apa"),
  show_observed = TRUE,
  show_pred = NULL,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  free_trans = 0.01,
  x_limits = NULL,
  y_limits = NULL,
  n_points = 200,
  x_lab = NULL,
  y_lab = NULL,
  xlab = NULL,
  ylab = NULL,
  facet = NULL,
  observed_point_alpha = 0.5,
  observed_point_size = 1.8,
  pop_line_alpha = 0.9,
  pop_line_size = 1,
  ind_line_alpha = 0.35,
  ind_line_size = 0.7,
  subtitle = NULL,
  ...
)
```

## Arguments

- x:

  A beezdemand_fixed object.

- type:

  Plot type: "demand", "population", "individual", or "both".

- ids:

  Optional vector of subject IDs to plot. Defaults to all subjects.

- style:

  Plot styling, passed to
  [`theme_beezdemand()`](https://brentkaplan.github.io/beezdemand/reference/theme_beezdemand.md).

- show_observed:

  Logical; if TRUE, overlay observed data points where possible.

- show_pred:

  Which prediction layers to plot: "population", "individual", or
  "both".

- x_trans:

  X-axis transform: "log", "log10", "linear", or "pseudo_log".

- y_trans:

  Y-axis transform: "log", "log10", "linear", or "pseudo_log".

- free_trans:

  Value used to display free (x = 0) on log scales. Use NULL to drop x
  \<= 0 values instead.

- x_limits:

  Optional numeric vector of length 2 for x-axis limits.

- y_limits:

  Optional numeric vector of length 2 for y-axis limits.

- n_points:

  Number of points to use for prediction curves when thinning.

- x_lab:

  Optional x-axis label.

- y_lab:

  Optional y-axis label.

- xlab:

  Deprecated alias for `x_lab`.

- ylab:

  Deprecated alias for `y_lab`.

- facet:

  Faceting specification (TRUE for `~id` or a formula).

- observed_point_alpha:

  Alpha for observed points.

- observed_point_size:

  Size for observed points.

- pop_line_alpha:

  Alpha for population curve.

- pop_line_size:

  Line size for population curve.

- ind_line_alpha:

  Alpha for individual curves.

- ind_line_size:

  Line size for individual curves.

- subtitle:

  Optional subtitle for the plot.

- ...:

  Additional arguments (currently unused).

## Value

A ggplot2 object.
