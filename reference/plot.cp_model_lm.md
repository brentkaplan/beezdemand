# Plot Method for Linear Cross-Price Demand Models

Creates a ggplot2 visualization of a fitted linear cross-price demand
model (of class `cp_model_lm`). The plot overlays a prediction line over
the observed data points. Axis transformations (e.g., `"log10"`) can be
applied. If the model includes group effects, separate lines will be
drawn for each group.

## Usage

``` r
# S3 method for class 'cp_model_lm'
plot(
  x,
  data = NULL,
  inv_fun = identity,
  n_points = 100,
  title = NULL,
  xlab = "Price",
  ylab = "Consumption",
  x_trans = "identity",
  y_trans = "identity",
  point_size = 3,
  ...
)
```

## Arguments

- x:

  A `cp_model_lm` object (as returned by
  `fit_cp_linear(type = "fixed", ...)`).

- data:

  Optional data frame containing columns `x` and `y` to plot. If not
  provided, the function uses `object$data` if available.

- inv_fun:

  Optional function to inverse-transform predictions. Default is
  `identity`. Not typically used for linear models but included for API
  consistency.

- n_points:

  Number of points to create in the prediction grid. Default is `100`.

- title:

  Optional title for the plot; default is `NULL`.

- xlab:

  Label for the x-axis. Default is `"Price"`.

- ylab:

  Label for the y-axis. Default is `"Consumption"`.

- x_trans:

  Transformation for the x-axis; one of `"identity"`, `"log10"`, or
  `"pseudo_log"`. Default is `"identity"`.

- y_trans:

  Transformation for the y-axis; one of `"identity"`, `"log10"`, or
  `"pseudo_log"`. Default is `"identity"`.

- point_size:

  Size of the data points in the plot. Default is `3`.

- ...:

  Additional arguments passed to the generic `predict` method.

## Value

A ggplot2 object displaying the fitted model predictions and observed
data.
