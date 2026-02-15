# Plot a Cross-Price Demand Model (Nonlinear)

Plot a Cross-Price Demand Model (Nonlinear)

## Usage

``` r
# S3 method for class 'cp_model_nls'
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
  inverse_fun = deprecated(),
  ...
)
```

## Arguments

- x:

  A cross-price model object from fit_cp_nls with return_all=TRUE.

- data:

  Optional data frame with x and y; if NULL, uses object\$data.

- inv_fun:

  Optional function to inverse-transform predictions. Default is
  \`identity\`.

- n_points:

  Number of points used for prediction curve.

- title:

  Optional plot title.

- xlab:

  X-axis label.

- ylab:

  Y-axis label.

- x_trans:

  Transformation for x-axis: "identity", "log10", or "pseudo_log".

- y_trans:

  Transformation for y-axis: "identity", "log10", or "pseudo_log".

- point_size:

  Size of data points.

- inverse_fun:

  \`r lifecycle::badge("deprecated")\` Use \`inv_fun\` instead.

- ...:

  Additional arguments (passed to predict).

## Value

A ggplot2 object.
