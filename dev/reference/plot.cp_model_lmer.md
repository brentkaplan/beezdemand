# Plot Method for Mixed-Effects Cross-Price Demand Models

Creates a ggplot2 visualization of a fitted mixed-effects cross-price
demand model (of class `cp_model_lmer`). This function allows you to
plot:

## Usage

``` r
# S3 method for class 'cp_model_lmer'
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
  pred_type = c("fixed", "random", "all"),
  ...
)
```

## Arguments

- x:

  A `cp_model_lmer` object (as returned by
  `fit_cp_linear(type = "mixed", ...)`).

- data:

  Optional data frame containing columns `x` and `y` to be plotted. If
  not provided, `object$data` is used.

- inv_fun:

  Optional function to inverse-transform predictions. Default is
  \`identity\`. Not typically used for linear models but included for
  API consistency.

- n_points:

  Number of points to use in creating the prediction grid. Default is
  `100`.

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

  Size of the observed data points. Default is `3`.

- pred_type:

  Character string specifying which prediction components to plot:

  `"fixed"`

  :   Plot only the fixed-effects (population) prediction.

  `"random"`

  :   Plot only the subject-specific predictions.

  `"all"`

  :   Plot both the fixed-effects and the subject-specific predictions.

  The default is `"fixed"`.

- ...:

  Additional arguments passed to
  [`predict.cp_model_lmer`](https://brentkaplan.github.io/beezdemand/reference/predict.cp_model_lmer.md).

## Value

A ggplot2 object displaying the observed data points along with the
prediction curves.

## Details

- `"fixed"`:

  Only the population-level (fixed-effects) prediction.

- `"random"`:

  Only the subject-specific predictions.

- `"all"`:

  Both: the fixed-effects and the subject-specific predictions.

If the model includes group effects, separate lines will be drawn for
each group.
