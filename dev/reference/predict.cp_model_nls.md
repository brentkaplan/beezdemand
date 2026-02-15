# Predict from a Cross-Price Demand Model (Nonlinear)

Predict from a Cross-Price Demand Model (Nonlinear)

## Usage

``` r
# S3 method for class 'cp_model_nls'
predict(
  object,
  newdata = NULL,
  inv_fun = identity,
  inverse_fun = deprecated(),
  ...
)
```

## Arguments

- object:

  A cross-price model object from fit_cp_nls with return_all=TRUE.

- newdata:

  A data frame containing an 'x' column.

- inv_fun:

  Optional inverse transformation function. Default is \`identity\`.

- inverse_fun:

  \`r lifecycle::badge("deprecated")\` Use \`inv_fun\` instead.

- ...:

  Additional arguments.

## Value

A data frame with x values and predicted y values.
