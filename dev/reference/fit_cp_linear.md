# Fit a Linear Cross-Price Demand Model

Fit a Linear Cross-Price Demand Model

## Usage

``` r
fit_cp_linear(
  data,
  type = c("fixed", "mixed"),
  formula = NULL,
  log10x = FALSE,
  group_effects = FALSE,
  random_slope = FALSE,
  return_all = TRUE,
  ...
)

fit_cp_linear.default(
  data,
  formula = NULL,
  log10x = FALSE,
  return_all = FALSE,
  ...
)

fit_cp_linear.mixed(
  data,
  formula = NULL,
  log10x = FALSE,
  return_all = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing columns: x and y, and optionally target, id,
  and group.

- type:

  The type of model: "fixed" for standard linear or "mixed" for mixed
  effects.

- formula:

  Optional formula override. If NULL, a formula will be constructed
  based on other parameters.

- log10x:

  Logical; if TRUE and formula is NULL, uses log10(x) instead of x in
  the formula. Default is FALSE.

- group_effects:

  Logical or character; if TRUE, includes group as a factor with
  interactions. Can also be "intercept" for group intercepts only or
  "interaction" for full interactions. Default is FALSE.

- random_slope:

  Logical; for mixed models, if TRUE, includes random slopes for x.
  Default is FALSE.

- return_all:

  Logical; if TRUE, returns additional model metadata.

- ...:

  Additional arguments passed to underlying modeling functions.

## Value

Fitted linear model.
