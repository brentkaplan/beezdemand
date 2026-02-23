# Fit a Linear Cross-Price Demand Model

Fit a Linear Cross-Price Demand Model

## Usage

``` r
fit_cp_linear(
  data,
  type = c("fixed", "mixed"),
  x_var = "x",
  y_var = "y",
  id_var = "id",
  group_var = "group",
  target_var = "target",
  filter_target = TRUE,
  target_level = "alt",
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

  A data frame containing columns for price, consumption, and optionally
  target indicator, subject identifier, and group.

- type:

  The type of model: `"fixed"` for standard linear or `"mixed"` for
  mixed effects.

- x_var:

  Character string; name of the price column. Default is `"x"`. Renamed
  to `"x"` internally.

- y_var:

  Character string; name of the consumption column. Default is `"y"`.
  Renamed to `"y"` internally.

- id_var:

  Character string; name of the subject identifier column. Default is
  `"id"`. Required for mixed models; renamed to `"id"` internally.

- group_var:

  Character string; name of the group column. Default is `"group"`.
  Renamed to `"group"` internally when present.

- target_var:

  Character string; name of the target indicator column. Default is
  `"target"`. Renamed to `"target"` internally when present.

- filter_target:

  Logical; if TRUE (default), filters to rows where the target column
  equals `target_level`.

- target_level:

  Character string; value of the target column to retain when
  `filter_target = TRUE`. Default is `"alt"`.

- formula:

  Optional formula override. If NULL, a formula will be constructed
  based on other parameters. If non-NULL and any `*_var` argument
  differs from its default, an error is thrown because the formula
  references canonical column names that no longer exist before
  renaming; rename columns before calling, or omit the `formula`
  argument.

- log10x:

  Logical; if TRUE and formula is NULL, uses `log10(x)` instead of `x`
  in the formula. Default is FALSE.

- group_effects:

  Logical or character; if TRUE, includes group as a factor with
  interactions. Can also be `"intercept"` for group intercepts only or
  `"interaction"` for full interactions. Default is FALSE.

- random_slope:

  Logical; for mixed models, if TRUE, includes random slopes for x.
  Default is FALSE.

- return_all:

  Logical; if TRUE, returns additional model metadata.

- ...:

  Additional arguments passed to underlying modeling functions.

## Value

Fitted linear model.
