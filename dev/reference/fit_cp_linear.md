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

## Examples

``` r
# \donttest{
data(etm)
## Fixed-effects linear cross-price model
fit_fixed <- fit_cp_linear(etm, type = "fixed", group_effects = TRUE)
summary(fit_fixed)
#> Linear Cross-Price Demand Model Summary
#> =======================================
#> 
#> Formula: y ~ x * group 
#> Method: lm 
#> 
#> Coefficients:
#>                         Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)             0.400000   1.590686  0.2515  0.80168  
#> x                       0.016667   0.052731  0.3161  0.75223  
#> groupCombustibles       4.594527   2.249569  2.0424  0.04224 *
#> groupE-Cigarettes       1.586070   2.249569  0.7051  0.48148  
#> groupNon-Combustibles   3.572637   2.249569  1.5881  0.11362  
#> x:groupCombustibles     0.073276   0.074572  0.9826  0.32682  
#> x:groupE-Cigarettes     0.010981   0.074572  0.1473  0.88306  
#> x:groupNon-Combustibles 0.075906   0.074572  1.0179  0.30979  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> R-squared: 0.09809   Adjusted R-squared: 0.07087 

## Mixed-effects linear cross-price model
fit_mixed <- fit_cp_linear(etm, type = "mixed", group_effects = TRUE)
summary(fit_mixed)
#> Mixed-Effects Linear Cross-Price Demand Model Summary
#> ====================================================
#> 
#> Formula: y ~ x * group + (1 | id) 
#> Method: lmer 
#> 
#> Fixed Effects:
#>                         Estimate Std. Error t value
#> (Intercept)             0.400000   2.045999  0.1955
#> x                       0.016667   0.044684  0.3730
#> groupCombustibles       4.594527   1.906295  2.4102
#> groupE-Cigarettes       1.586070   1.906295  0.8320
#> groupNon-Combustibles   3.572637   1.906295  1.8741
#> x:groupCombustibles     0.073276   0.063193  1.1596
#> x:groupE-Cigarettes     0.010981   0.063193  0.1738
#> x:groupNon-Combustibles 0.075906   0.063193  1.2012
#> 
#> Random Effects:
#>     Group        Term Variance  Std.Dev       NA
#>        id (Intercept)     <NA> 23.69132 4.867373
#>  Residual        <NA>     <NA> 56.18664 7.495775
#> 
#> Model Fit:
#> R2 (marginal): 0.09372   [Fixed effects only]
#> R2 (conditional): 0.3625   [Fixed + random effects]
#> AIC: 1692 
#> BIC: 1727 
#> 
#> Note: R2 values for mixed models are approximate.
# }
```
