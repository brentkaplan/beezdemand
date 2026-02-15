# Augment a beezdemand_nlme Model with Fitted Values and Residuals

Returns the original data with fitted values and residuals from a
nonlinear mixed-effects demand model. This enables easy model
diagnostics and visualization with the tidyverse.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
augment(x, newdata = NULL, ...)
```

## Arguments

- x:

  An object of class `beezdemand_nlme`.

- newdata:

  Optional data frame of new data for prediction. If NULL, uses the
  original data from the model.

- ...:

  Additional arguments (currently unused).

## Value

A tibble containing the original data plus:

- .fitted:

  Fitted values on the model scale (may be transformed, e.g., LL4)

- .resid:

  Residuals on the model scale

- .fixed:

  Fitted values from fixed effects only (population-level)

## Details

The fitted values and residuals are on the same scale as the response
variable used in the model. For \`equation_form = "zben"\`, this is the
LL4-transformed scale. For \`equation_form = "simplified"\` or
\`"exponentiated"\`, this is the natural consumption scale.

To back-transform predictions to the natural scale for "zben" models,
use: \`ll4_inv(augmented\$.fitted)\`

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt)
apt_ll4 <- apt |> dplyr::mutate(y_ll4 = ll4(y))
fit <- fit_demand_mixed(apt_ll4, y_var = "y_ll4", x_var = "x",
                        id_var = "id", equation_form = "zben")
augmented <- augment(fit)

# Plot residuals
library(ggplot2)
ggplot(augmented, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed")
} # }
```
