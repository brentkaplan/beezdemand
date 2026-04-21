# Augment a beezdemand_hurdle Model with Fitted Values and Residuals

Returns the original data with fitted values, residuals, and predictions
from a hurdle demand model. This enables easy model diagnostics and
visualization with the tidyverse.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
augment(x, newdata = NULL, component = c("combined", "continuous"), ...)
```

## Arguments

- x:

  An object of class `beezdemand_hurdle`.

- newdata:

  Optional data frame of new data for prediction. If NULL, uses the
  original data from the model.

- component:

  Character. Which residuals to compute:

  `"combined"`

  :   (Default) Randomized quantile residuals (Dunn & Smyth, 1996) that
      assess both the binary and continuous components simultaneously.
      If the model is correctly specified, these are exactly N(0,1).

  `"continuous"`

  :   Log-scale residuals `log(y) - mu` for positive observations only
      (zeros are NA). Assesses Part II specification.

  `"binary"`

  :   Not returned as residuals; use the `.fitted_prob` column and
      observed binary indicators for calibration diagnostics.

- ...:

  Additional arguments (currently unused).

## Value

A tibble containing the original data plus:

- .fitted:

  Fitted demand values (natural scale)

- .fitted_link:

  Fitted values on log scale (Part II mean)

- .fitted_prob:

  Predicted probability of consumption (1 - P(zero))

- .resid:

  Residuals (type depends on `component`; see above)

- .resid_response:

  Residuals on response scale (y - .fitted)

## Details

### Residual types for hurdle models

The hurdle model has two components, each requiring different diagnostic
approaches:

- **Continuous residuals** (`component = "continuous"`): Standard
  log-scale residuals `log(y) - mu` for observations where y \> 0. Zeros
  are excluded (NA). Assesses whether the lognormal conditional
  distribution is well- specified.

- **Randomized quantile residuals** (`component = "combined"`, default):
  Following Dunn & Smyth (1996), maps each observation through the
  fitted hurdle CDF and then the standard normal quantile function. If
  the model is correctly specified, these residuals are exactly N(0,1)
  regardless of which component generated the observation. For zeros, a
  uniform random variate within `[0, P(zero)]` breaks ties that would
  otherwise create a spike in the QQ plot.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   Converged in 81 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 32.81
augmented <- augment(fit)

# Plot residuals
library(ggplot2)
ggplot(augmented, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed")

# }
```
