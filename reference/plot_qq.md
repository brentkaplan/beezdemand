# Plot Random Effects Q-Q

Creates Q-Q plots for random effects to assess normality assumptions.

## Usage

``` r
plot_qq(object, which = NULL, ...)

# S3 method for class 'beezdemand_hurdle'
plot_qq(object, which = NULL, ...)

# S3 method for class 'beezdemand_nlme'
plot_qq(object, which = NULL, ...)

# S3 method for class 'beezdemand_tmb'
plot_qq(object, which = NULL, ...)
```

## Arguments

- object:

  A fitted model object with random effects (`beezdemand_hurdle`,
  `beezdemand_nlme`, or `beezdemand_tmb`).

- which:

  Character vector; which random effects to plot. Default is all.

- ...:

  Additional arguments (ignored).

## Value

A ggplot2 object.

## Details

For `beezdemand_tmb` models, Q-Q plots display empirical Bayes
predictions (BLUPs) of the random effects (`b_i` for Q0, `c_i` for
alpha). These exhibit shrinkage toward the population mean: subjects
with fewer observations or higher variability are pulled toward zero.
Consequently, Q-Q plots may appear more normal than the true random
effect distribution (Verbeke & Molenberghs, 2000). Useful for detecting
gross departures (bimodality, heavy tails) but not a definitive
normality test.

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
plot_qq(fit)

# }
```
