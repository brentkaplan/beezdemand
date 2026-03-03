# Extract Log-Likelihood from Hurdle Demand Model

Extracts the log-likelihood from a fitted hurdle demand model. Useful
for likelihood ratio tests comparing nested models.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
logLik(object, ...)
```

## Arguments

- object:

  An object of class `beezdemand_hurdle`.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `logLik` with the log-likelihood value and attributes
for degrees of freedom and number of observations.

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
logLik(fit)
#> 'log Lik.' 32.81453 (df=12)
# }
```
