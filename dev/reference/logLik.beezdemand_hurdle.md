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
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
logLik(fit)
} # }
```
