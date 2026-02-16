# ANOVA Method for Hurdle Demand Models

Compare nested hurdle demand models using likelihood ratio tests.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
anova(object, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` model.

- ...:

  Additional `beezdemand_hurdle` models to compare.

## Value

An object of class `anova.beezdemand` containing:

- table:

  Data frame with model comparison statistics

- lrt:

  Likelihood ratio test results

## Details

All models must be fit to the same data. Models are ordered by degrees
of freedom, and sequential likelihood ratio tests are performed.

## Examples

``` r
if (FALSE) { # \dontrun{
fit2 <- fit_demand_hurdle(data, random_effects = c("zeros", "q0"))
fit3 <- fit_demand_hurdle(data, random_effects = c("zeros", "q0", "alpha"))
anova(fit2, fit3)
} # }
```
