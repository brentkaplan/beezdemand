# ANOVA Method for NLME Demand Models

Compare nested NLME demand models using likelihood ratio tests.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
anova(object, ...)
```

## Arguments

- object:

  A `beezdemand_nlme` model.

- ...:

  Additional `beezdemand_nlme` models to compare.

## Value

An object of class `anova.beezdemand` containing model comparison
statistics.

## Details

For NLME models, this method delegates to
[`nlme::anova.lme()`](https://rdrr.io/pkg/nlme/man/anova.lme.html) on
the underlying model objects when possible.

## Examples

``` r
if (FALSE) { # \dontrun{
fit1 <- fit_demand_mixed(data, random_effects = "Q0")
fit2 <- fit_demand_mixed(data, random_effects = c("Q0", "alpha"))
anova(fit1, fit2)
} # }
```
