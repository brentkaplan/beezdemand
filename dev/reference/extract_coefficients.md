# Extract All Coefficient Types from Cross-Price Demand Models

A convenience function to extract coefficients from any type of
cross-price demand model in a unified format. For mixed effects models,
returns a list with different coefficient types.

## Usage

``` r
extract_coefficients(object, ...)
```

## Arguments

- object:

  A cross-price demand model object (cp_model_nls, cp_model_lm, or
  cp_model_lmer)

- ...:

  Additional arguments passed to the appropriate coef method

## Value

For cp_model_nls and cp_model_lm, returns the model coefficients. For
cp_model_lmer, returns a list with fixed, random, and combined
coefficients.

## Examples

``` r
# \donttest{
data(etm, package = "beezdemand")
fit <- fit_cp_nls(etm, equation = "exponentiated")
extract_coefficients(fit)
#> log10_qalone            I   log10_beta 
#>     0.743096    -1.158117    -0.546300 
# }
```
