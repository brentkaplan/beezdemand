# Get model summaries from a cross-price model

This function extracts model summary statistics from a cross-price
demand model into a single-row data frame, following the conventions of
the broom package. It returns goodness-of-fit measures and other model
information.

## Usage

``` r
# S3 method for class 'cp_model_nls'
glance(x, ...)
```

## Arguments

- x:

  A model object from fit_cp_nls or fit_cp_linear

- ...:

  Additional arguments (unused)

## Value

A one-row data frame with model summary statistics:

- r.squared:

  R-squared value indicating model fit

- aic:

  Akaike Information Criterion

- bic:

  Bayesian Information Criterion

- equation:

  The equation type used in the model

- method:

  The method used to fit the model

- transform:

  The transformation applied to the data, if any

## Examples

``` r
if (FALSE) { # \dontrun{
# Fit a cross-price demand model
model <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE)

# Get model summary statistics
glance(model)
} # }
```
