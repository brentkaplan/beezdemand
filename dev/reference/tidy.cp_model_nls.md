# Convert a cross-price model to a tidy data frame of coefficients

This function extracts model coefficients from a cross-price demand
model into a tidy data frame format, following the conventions of the
broom package. It handles cases where model fitting failed gracefully,
returning an empty data frame with the expected structure.

## Usage

``` r
# S3 method for class 'cp_model_nls'
tidy(x, ...)
```

## Arguments

- x:

  A model object from fit_cp_nls or fit_cp_linear

- ...:

  Additional arguments (unused)

## Value

A data frame with one row per coefficient, containing columns:

- term:

  The name of the model parameter

- estimate:

  The estimated coefficient value

- std.error:

  The standard error of the coefficient

- statistic:

  The t-statistic for the coefficient

- p.value:

  The p-value for the coefficient

## Examples

``` r
if (FALSE) { # \dontrun{
# Fit a cross-price demand model
model <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE)

# Get coefficients in tidy format
tidy(model)
} # }
```
