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
# \donttest{
data(etm)
fit <- fit_cp_nls(etm, equation = "exponentiated")
tidy(fit)
#> # A tibble: 3 × 5
#>   term         estimate std.error statistic  p.value
#>   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
#> 1 log10_qalone    0.743    0.0704    10.6   1.35e-21
#> 2 I              -1.16     1.20      -0.964 3.36e- 1
#> 3 log10_beta     -0.546    0.395     -1.38  1.67e- 1
# }
```
