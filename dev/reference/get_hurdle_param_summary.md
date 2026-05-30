# Get Hurdle Model Parameter Summary

Provides summary statistics for subject-level demand parameters from a
hurdle demand model. This is analogous to EMMs but based on empirical
Bayes estimates of subject-specific parameters.

## Usage

``` r
get_hurdle_param_summary(fit_obj, ci_level = 0.95)
```

## Arguments

- fit_obj:

  A `beezdemand_hurdle` object.

- ci_level:

  Confidence level for intervals (default 0.95).

## Value

A data frame with summary statistics for each parameter:

- parameter:

  Parameter name

- mean:

  Mean across subjects

- sd:

  Standard deviation across subjects

- median:

  Median across subjects

- lcl:

  Lower confidence limit (based on percentiles)

- ucl:

  Upper confidence limit (based on percentiles)

- min:

  Minimum value

- max:

  Maximum value

## See also

[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md),
[`get_subject_pars`](https://brentkaplan.github.io/beezdemand/reference/get_subject_pars.md)

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
get_hurdle_param_summary(fit)
#>    parameter        mean          sd      median          lcl         ucl
#> 1         Q0  6.96190164  2.46592004  6.23633363  3.215952224 10.16757497
#> 2      alpha  0.01947681  0.00863862  0.01733102  0.009613567  0.03276271
#> 3 breakpoint 16.99524869  6.50011279 15.06387016  8.420514102 26.82632085
#> 4       Pmax 11.98608909  5.31880020 11.48311653  5.949514052 20.24056587
#> 5       Omax 26.13650188 12.21190919 21.14026966 12.408941449 43.97742007
#>            min         max n_valid
#> 1  2.828143606 10.20941117      10
#> 2  0.009339529  0.03387715      10
#> 3  7.593381759 27.64328021      10
#> 4  5.728784989 20.77994569      10
#> 5 11.775173761 44.18881361      10
# }
```
