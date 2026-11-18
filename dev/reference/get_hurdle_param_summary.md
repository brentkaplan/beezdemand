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
#>    parameter        mean           sd      median          lcl         ucl
#> 1         Q0  6.96190165  2.465920051  6.23633364  3.215952220 10.16757499
#> 2      alpha  0.01947681  0.008638621  0.01733102  0.009613567  0.03276271
#> 3 breakpoint 16.99524919  6.500113356 15.06387018  8.420514131 26.82632360
#> 4       Pmax 11.98608915  5.318800213 11.48311654  5.949514071 20.24056593
#> 5       Omax 26.13650188 12.211909133 21.14026971 12.408941471 43.97741998
#>           min         max n_valid
#> 1  2.82814360 10.20941119      10
#> 2  0.00933953  0.03387715      10
#> 3  7.59338174 27.64328373      10
#> 4  5.72878501 20.77994575      10
#> 5 11.77517376 44.18881350      10
# }
```
