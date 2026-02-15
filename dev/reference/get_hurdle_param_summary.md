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
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
get_hurdle_param_summary(fit)
} # }
```
