# Extract Results from TMB Hurdle Fit

Internal function to extract coefficients, standard errors, and derived
quantities from a fitted TMB hurdle model.

## Usage

``` r
.hurdle_extract_estimates(opt, obj, sdr, param_names, n_subjects, n_re)
```

## Arguments

- opt:

  Optimization result from
  [`nlminb()`](https://rdrr.io/r/stats/nlminb.html).

- obj:

  TMB objective function object.

- sdr:

  TMB sdreport object (can be NULL).

- param_names:

  List from
  [`.hurdle_get_param_names()`](https://brentkaplan.github.io/beezdemand/reference/dot-hurdle_get_param_names.md).

- n_subjects:

  Number of subjects.

- n_re:

  Number of random effects.

## Value

A list with coefficients, se, variance_components, correlations, and
random effects matrix.
