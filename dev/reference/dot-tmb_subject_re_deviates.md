# Per-subject random-effect deviates for predict.beezdemand_tmb

For each row of `newdata`, returns the random-effect contribution to the
Q0 and alpha linear predictors, looked up by `id`; errors if any id is
not a subject in the fit. Extracted from
[`.tmb_build_predicted_pars()`](https://brentkaplan.github.io/beezdemand/reference/dot-tmb_build_predicted_pars.md)
so the population-level prediction path can skip it entirely.

## Usage

``` r
.tmb_subject_re_deviates(object, newdata, re_parsed)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

- newdata:

  A data frame containing the model's `id` column.

- re_parsed:

  The fit's parsed random-effects specification
  (`object$param_info$random_effects_parsed`); may be `NULL`.

## Value

A list with `re_q0_contrib` and `re_alpha_contrib`, each a numeric
vector of length `nrow(newdata)`.
