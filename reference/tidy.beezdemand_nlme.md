# Tidy method for beezdemand_nlme

Tidy method for beezdemand_nlme

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
tidy(
  x,
  effects = c("fixed", "ran_pars"),
  report_space = c("natural", "log10"),
  df_method = c("containment", "between"),
  ...
)
```

## Arguments

- x:

  A beezdemand_nlme object

- effects:

  Character. Which effects to include: `"fixed"`, `"ran_pars"`, or both
  (the default).

- report_space:

  Character. Reporting space for core parameters. One of `"natural"` or
  `"log10"` (`match.arg` default `"natural"`). `estimate`/`std.error`
  follow this scale; `statistic`/`p.value` are always on the estimation
  scale (nlme's native containment-t test, which is
  transformation-invariant).

- df_method:

  Character. `"containment"` (default) reports nlme's own df, which uses
  the observation-level residual df for between-subject terms as well,
  so their p-values are anticonservative; `"between"` substitutes
  `n_subjects - rank(X_between)` for between-subject terms and
  recomputes their p-values. See
  [`summary.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_nlme.md)
  for the rule.

- ...:

  Additional arguments (ignored)

## Value

A tibble of model terms with columns:

- `term`: Parameter name

- `estimate`: Point estimate. For `component == "variance"` rows this is
  a *standard deviation* (pulled from
  `nlme::VarCorr(model)[, "StdDev"]`), matching
  [`tidy.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_tmb.md)
  and the
  [`broom.mixed::tidy.lme`](https://rdrr.io/pkg/broom.mixed/man/nlme_tidiers.html)
  convention.

- `std.error`: Standard error (`NA` for variance components)

- `statistic`: t-value (`NA` for variance components)

- `p.value`: P-value (`NA` for variance components)

- `df`: Degrees of freedom the p-value uses (`NA` for variance
  components); see `df_method`

- `component`: `"fixed"` or `"variance"`

- `estimate_scale`: Scale that `estimate` is reported on

- `term_display`: Display label for `term`

- `estimate_internal`: Pre-transform estimate; present whenever
  `effects` includes `"fixed"`
