# Summary method for beezdemand_nlme

Returns a structured summary object containing model coefficients, fit
statistics, and random effects information.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
summary(
  object,
  report_space = c("natural", "log10"),
  df_method = c("containment", "between"),
  ...
)
```

## Arguments

- object:

  A beezdemand_nlme object

- report_space:

  Character. Reporting space for core parameters. One of `"natural"` or
  `"log10"` (`match.arg` default `"natural"`). `estimate`/`std.error`
  follow this scale; `statistic`/`p.value` are always on the estimation
  scale (nlme's native containment-t test, which is
  transformation-invariant).

- df_method:

  Character. Degrees of freedom used for the fixed-effect t tests.
  `"containment"` (default) reports nlme's own containment df unchanged.
  nlme's containment rule assigns the observation-level residual degrees
  of freedom to every fixed effect, including between-subject terms (a
  group factor), for which the effective sample size is the number of
  subjects; the default `df` and p-values for such terms are therefore
  anticonservative. `"between"` replaces the df of each between-subject
  term (a coefficient whose design column is constant within every
  subject, excluding the parameter intercepts) with
  `n_subjects - rank(X_between)`, where `X_between` is that parameter's
  subject-level design (intercept plus its between-subject columns), and
  recomputes the p-value from the unchanged t statistic. Intercepts and
  within-subject terms keep containment df in both modes (Pinheiro &
  Bates, 2000, section 2.4.2). Estimates and standard errors never
  change. Satterthwaite / Kenward-Roger df are not available for
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html). The TMB
  backend reports an asymptotic z test instead.

- ...:

  Additional arguments (passed to summary.nlme)

## Value

A `summary.beezdemand_nlme` object (inherits from `beezdemand_summary`)
with fields including:

- `call`: The original function call

- `model_class`: "beezdemand_nlme"

- `backend`: "nlme"

- `equation_form`: The equation form used ("zben" or "simplified")

- `coefficients`: Tibble of fixed effects with std.error, statistic,
  p.value and `df` (the degrees of freedom the p-value uses; see
  `df_method`)

- `df_method`: The `df_method` in effect

- `random_effects`: VarCorr output for random effects

- `logLik`, `AIC`, `BIC`: Model fit statistics

## References

Pinheiro, J. C., & Bates, D. M. (2000). *Mixed-Effects Models in S and
S-PLUS*. Springer. Section 2.4.2.
