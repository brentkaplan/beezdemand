# Changelog

All notable changes to this project will be documented in this file.

The format follows Keep a Changelog and this project adheres to Semantic
Versioning.

## [Unreleased](https://github.com/brentkaplan/beezdemand/compare/v0.2.0...HEAD)

### Fixed

- [`get_demand_comparisons()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.md)
  now filters emmeans grid to observed factor combinations before
  computing contrasts, preventing phantom comparisons in unbalanced
  multi-factor designs.

## [0.2.0](https://github.com/brentkaplan/beezdemand/compare/v0.1.3...v0.2.0) - 2026-03-03

### Added

- Four-tier modeling framework: legacy NLS, fixed-effect
  ([`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)),
  mixed-effects NLME
  ([`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)),
  and hurdle via TMB
  ([`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)).
- [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
  modern S3 wrapper for individual NLS demand curves with
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
- Koffarnus equation support in
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
  via `equation_form = "koff"`.
- broom [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods for all model classes (`beezdemand_fixed`,
  `beezdemand_hurdle`, `beezdemand_nlme`).
- [`compare_models()`](https://brentkaplan.github.io/beezdemand/reference/compare_models.md)
  unified model comparison with AIC, BIC, delta metrics, and likelihood
  ratio tests.
- [`anova()`](https://rdrr.io/r/stats/anova.html) S3 methods for nested
  hurdle and NLME model comparison.
- [`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
  diagnostic suite with convergence, boundary, and residual checks for
  all model classes.
- [`plot_residuals()`](https://brentkaplan.github.io/beezdemand/reference/plot_residuals.md)
  and
  [`plot_qq()`](https://brentkaplan.github.io/beezdemand/reference/plot_qq.md)
  diagnostic plot functions.
- Normalized alpha (`alpha_star`; Rzeszutek et al., 2025) across all
  model classes.
- Modern wrappers:
  [`get_empirical_measures()`](https://brentkaplan.github.io/beezdemand/reference/get_empirical_measures.md),
  [`get_descriptive_summary()`](https://brentkaplan.github.io/beezdemand/reference/get_descriptive_summary.md),
  [`get_k()`](https://brentkaplan.github.io/beezdemand/reference/get_k.md).
- [`confint()`](https://rdrr.io/r/stats/confint.html) methods for all
  model classes.
- [`pivot_demand_data()`](https://brentkaplan.github.io/beezdemand/reference/pivot_demand_data.md)
  for wide/long data reshaping.
- Systematicity wrappers:
  [`check_systematic_demand()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_demand.md),
  [`check_systematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_cp.md).
- Migration guide vignette
  ([`vignette("migration-guide")`](https://brentkaplan.github.io/beezdemand/articles/migration-guide.md)).

### Changed

- [`summary()`](https://rdrr.io/r/base/summary.html) methods for hurdle
  and NLME models now return structured S3 objects (use
  `print(summary(fit))` for console output).
- [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
  fits demand parameters in natural-log space; `param_space` argument
  removed.
- [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)
  uses log10-parameterized optimizer coefficients; `"exponential"` form
  fits on the `log10(y)` response scale.
- [`predict.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/predict.cp_model_nls.md)
  always returns `y_pred` on natural scale.
- All [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) methods
  return tibbles with `model_class` and `backend`.
- All summary objects inherit from `beezdemand_summary` base class.
- API standardization: stability contracts for summary, tidy, and glance
  outputs.

### Deprecated

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  superseded by
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).
- [`FitMeanCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitMeanCurves.md)
  superseded by `fit_demand_fixed(agg = ...)`.
- `fit_cp_nls(start_vals=)` deprecated in favor of `start_values=`.

## [0.1.3](https://github.com/brentkaplan/beezdemand/compare/v0.1.2...v0.1.3) - 2025-04-20

### Added

- Cross-price demand model suite:
  - [`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md)
    to detect unsystematic cross-price patterns.
  - [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)
    nonlinear cross-price model fitting.
  - [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)
    linear and mixed-effects cross-price model fitting.
  - S3 methods for cross-price objects:
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
    [`glance()`](https://generics.r-lib.org/reference/glance.html),
    [`tidy()`](https://generics.r-lib.org/reference/tidy.html).
  - [`extract_coefficients()`](https://brentkaplan.github.io/beezdemand/reference/extract_coefficients.md)
    unified coefficient extraction for model types.
  - [`cp_posthoc_slopes()`](https://brentkaplan.github.io/beezdemand/reference/cp_posthoc_slopes.md)
    and
    [`cp_posthoc_intercepts()`](https://brentkaplan.github.io/beezdemand/reference/cp_posthoc_intercepts.md)
    for post-hoc comparisons.
  - [`validate_cp_data()`](https://brentkaplan.github.io/beezdemand/reference/validate_cp_data.md)
    to validate and (optionally) filter cross-price datasets.
- Vignette: “How to Use Cross-Price Demand Model Functions” covering
  data structure, unsystematic checks, two-stage and pooled fits,
  linear/mixed modeling, visualization, coefficient extraction, and
  post-hoc analyses.

### Documentation

- Added a comprehensive vignette demonstrating the cross-price workflow
  end-to-end.

## [0.1.2](https://github.com/brentkaplan/beezdemand/compare/v0.1.0...v0.1.2) - YYYY-MM-DD

### Added

- [`ExtraF()`](https://brentkaplan.github.io/beezdemand/reference/ExtraF.md)
  gained the ability to specify a start value for alpha.

### Changed

- Replaced dependency on `nlmrt` with `nlsr`.

### Fixed

- [`CheckUnsystematic()`](https://brentkaplan.github.io/beezdemand/reference/CheckUnsystematic.md)
  correctly flags cases when data are passed as a tibble.
- Updated deprecated `ggplot2` arguments to current APIs.

## [0.1.1](NA) - YYYY-MM-DD

### Added

- Experimental
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  arguments:
  - `constrainq0` to fit alpha with Q0 constrained.
  - `startq0`, `startalpha` to allow user-specified starting values.

## [0.1.0](https://github.com/brentkaplan/beezdemand/compare/v0.1.0...v0.1.0) - YYYY-MM-DD

### Added

- Initial CRAN release.

## [0.1.00](NA) - YYYY-MM-DD

### Documentation

- Package prepared and submitted to CRAN.

## [0.0.95](NA) - YYYY-MM-DD

### Changed

- **BREAKING:** Output summary column formerly named `ID` is now `id`
  (lowercase). Update scripts to reference `id` accordingly.

### Documentation

- General cleanup toward CRAN readiness; `R CMD check` passes with no
  errors, warnings, or notes.

## [0.0.91](NA) - YYYY-MM-DD

### Performance

- [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)
  updated for improved performance and robustness via added helpers.

## [0.0.90](NA) - YYYY-MM-DD

### Added

- [`ExtraF()`](https://brentkaplan.github.io/beezdemand/reference/ExtraF.md)
  now compares both alpha and Q0.
- Functions now allow specifying column names where appropriate.

## [0.0.85](NA) - YYYY-MM-DD

### Added

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  supports fitting mean/pooled data via `method` argument.

### Changed

- **BREAKING:**
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  no longer accepts in-function data transformations. Perform
  transformations beforehand using
  [`ChangeData()`](https://brentkaplan.github.io/beezdemand/reference/ChangeData.md).
- **BREAKING:**
  [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)
  no longer accepts data transformations. Transform data prior to
  calling.

### Fixed

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  correctly reports alpha and Q0 standard errors when `k` is fitted as a
  free parameter.

## [0.0.84](NA) - YYYY-MM-DD

### Added

- [`ChangeData()`](https://brentkaplan.github.io/beezdemand/reference/ChangeData.md)
  introduced to replace
  [`ReplaceZeros()`](https://brentkaplan.github.io/beezdemand/reference/ReplaceZeros.md)
  and certain transformation arguments previously in
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md).

### Changed

- **BREAKING (temporary):**
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  returns a list object during transition. Update scripts to extract the
  first element if required; this was marked as temporary.

### Documentation

- Updated contact email and minor Rd tidying.

## [0.0.6](NA) - YYYY-MM-DD

### Added

- [`FitMeanCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitMeanCurves.md)
  for averaged or pooled data; can produce plots.
- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  can produce plots.
- [`GetDescriptives()`](https://brentkaplan.github.io/beezdemand/reference/GetDescriptives.md)
  can produce box-and-whisker plots.

### Removed

- Removed legacy code from an older workflow; all functions now use
  long-form data.

### Changed

- **BREAKING:** Standardized on long-form data input across the package.
  Convert data to long format before using modeling functions.

------------------------------------------------------------------------

## Compare Links
