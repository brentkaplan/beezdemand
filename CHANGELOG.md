# Changelog

All notable changes to this project will be documented in this file.

The format follows Keep a Changelog and this project adheres to Semantic
Versioning.

## [Unreleased](https://github.com/brentkaplan/beezdemand/compare/v0.1.2...main)

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
