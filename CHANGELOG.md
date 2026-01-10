# Changelog

All notable changes to this project will be documented in this file.

The format follows Keep a Changelog and this project adheres to Semantic Versioning.

## [Unreleased]

<!-- TODO: Populate entries for unreleased changes once available. -->

## [0.1.3] - 2025-04-20

### Added
- Cross-price demand model suite:
  - `check_unsystematic_cp()` to detect unsystematic cross-price patterns.
  - `fit_cp_nls()` nonlinear cross-price model fitting.
  - `fit_cp_linear()` linear and mixed-effects cross-price model fitting.
  - S3 methods for cross-price objects: `summary()`, `plot()`, `glance()`, `tidy()`.
  - `extract_coefficients()` unified coefficient extraction for model types.
  - `cp_posthoc_slopes()` and `cp_posthoc_intercepts()` for post-hoc comparisons.
  - `validate_cp_data()` to validate and (optionally) filter cross-price datasets.
- Vignette: “How to Use Cross-Price Demand Model Functions” covering data structure,
  unsystematic checks, two-stage and pooled fits, linear/mixed modeling, visualization,
  coefficient extraction, and post-hoc analyses.

### Documentation
- Added a comprehensive vignette demonstrating the cross-price workflow end-to-end.

## [0.1.2] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- `ExtraF()` gained the ability to specify a start value for alpha.

### Changed
- Replaced dependency on `nlmrt` with `nlsr`.

### Fixed
- `CheckUnsystematic()` correctly flags cases when data are passed as a tibble.
- Updated deprecated `ggplot2` arguments to current APIs.

## [0.1.1] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- Experimental `FitCurves()` arguments:
  - `constrainq0` to fit alpha with Q0 constrained.
  - `startq0`, `startalpha` to allow user-specified starting values.

## [0.1.0] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- Initial CRAN release.

## [0.1.00] - YYYY-MM-DD
<!-- Non-SemVer entry retained for historical accuracy. TBD: Add date if known. -->

### Documentation
- Package prepared and submitted to CRAN.

## [0.0.95] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Changed
- **BREAKING:** Output summary column formerly named `ID` is now `id` (lowercase).
  Update scripts to reference `id` accordingly.

### Documentation
- General cleanup toward CRAN readiness; `R CMD check` passes with no errors,
  warnings, or notes.

## [0.0.91] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Performance
- `GetSharedK()` updated for improved performance and robustness via added helpers.

## [0.0.90] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- `ExtraF()` now compares both alpha and Q0.
- Functions now allow specifying column names where appropriate.

## [0.0.85] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- `FitCurves()` supports fitting mean/pooled data via `method` argument.

### Changed
- **BREAKING:** `FitCurves()` no longer accepts in-function data transformations.
  Perform transformations beforehand using `ChangeData()`.
- **BREAKING:** `GetSharedK()` no longer accepts data transformations. Transform data prior to calling.

### Fixed
- `FitCurves()` correctly reports alpha and Q0 standard errors when `k` is fitted
  as a free parameter.

## [0.0.84] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- `ChangeData()` introduced to replace `ReplaceZeros()` and certain transformation
  arguments previously in `FitCurves()`.

### Changed
- **BREAKING (temporary):** `FitCurves()` returns a list object during transition.
  Update scripts to extract the first element if required; this was marked as temporary.

### Documentation
- Updated contact email and minor Rd tidying.

## [0.0.6] - YYYY-MM-DD
<!-- TBD: Add actual release date -->

### Added
- `FitMeanCurves()` for averaged or pooled data; can produce plots.
- `FitCurves()` can produce plots.
- `GetDescriptives()` can produce box-and-whisker plots.

### Removed
- Removed legacy code from an older workflow; all functions now use long-form data.

### Changed
- **BREAKING:** Standardized on long-form data input across the package. Convert data to long
  format before using modeling functions.


---

## Compare Links

<!--
When tags are available, update the compare links below.
The repository URL from DESCRIPTION is https://github.com/brentkaplan/beezdemand
-->

[Unreleased]: https://github.com/brentkaplan/beezdemand/compare/v0.1.2...main
<!-- TODO: Update to compare v0.1.3...main once a v0.1.3 tag is created. -->

[0.1.3]: https://github.com/brentkaplan/beezdemand/compare/v0.1.2...v0.1.3
<!-- TODO: Add tag v0.1.3 (currently not found) to activate this link. -->

[0.1.2]: https://github.com/brentkaplan/beezdemand/compare/v0.1.0...v0.1.2
[0.1.1]: <!-- TODO: Add compare link once a v0.1.1 tag exists. -->
[0.1.0]: https://github.com/brentkaplan/beezdemand/compare/v0.1.0...v0.1.0
<!-- The initial tag compares to itself; replace with previous tag if known. -->
[0.1.00]: <!-- TODO: Non-SemVer entry; add compare link if a matching tag exists. -->
[0.0.95]: <!-- TODO: Add compare link once a v0.0.95 tag exists. -->
[0.0.91]: <!-- TODO: Add compare link once a v0.0.91 tag exists. -->
[0.0.90]: <!-- TODO: Add compare link once a v0.0.90 tag exists. -->
[0.0.85]: <!-- TODO: Add compare link once a v0.0.85 tag exists. -->
[0.0.84]: <!-- TODO: Add compare link once a v0.0.84 tag exists. -->
[0.0.6]:  <!-- TODO: Add compare link once a v0.0.6 tag exists. -->

