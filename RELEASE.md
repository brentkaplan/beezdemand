# beezdemand Release Checklist

This document defines the release process and quality gates for
beezdemand.

## Pre-Release Checklist

Before submitting to CRAN, ensure all items are checked:

### Code Quality

[`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
passes with 0 errors, 0 warnings, 0 notes

[`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
passes (all tests green)

`lintr::lint_package()` reports no critical issues

Test coverage is adequate (check with `covr::package_coverage()`)

### Documentation

All exported functions have complete roxygen2 documentation

Examples run without error
([`devtools::run_examples()`](https://devtools.r-lib.org/reference/run_examples.html))

Vignettes build without network access
([`devtools::build_vignettes()`](https://devtools.r-lib.org/reference/build_vignettes.html))

`NEWS.md` updated with changes since last release

`DESCRIPTION` version number bumped appropriately

### Compatibility

Package installs on R \>= 4.1.0 (minimum stated version)

No breaking changes to stable API (or documented in NEWS.md if
intentional)

Deprecated functions emit appropriate warnings

Legacy API tests pass (test-legacy-\*.R files)

### CRAN Compliance

[`urlchecker::url_check()`](https://rdrr.io/pkg/urlchecker/man/url_check.html)
passes

No non-standard files in package root

License file is present and correct

Package does not write to user’s home directory or working directory

Examples complete in \< 5 seconds each (or wrapped in `\donttest{}`)

------------------------------------------------------------------------

## Per-Ticket Definition of Done

Every code change (bug fix, feature, refactor) should meet these
criteria before merging:

### Required

1.  **Tests**: New/modified code has corresponding tests
2.  **Documentation**: Roxygen updated for any API changes
3.  **NEWS entry**: User-facing changes documented in NEWS.md
4.  **R CMD check**: Passes locally with no new warnings

### If Applicable

5.  **Backward compatibility**: Breaking changes documented and
    justified
6.  **Vignette update**: Major features documented in relevant vignette
7.  **Migration note**: Deprecations include migration guidance

------------------------------------------------------------------------

## Version Numbering

beezdemand follows semantic versioning:

- **MAJOR.MINOR.PATCH** (e.g., 0.2.0)
- PATCH: Bug fixes, minor improvements (0.1.3 -\> 0.1.4)
- MINOR: New features, deprecations (0.1.3 -\> 0.2.0)
- MAJOR: Breaking API changes (0.2.0 -\> 1.0.0)

------------------------------------------------------------------------

## Release Steps

### 1. Preparation

``` r
# Update version in DESCRIPTION
# Update NEWS.md with release date
devtools::document()
devtools::check()
```

### 2. Final Verification

``` r
# Full check with CRAN settings
devtools::check(remote = TRUE, manual = TRUE)

# Verify examples run
devtools::run_examples()

# Build and check vignettes
devtools::build_vignettes()
```

### 3. Submit to CRAN

``` r
devtools::release()
```

### 4. Post-Release

Tag release in git: `git tag -a v0.2.0 -m "Release 0.2.0"`

Push tags: `git push origin --tags`

Create GitHub release from tag

Update pkgdown site (automated via GitHub Actions)

------------------------------------------------------------------------

## API Stability Policy

### Stable API (v0.2.0+)

The following functions have stable, tested interfaces:

- [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md) -
  Fixed-effect demand models
- [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md) -
  Mixed-effect demand models
- [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md) -
  Hurdle (zero-inflated) demand models
- S3 methods: [`summary()`](https://rdrr.io/r/base/summary.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html)

### Superseded API

These functions work but are no longer recommended:

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  -\> Use
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
- [`FitMeanCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitMeanCurves.md)
  -\> Use `fit_demand_fixed(agg = "Mean")` or `agg = "Pooled"`

Superseded functions will continue to work through v0.3.x and emit soft
deprecation warnings.

### Experimental API

Functions marked `r lifecycle::badge("experimental")` may change without
notice.
