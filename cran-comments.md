## Test environments
* macOS Tahoe 26.3, R 4.5.2 (local)
* GitHub Actions: macOS-latest (R release)
* GitHub Actions: Windows-latest (R release)
* GitHub Actions: Ubuntu-latest (R devel, release, oldrel-1)

## R CMD check results
0 errors | 0 warnings | 1 note

The single NOTE is the standard CRAN incoming feasibility check
showing the maintainer email and tarball size.

## Submission comments

This is a major update (0.1.2 -> 0.2.0) with significant new functionality.
(Version 0.1.3 was a GitHub-only release, never submitted to CRAN.)

New in 0.2.0:

* New two-part hurdle demand models via TMB (Template Model Builder)
* New cross-price demand model functions
* New `fit_demand_fixed()` as modern replacement for `FitCurves()`
* New mixed-effects demand modeling with `fit_demand_mixed()`
* Comprehensive broom integration (tidy, glance, augment methods)
* Model comparison, diagnostics, and confidence interval methods
* Nine vignettes covering all major workflows

### Package size note
The installed package size is approximately 21 MB, with the `libs`
subdirectory accounting for ~12 MB. This is due to TMB (Template
Model Builder) template compilation for the hurdle demand models,
which is typical for packages using TMB.
