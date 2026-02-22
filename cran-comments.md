## Test environments
* macOS Sequoia 26.3, R 4.5.2 (local)
* GitHub Actions: macOS-latest (R release)
* GitHub Actions: Windows-latest (R release)
* GitHub Actions: Ubuntu-latest (R devel, release, oldrel-1)

## R CMD check results
0 errors | 0 warnings | 1 note

The single NOTE is from CRAN incoming feasibility checks: some
pkgdown URLs return 404 because the GitHub Pages site has not yet
been deployed (will be published upon CRAN acceptance).

## Submission comments

This is a major update (0.1.2 -> 0.2.0) with significant new functionality:

* New two-part hurdle demand models via TMB (Template Model Builder)
* New cross-price demand model functions
* New `fit_demand_fixed()` as modern replacement for `FitCurves()`
* New mixed-effects demand modeling with `fit_demand_mixed()`
* Comprehensive broom integration (tidy, glance, augment methods)
* Model comparison, diagnostics, and confidence interval methods
* Nine vignettes covering all major workflows

### Package size note
The installed package size is approximately 22 MB, with the `libs`
subdirectory accounting for ~20 MB. This is due to TMB template
compilation for the hurdle and joint hurdle models, which is typical
for packages using TMB.
