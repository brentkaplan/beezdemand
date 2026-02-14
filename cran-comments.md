## Test environments
* macOS Tahoe 26.2, R 4.5.1 (local)
* TODO: win-builder (devel and release)
* TODO: R-hub (linux, windows)

## R CMD check results
TODO: Update after running fresh R CMD check --as-cran

## Submission comments

This is a major update (0.1.2 -> 0.2.0) with significant new functionality:

* New two-part hurdle demand models via TMB (Template Model Builder)
* New cross-price demand model functions
* New `fit_demand_fixed()` as modern replacement for `FitCurves()`
* New mixed-effects demand modeling with `fit_demand_mixed()`
* Comprehensive broom integration (tidy, glance, augment methods)
* Model comparison, diagnostics, and confidence interval methods
* Six vignettes covering all major workflows

### Package size note
The installed package size is approximately 22 MB, with the `libs`
subdirectory accounting for ~20 MB. This is due to TMB template
compilation for the hurdle and joint hurdle models, which is typical
for packages using TMB.
