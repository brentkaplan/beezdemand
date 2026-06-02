## Test environments
* Local: macOS, R 4.5.x (release)
* GitHub Actions: Ubuntu-latest (R devel, release, oldrel-1)
* GitHub Actions: macOS-latest (R release)
* GitHub Actions: Windows-latest (R release)
* All Actions runs use `R CMD check --as-cran`.

## R CMD check results
0 errors | 0 warnings | 1 note

The single NOTE is the standard CRAN incoming feasibility check showing the
maintainer email and the installed package size.

## Submission comments

This is a feature release (0.2.0 -> 0.3.0).

New in 0.3.0:

* New TMB (Template Model Builder) mixed-effects modeling tier via
  `fit_demand_tmb()`: automatic differentiation, a Laplace approximation,
  multi-start optimization, optional estimation of the scaling constant `k`,
  and factor-expanded / multi-block (`pdBlocked`) random-effects structures.
* `get_subject_pars()` gains a `beezdemand_nlme` method, completing
  subject-level parameter extraction across all model backends.
* Harmonized estimated-marginal-means and contrast reporting across the TMB
  and NLME backends (consistent by-group naming, comparison metadata).
* New parametric-bootstrap confidence intervals (`boot_demand()`,
  `confint(method = "simulate")`).
* Statistical-correctness fixes to the NLME summary/tidy reporting layer
  (degrees-of-freedom-aware p-values shared between `summary()` and `tidy()`;
  `summary()` convergence status aligned with `glance()`), and corrected
  multi-block correlation placement in `VarCorr()` for TMB fits.
* Additional vignettes (TMB mixed-effects, advanced random-effects structures,
  convergence troubleshooting).

See NEWS.md for the full list, including a small number of intentional
breaking semantic changes (documented there).

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Notes on URLs

`R CMD check` / `urlchecker` may flag two categories of URLs; both are expected:

* Links to the package's own pkgdown article pages
  (`https://brentkaplan.github.io/beezdemand/articles/...`) for vignettes added
  in this release. These resolve once the accompanying documentation site is
  rebuilt on acceptance.
* A few `https://doi.org/...` reference links return 403 to automated checkers
  because the publishers block bots; the DOIs are valid and resolve in a
  browser.

## Package size note

The installed package size is dominated by the `libs` subdirectory from TMB
template compilation (mixed-effects and hurdle demand models), which is typical
for packages using TMB.
