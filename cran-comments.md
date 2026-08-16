## Test environments
* Local: macOS (Apple Silicon), R 4.5.x (release), `R CMD check --as-cran`
  on the built tarball with `--run-donttest`.
* GitHub Actions: Ubuntu-latest (R devel, release, oldrel-1), macOS-latest
  (R release), Windows-latest (R release) -- all `R CMD check --as-cran`.
* win-builder: R-devel and R-release (results summarised below once run).
* R-hub v2: linux, macos-arm64, windows, clang-asan, valgrind (the package
  ships TMB C++ templates in `src/`; sanitizer/valgrind legs are run
  because those templates changed since 0.2.0).

## R CMD check results
0 errors | 0 warnings | 1 note

The single NOTE is the CRAN incoming feasibility check (maintainer email,
installed size; see the package size note below). Links to the package's
own pkgdown article pages for vignettes new in this release resolve once the
documentation site is rebuilt on acceptance.

## Submission comments

This is a feature release (0.2.0 -> 0.3.0). It is large; NEWS.md gives the
full per-change detail. Headline changes:

* New TMB (Template Model Builder) mixed-effects modeling tier via
  `fit_demand_tmb()`: automatic differentiation, a Laplace approximation,
  multi-start optimization, optional estimation of the scaling constant `k`,
  factor-expanded / multi-block (`pdBlocked`) random-effects structures and
  continuous within-subject random slopes, with the full post-hoc surface
  (estimated marginal means, contrasts, subject-level parameters,
  parametric-bootstrap CIs, diagnostics).
* Monte Carlo power analysis (`power_demand()`, `find_n_demand()`) for
  within- and between-subject demand designs. The accompanying vignette is
  precomputed so it does not run simulations at build time; the `\donttest`
  examples run in a few seconds each.
* Bug fixes that can change estimates relative to 0.2.0 (documented in a
  dedicated NEWS subsection with the exact conditions and a pointer to
  pinning the previous version): multi-start rescue is now the default in
  `fit_demand_fixed()`, `zben` Pmax/Omax are computed numerically, an EV
  exponent fix, and several legacy `FitCurves()` batch-processing fixes.
* Inference surfaces for TMB/hurdle/NLME fits now refuse or flag results
  from non-converged / non-positive-definite fits instead of reporting them
  silently; a number of silent-failure paths in the hurdle, cross-price and
  legacy fitters now degrade gracefully or name their cause.
* One intentional breaking change: `predict()` for hurdle fits defaults to
  `type = "demand"` (the marginal expectation) instead of the conditional
  positive mean; documented in NEWS.
* `nls2` dropped from Imports (no longer used).

## Reverse dependencies

There are no reverse dependencies on CRAN. The maintainer's Shiny app
(shinybeez, GitHub only) was tested against this release candidate.

## Notes on URLs

`R CMD check` / `urlchecker` may flag two categories of URLs; both are
expected:

* Links to the package's own pkgdown article pages
  (`https://brentkaplan.github.io/beezdemand/articles/...`) for vignettes
  added in this release. These resolve once the accompanying documentation
  site is rebuilt on acceptance.
* A few `https://doi.org/...` reference links return 403 to automated
  checkers because the publishers block bots; the DOIs are valid and resolve
  in a browser.

## Package size note

The installed package size is dominated by the `libs` subdirectory from TMB
template compilation (mixed-effects and hurdle demand models), which is
typical for packages using TMB.
