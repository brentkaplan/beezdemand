# beezdemand 0.2.0

## Deprecations

* `FitCurves()` is now superseded by `fit_demand_fixed()`. `FitCurves()` will
  continue to work but emits a soft deprecation warning. The new function
  provides a modern S3 interface with `summary()`, `tidy()`, `glance()`,
  `predict()`, and `plot()` methods. See `vignette("migration-guide")` for
  migration instructions.

* `FitMeanCurves()` is now superseded by `fit_demand_fixed(agg = "Mean")` or
  `fit_demand_fixed(agg = "Pooled")`.

## New Features

### Koffarnus Equation for Mixed-Effects Models

* `fit_demand_mixed()` now supports the Koffarnus et al. (2015) exponentiated
  equation via `equation_form = "koff"`. This enables fitting demand curves
  using the same equation form available in `FitCurves()` within the modern
  hierarchical mixed-effects framework. The `k` parameter can be user-specified
  or auto-calculated from data range.

### broom Integration

* New `augment()` methods for all model classes provide fitted values and
  residuals in a tidy tibble:
  - `augment.beezdemand_fixed()`: Returns `.fitted`, `.resid`
  - `augment.beezdemand_hurdle()`: Returns `.fitted`, `.fitted_link`,
    `.fitted_prob`, `.resid`, `.resid_response`
  - `augment.beezdemand_nlme()`: Returns `.fitted`, `.resid`, `.fixed`

### Model Comparison Framework

* New `compare_models()` function for unified model comparison across all
  beezdemand model classes. Reports AIC, BIC, delta_AIC, delta_BIC, and
  performs likelihood ratio tests when models are from the same backend
  and nested.

* New `anova()` S3 methods for comparing nested models:
  - `anova.beezdemand_hurdle()`: LRT for nested hurdle models
  - `anova.beezdemand_nlme()`: Delegates to nlme::anova.lme()

### Model Diagnostics Suite

* New `check_demand_model()` generic with methods for all model classes. Performs
  comprehensive diagnostics including convergence checks, boundary condition
  detection, random effect variance assessment, and residual outlier detection.
  Returns structured diagnostics object with issues and recommendations. (Named
  `check_demand_model()` to avoid conflict with `performance::check_model()`.)

* New `plot_residuals()` function creates diagnostic plots: residuals vs fitted,
  histogram of residuals, and Q-Q plots. Works with all model classes via
  the `augment()` infrastructure.

* New `plot_qq()` function creates Q-Q plots for random effects to assess
  normality assumptions in hurdle and NLME models.

### Normalized Alpha (Alpha Star)

* All model classes now compute `alpha_star` (normalized alpha, Strategy B;
  Rzeszutek et al., 2025), which makes the elasticity parameter comparable
  across different values of `k`. Available in `FitCurves()` output (columns
  `alpha_star` and `alpha_star_se`), `tidy()` on `beezdemand_fixed` objects,
  and `tidy()` on `beezdemand_hurdle` objects. Standard errors are obtained
  via the delta method. See `?param-registry` for details.

### Modern Wrappers for Legacy Functions

* New `get_empirical_measures()` as a modern replacement for `GetEmpirical()`.
  Returns a tibble with consistent column naming.

* New `get_descriptive_summary()` as a modern replacement for
  `GetDescriptives()`. Returns a tibble with consistent column naming.

* New `get_k()` as a modern replacement for `GetK()`. Returns a tibble with
  consistent column naming.

### Other New Features

* New `confint()` methods for extracting confidence intervals from all model
  classes: `beezdemand_fixed`, `beezdemand_hurdle`, `beezdemand_nlme`, and
  `cp_model_nls`.

* New migration guide vignette (`vignette("migration-guide")`) documenting the

  transition from `FitCurves()` to `fit_demand_fixed()`.

## Breaking Changes

* `summary()` methods for `beezdemand_cp_hurdle`, `beezdemand_joint_hurdle`,
  and `beezdemand_nlme` now return structured summary objects instead of
  printing directly. Use `print(summary(fit))` for console output.
  Programmatic access is now possible: `s <- summary(fit); s$coefficients`.

* `fit_demand_hurdle()` now fits demand parameters in natural-log space
  (`log_q0`, `log_alpha`, `log_k`) and reports back-transformed values; the
  `param_space` argument has been removed.

* `fit_cp_nls()` now uses log10-parameterized optimizer coefficients
  (`log10_qalone`, `I`, `log10_beta`) across equation forms; the `"exponential"`
  form fits on the `log10(y)` response scale and filters `y <= 0` with a warning.
  `predict.cp_model_nls()` now always returns `y_pred` on the natural `y` scale;
  for `"exponential"` it additionally returns `y_pred_log10` (and no longer returns
  `y_pred_natural`).

### Additional New Features

* `fit_joint_hurdle()` now accepts `k = "estimate"` to estimate the scaling
  constant as a free parameter. Default remains `k = 2` (fixed). Estimating
  k will emit a warning about potential identifiability issues.

* New `fit_demand_fixed()` function provides a modern interface for individual
  demand curve fitting. Returns a structured S3 object with `summary()`,
  `tidy()`, and `glance()` methods. This wrapper offers the same functionality
  as `FitCurves()` but with a standardized API.

* New systematicity wrappers with unified output vocabulary:
  - `check_systematic_demand()` for purchase task data (wraps `CheckUnsystematic()`)
  - `check_systematic_cp()` for cross-price data (wraps `check_unsystematic_cp()`)

  Both return `beezdemand_systematicity` objects with identical column
  schemas (differing only in NA values for domain-specific fields).

* First-class `tidy()` and `glance()` support is now guaranteed across all
  beezdemand model classes. All methods return tibbles with standardized
  columns including `model_class` and `backend`.

* All summary objects now inherit from `beezdemand_summary` base class,
  enabling shared fallback behavior and consistent field availability.

## API Standardization

This release introduces **Stability Contracts** for all model classes:

* **summary() objects** now return structured S3 objects with class
  `c("summary.<class>", "beezdemand_summary")`. Required fields include:
  `call`, `model_class`, `backend`, `nobs`, `n_subjects`, `converged`,
  `logLik`, `AIC`, `BIC`, `coefficients` (tibble), `notes`.

* **tidy() methods** return tibbles with columns: `term`, `estimate`,
  `std.error`, `statistic`, `p.value`. Multi-part models include a
  `component` column (e.g., "fixed", "variance", "derived").

* **glance() methods** return 1-row tibbles with columns: `model_class`,
  `backend`, `nobs`, `n_subjects`, `converged`, `logLik`, `AIC`, `BIC`.

See the ARCHITECTURE.md "Stability Contracts" section for complete details.

---

# beezdemand 0.1.3

## Deprecations

The following deprecations will take effect in version 0.2.0:

* `beezdemand::pull()` is deprecated in favor of `dplyr::pull()`. The beezdemand
  version was a legacy helper that predates the dplyr function.

* The `inverse_fun` argument in `summary.cp_model_nls()`, `plot.cp_model_nls()`,
  and `predict.cp_model_nls()` is deprecated in favor of `inv_fun` for consistency
  with mixed-effects model methods.

## API Improvements

* Standardized argument names across cross-price model methods (`inv_fun` instead
  of `inverse_fun`)

* Cross-price plot methods now have consistent argument ordering across
  `plot.cp_model_nls()`, `plot.cp_model_lm()`, and `plot.cp_model_lmer()`

* Key user-facing functions now return tibbles for better compatibility with

  tidyverse workflows: `predict.cp_model_nls()`, `predict.cp_model_lm()`,
  `tidy.cp_model_nls()`, `glance.cp_model_nls()`

* Added standardized error helpers (`validation_error()`, `fitting_error()`,
  `missing_package_error()`) for consistent error messaging

* `check_unsystematic_cp()` now returns an object of class `cp_unsystematic`
  with proper `summary()` method dispatch (no longer overrides `summary.tbl_df()`)

## New Features

### Two-Part Mixed Effects Hurdle Demand Models

* Added comprehensive hurdle model functionality using TMB (Template Model Builder):

  * `fit_demand_hurdle()`: Fit two-part hurdle models with 2 or 3 random effects

  * Part I models probability of zero consumption (logistic regression with random intercept)

  * Part II models log-consumption given positive response (nonlinear mixed effects)

* S3 methods for `beezdemand_hurdle` objects:

  * `print()`, `summary()`, `coef()`, `logLik()`, `AIC()`, `BIC()`

  * `predict()`: Extract subject parameters or predict demand/probability

  * `plot()`: Visualize demand curves, zero probability, parameter distributions

* Utility functions:

  * `calc_omax_pmax()`: Calculate Pmax and Omax from demand parameters

  * `get_subject_pars()`: Extract subject-specific parameter estimates

  * `compare_hurdle_models()`: Likelihood ratio test for model comparison

  * `get_hurdle_param_summary()`: Summary statistics for individual parameters

* Simulation functions:

  * `simulate_hurdle_data()`: Generate synthetic hurdle model data

  * `run_hurdle_monte_carlo()`: Monte Carlo simulation studies

* New vignette "Hurdle Demand Models" with comprehensive examples

* New dataset `apt_full`: Full alcohol purchase task data with 1,100 subjects and demographic covariates

### Cross-Price Demand Models

* Added comprehensive cross-price demand model functionality:

  * `check_unsystematic_cp()`: Check for unsystematic data in cross-price models

  * `fit_cp_nls()`: Nonlinear model fitting for cross-price demand data

  * `fit_cp_linear()`: Linear model fitting for cross-price demand data with options for fixed effects and mixed-effects models

  * New utility functions for cross-price model objects:

    * `summary()`, `plot()`, `glance()`, and `tidy()` methods

  * `extract_coefficients()`: Extract model coefficients in tidy format

  * `cp_posthoc_slopes()` and `cp_posthoc_intercepts()`: Post-hoc comparisons for model parameters

  * `validate_cp_data()`: Validate and filter cross-price demand data

* Added new vignette "How to Use Cross-Price Demand Model Functions" demonstrating:

  * Required data structure for cross-price analyses

  * Checking for unsystematic data

  * Both two-stage and pooled model fitting approaches

  * Linear and mixed-effects modeling options

  * Model visualization and coefficient extraction

  * Post-hoc comparisons

# beezdemand 0.1.2

* No longer relies on `nlmrt` and instead relies on `nlsr`

* Fixes an issue where CheckUnsystematic may not flag certain cases when data are passed as `tibble`

* Fixes deprecated arguments in `ggplot2`

* Add ability to specify a start value for alpha in `ExtraF()` function

# beezdemand 0.1.1

* Add experimental features for `FitCurves()`. These arguments are `constrainq0`, `startq0`, and `startalpha`. These arguments allow Q0 to be constrained so alpha is the only fitted parameter and allow for user-specified starting values.

# beezdemand 0.1.0

* Package successfully on CRAN!

# beezdemand 0.1.00

* Package should be ready for CRAN and is being submitted

# beezdemand 0.0.95

## New updates

* One major change that might affect previous scripts is that in output summary tables, the column formally named ID is now named id (lowercase)

* Cleaned up a few things here and there. The package is close for submission to CRAN as it passes R CMD check with no errors, warnings, or notes

# beezdemand 0.0.91

## New updates

* `GetSharedK()` updated to work better and faster at finding a reasonable value

* Internal helper functions added to optimize `GetSharedK()`

# beezdemand 0.0.90

## New updates

* `ExtraF()` now compares alpha and Q0

* A number of functions now allows you to specify the column names

# beezdemand 0.0.85

## New updates

* `FitCurves()` correctly pulls alpha and q0 standard errors when k is fitted as a free parameter. Also no longer accepts data transformations. Must be done prior to fitting using `ChangeData()`.

* `FitCurves()` now fits mean/pooled data based on `method` argument.

* `GetSharedK()` no longer accepts data transformations.

# beezdemand 0.0.84

## New updates

* New `ChangeData()` will soon serve as the replacement to
  `ReplaceZeros()` and other arguments specified in `FitCurves()`.

* For the time being, `FitCurves()` will actually output a list
  object. This may cause failures with old scripts. Try modifying
  scripts to take the first element out of the list. This will soon be
  taken care of.

## Tidying

* Email contact has been changed and some minor updates to .rd files.

# beezdemand 0.0.6

## New features

* New `FitMeanCurves()` will fit curve to averaged or pooled
  data. Can also make plots.

* `FitCurves()` can now make plots.

* `GetDescriptives()` can make box and whisker plots.

## Cleanup

* Old code from previous workflow is removed. Now all functions use
  longform data.
