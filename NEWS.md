# beezdemand 0.1.3

## New Features

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
