# Predict from a Mixed-Effects Cross-Price Demand Model

Generates predictions from a mixed-effects cross-price demand model (of
class `cp_model_lmer`). The function supports two modes:

## Usage

``` r
# S3 method for class 'cp_model_lmer'
predict(object, newdata = NULL, pred_type = c("fixed", "random"), ...)
```

## Arguments

- object:

  A `cp_model_lmer` object (as returned by
  `fit_cp_linear(type = "mixed", ...)`).

- newdata:

  A data frame containing at least an `x` column. For
  `pred_type = "random"`, an `id` column is required. If absent, the
  function extracts unique ids from `object$data` and expands the grid
  accordingly. If no ids are available, a default id of 1 is used (with
  a warning).

- pred_type:

  Character string specifying the type of prediction: either `"fixed"`
  (population-level) or `"random"` (subject-specific). The default is
  `"fixed"`.

- ...:

  Additional arguments passed to the underlying `predict` function.

## Value

A data frame containing all columns of `newdata` plus a column `y_pred`
with the corresponding predictions.

## Details

- `"fixed"`:

  Returns predictions based solely on the fixed-effects component (using
  `re.form = NA`).

- `"random"`:

  Returns subject-specific predictions (fixed plus random effects)
  (using `re.form = NULL`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Population-level predictions:
predict(fit_out_3, newdata = ex_sub, pred_type = "fixed")

# Subject-specific predictions:
predict(fit_out_3, newdata = ex_sub, pred_type = "random")
} # }
```
