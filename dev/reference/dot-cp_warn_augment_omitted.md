# Warn When augment.cp_model\_\*() Omits a Documented Column

Internal helper (TICKET-068, E5c). The `augment.cp_model_*()` methods
document `.fitted`/`.resid`/`.fixed` as always-present columns; when the
underlying
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html)/[`residuals()`](https://rdrr.io/r/stats/residuals.html)/[`predict()`](https://rdrr.io/r/stats/predict.html)
call errors, or its result's length doesn't match the augmented data,
the column was previously dropped with no indication, indistinguishable
from "not applicable". The warning therefore names which call failed and
includes `conditionMessage(e)` rather than the column name alone.

## Usage

``` r
.cp_warn_augment_omitted(details)
```

## Arguments

- details:

  Character vector, one fully-composed detail string per omitted column
  (e.g. `".fitted: fitted() failed: <message>"`).

## Value

Invisible `NULL`; called for the warning side effect.
