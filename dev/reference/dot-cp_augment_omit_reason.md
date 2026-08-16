# Describe Why an Augment Column Was Omitted

Internal helper (TICKET-068, E5c). Builds the "colname: reason" detail
string for
[`.cp_warn_augment_omitted()`](https://brentkaplan.github.io/beezdemand/reference/dot-cp_warn_augment_omitted.md)
from either a caught condition or a length mismatch against the expected
row count.

## Usage

``` r
.cp_augment_omit_reason(col, fn_label, res, n_expected)
```

## Arguments

- col:

  Column name (e.g. `".fitted"`).

- fn_label:

  Human name of the failing call (e.g. `"fitted()"`).

- res:

  Either the successful result, or a condition object caught via
  `tryCatch(..., error = function(e) e)`.

- n_expected:

  Expected length (number of rows in the augmented data).

## Value

Character string.
