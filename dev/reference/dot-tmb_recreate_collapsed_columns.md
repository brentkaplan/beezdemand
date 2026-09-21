# Recreate `collapse_levels` factor columns in newdata

`fit_demand_tmb(collapse_levels = ...)` fits on derived columns named
`<factor>_Q0` / `<factor>_alpha` that live only in `object$data`. A user
supplying newdata in the original shape has the original factor but not
those columns, and [`predict()`](https://rdrr.io/r/stats/predict.html)
rejected the rows as missing required columns (F-BD6-4). The old-to-new
level map is not stored on the fit, but it is recoverable exactly from
the training data, where both columns are present. Columns already in
`newdata` are left alone; an original level unseen in training is left
`NA` and caught by the later level check.

## Usage

``` r
.tmb_recreate_collapsed_columns(object, newdata)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

- newdata:

  Data frame.

## Value

`newdata` with any missing collapsed columns added.
