# Apply the multi-start rescue protocol to a completed FitCurves() run

Called from
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
after the production
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
call but before its success/failure bookkeeping, so downstream logic
(which derives `results$converged` from `results$converged_strict`) sees
the post-rescue verdicts automatically.

## Usage

``` r
.fixed_multistart_apply(
  results,
  fits,
  predictions,
  data_used,
  equation,
  k,
  agg = NULL,
  param_space,
  multistart,
  S,
  dots = list()
)
```

## Arguments

- results:

  `dfres` data frame from the production
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  call.

- fits:

  `fits` list from the production call.

- predictions:

  `newdats` list from the production call.

- data_used:

  `adfs` list from the production call.

- equation:

  Canonical (post `normalize_equation()`) equation name.

- k:

  The `k` argument as supplied to
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).

- agg:

  The `agg` argument as supplied to
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
  (`NULL`, `"Mean"`, or `"Pooled"`). This must be passed through
  unchanged to each rescue
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  call (never hard-coded to `NULL`). For `agg = "Pooled"`,
  `data_used[[i]]` is the raw (duplicated-price) stacked data, and
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)'s
  own `agg = "Pooled"` handling is what builds the deduplicated table
  [`GetEmpirical()`](https://brentkaplan.github.io/beezdemand/reference/GetEmpirical.md)
  needs; without it,
  [`GetEmpirical()`](https://brentkaplan.github.io/beezdemand/reference/GetEmpirical.md)
  hard-errors on "Duplicates found where id = pooled".

- param_space:

  `"natural"` or `"log10"`.

- multistart:

  Logical; multi-start on/off.

- S:

  Integer budget, or `NULL` to use the tiered default.

- dots:

  Named list of additional arguments forwarded from
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)'s
  `...` (e.g. `lobound`, `hibound`, `constrainq0`), passed through to
  each rescue
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  call, with any user-supplied `startq0`/`startalpha` stripped (the
  sampled starts replace them for rescue attempts).

## Value

List with elements `results`, `fits`, `predictions`, `data_used`
(post-rescue, same shapes as the inputs) and `multistart_info`
(settings + per-subject summary data frame, for the `beezdemand_fixed`
object's `$multistart` field).
