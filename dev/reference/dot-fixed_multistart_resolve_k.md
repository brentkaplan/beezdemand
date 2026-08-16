# Resolve the `k` value to pass to each rescue `FitCurves()` call

Codex 2F review fold, TICKET-047 item 3: the production `k` argument
(`"share"`, `"range"`, `"fit"`, `"ind"`, or a plain number) means
different things and CANNOT simply be re-passed unchanged to a rescue
call that fits a single subject at a time:

## Usage

``` r
.fixed_multistart_resolve_k(k_arg, results, data_used, equation)
```

## Arguments

- k_arg:

  The `k` argument as supplied to
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).

- results:

  The (pre-rescue) production `dfres` data frame.

- data_used:

  The (pre-rescue) production `adfs` list.

- equation:

  Canonical equation name (needed by
  [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)).

## Value

A list with `value` (the `k` to pass to rescue
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
calls – numeric for `"share"`/`"range"`, unchanged otherwise) and
`resolvable` (`FALSE` only when `k_arg` is `"share"`/`"range"` and the
dataset-wide k is genuinely unknowable from this run – callers must skip
rescue entirely rather than fit a different, unreproducible model).

## Details

- `"share"`:
  [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)
  requires 2+ groups and hard-stops ("Cannot find a shared k value with
  only one dataset!") on a single-subject dataset – every rescue attempt
  would silently fail via [`try()`](https://rdrr.io/r/base/try.html),
  never actually rescuing anything.

- `"range"`:
  [`GetK()`](https://brentkaplan.github.io/beezdemand/reference/GetK.md)
  is dataset-wide in production (computed ONCE, before the per-subject
  loop); re-passing `"range"` would recompute a DIFFERENT,
  subject-specific K for the rescue, silently fitting a different model
  than production used for every other subject.

- `"fit"`: k is a genuinely free parameter refit per subject already, so
  `"fit"` is safe to pass through unchanged. Its *starting* value
  (`kstart`) is recomputed internally by
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  from the single-subject slice, which is NOT the same starting value
  production used (production's `kstart` comes from
  [`GetK()`](https://brentkaplan.github.io/beezdemand/reference/GetK.md)
  on the full dataset) – this is a known, accepted, and documented
  limitation: only the Q0/alpha starts are actually sampled by the
  multi-start protocol for `k = "fit"`, not the k start.

- `"ind"` or a plain number: already resolved per-subject (or globally)
  in exactly the way a single-subject rescue call would resolve it too,
  so passing through unchanged is correct.

For `"share"`/`"range"`, this first tries to read the actual numeric K
production used from the (dataset-wide, therefore identical across all
rows) `K` column of the production results – fast, and exact by
construction. If NO row recorded a K (e.g. every single subject's own
nls fit failed, which can happen independently of the dataset-wide k
itself being perfectly well-defined), it falls back to recomputing K
directly via the SAME function
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
used internally
([`GetK()`](https://brentkaplan.github.io/beezdemand/reference/GetK.md)/[`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md))
on the SAME combined data (the union of all `data_used` slices
reconstructs exactly the
post-[`CheckCols()`](https://brentkaplan.github.io/beezdemand/reference/CheckCols.md)/zero-drop
dataset
[`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
originally computed K from) – so this is exact, never an approximation,
and never a different model than production.
