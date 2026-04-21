# Expand Partial Bounds to Full Parameter Vector

Expand Partial Bounds to Full Parameter Vector

## Usage

``` r
.expand_bounds(bounds, par_names, default_val)
```

## Arguments

- bounds:

  Named numeric vector of user-specified bounds (possibly partial), or
  NULL.

- par_names:

  Character vector of optimizer parameter names (from `names(obj$par)`).
  May contain repeated names for vector parameters.

- default_val:

  Default bound value: `-Inf` for lower, `Inf` for upper.

## Value

Numeric vector of length `length(par_names)` with user bounds applied to
all matching positions and `default_val` elsewhere.
