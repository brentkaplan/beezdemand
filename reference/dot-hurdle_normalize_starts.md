# Normalize User-Provided Starting Values

Internal function to handle backwards-compatibility and normalization of
user-provided starting values.

## Usage

``` r
.hurdle_normalize_starts(start_values, n_re, has_k)
```

## Arguments

- start_values:

  User-provided named list of starting values.

- n_re:

  Number of random effects (2 or 3).

- has_k:

  Logical, whether the model has a k parameter.

## Value

Normalized starting values list.
