# Generate Default Starting Values for Hurdle Model

Internal function to generate sensible default starting values for TMB
hurdle model optimization.

## Usage

``` r
.hurdle_default_starts(consumption, n_re, n_subjects, has_k)
```

## Arguments

- consumption:

  Numeric vector of consumption values.

- n_re:

  Number of random effects (2 or 3).

- n_subjects:

  Number of subjects.

- has_k:

  Logical, whether the model has a k parameter.

## Value

A named list of starting values for TMB parameters.
