# Get Parameter Names for Hurdle Model

Internal function to get the names of fixed effect parameters and
variance component names based on model configuration.

## Usage

``` r
.hurdle_get_param_names(n_re, has_k)
```

## Arguments

- n_re:

  Number of random effects (2 or 3).

- has_k:

  Logical, whether the model has a k parameter.

## Value

A list with fixed_names, var_names, and rho_names.
