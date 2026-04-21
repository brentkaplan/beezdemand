# Extract Estimates from TMB Fit

Extract Estimates from TMB Fit

## Usage

``` r
.tmb_extract_estimates(obj, opt, n_re, n_subjects, has_k, verbose)
```

## Arguments

- obj:

  TMB objective function object.

- opt:

  nlminb optimization result.

- n_re:

  Integer.

- n_subjects:

  Integer.

- has_k:

  Logical.

- verbose:

  Integer.

## Value

List with coefficients, se, sdr, variance_components, u_hat.
