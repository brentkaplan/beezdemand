# Extract Estimates from TMB Fit

Extract Estimates from TMB Fit

## Usage

``` r
.tmb_extract_estimates(obj, opt, re_dim_total, n_subjects, has_k, verbose)
```

## Arguments

- obj:

  TMB objective function object.

- opt:

  nlminb optimization result.

- re_dim_total:

  Integer; total RE columns per subject (`re_dim_q0 + re_dim_alpha`).

- n_subjects:

  Integer.

- has_k:

  Logical.

- verbose:

  Integer.

## Value

List with coefficients, se, sdr, variance_components, u_hat.
