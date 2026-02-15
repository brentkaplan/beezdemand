# Calculate Omax and Pmax for Multiple Subjects

Vectorized calculation of Omax and Pmax for multiple subjects with
individual-specific parameters.

## Usage

``` r
calc_omax_pmax_vec(Q0, k, alpha, price_range = NULL)
```

## Arguments

- Q0:

  Numeric vector of intensity parameters.

- k:

  Numeric vector of scaling parameters (or single value).

- alpha:

  Numeric vector of elasticity parameters (or single value).

- price_range:

  Numeric vector of length 2 for search range. Default NULL uses
  adaptive range based on alpha.

## Value

A data frame with columns Pmax, Omax, Qmax.
