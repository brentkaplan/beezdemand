# Analytic Pmax for Hurdle Model (Natural-parameter exponential form)

For hurdle: Q(p) = Q0 \* exp(k \* (exp(-alpha \* p) - 1)) Note: No Q0
normalization in exponent (unlike HS) Pmax = -W_0(-1/k) / alpha

## Usage

``` r
.pmax_analytic_hurdle(alpha_nat, k_nat)
```

## Arguments

- alpha_nat:

  Natural-scale alpha

- k_nat:

  Natural-scale k

## Value

List with pmax, method, and notes
