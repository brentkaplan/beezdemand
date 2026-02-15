# Analytic Pmax for HS-Standardized Hurdle Part II (Q0 inside exponent)

For hurdle_hs_stdq0: Q(p) = Q0 \* exp(k \* (exp(-alpha \* Q0 \* p) - 1))
Pmax = -W_0(-1/k) / (alpha \* Q0)

## Usage

``` r
.pmax_analytic_hurdle_hs_stdq0(alpha_nat, q0_nat, k_nat)
```

## Arguments

- alpha_nat:

  Natural-scale alpha

- q0_nat:

  Natural-scale Q0

- k_nat:

  Natural-scale k

## Value

List with pmax, method, and notes
