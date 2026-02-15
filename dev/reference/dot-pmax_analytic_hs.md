# Analytic Pmax for HS/Exponential Model (Lambert W)

For the exponential model: Q(p) = Q0 \* 10^(k \* (exp(-alpha \* Q0 \*
p) - 1)) Pmax = -W_0(-1 / (k \* ln(10))) / (alpha \* Q0)

## Usage

``` r
.pmax_analytic_hs(alpha_nat, q0_nat, k_nat)
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
