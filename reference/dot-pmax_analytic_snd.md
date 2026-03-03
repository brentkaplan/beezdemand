# Analytic Pmax for Simplified/SND Model

For SND: Q(p) = Q0 \* exp(-alpha \* Q0 \* p) Pmax = 1 / (alpha \* Q0)

## Usage

``` r
.pmax_analytic_snd(alpha_nat, q0_nat)
```

## Arguments

- alpha_nat:

  Natural-scale alpha

- q0_nat:

  Natural-scale Q0

## Value

List with pmax, method, and notes
