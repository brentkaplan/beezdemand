# Calculate Elasticity at a Given Price

Computes eta(p) = d log(Q(p)) / d log(p) via central finite differences

## Usage

``` r
.elasticity_at_price(demand_fn, price, delta = 1e-05)
```

## Arguments

- demand_fn:

  Function Q(p) returning demand at price p

- price:

  Price point at which to evaluate elasticity

- delta:

  Relative step size for finite difference (default 1e-5)

## Value

Numeric elasticity value
