# Calculate Omax and Pmax for Demand Curves

Calculates the maximum expenditure (Omax) and the price at maximum
expenditure (Pmax) for the exponential demand model used in the two-part
hurdle model.

## Usage

``` r
calc_omax_pmax(Q0, k, alpha, price_range = NULL)
```

## Arguments

- Q0:

  Intensity parameter (consumption at price 0).

- k:

  Scaling parameter for the exponential decay.

- alpha:

  Elasticity parameter (rate of decay).

- price_range:

  Numeric vector of length 2 specifying the price range to search for
  Pmax. Default is `NULL`, which uses an adaptive range based on alpha
  (approximately 0 to 10/alpha).

## Value

A named list with:

- Pmax:

  Price at maximum expenditure

- Omax:

  Maximum expenditure (price \* quantity)

- Qmax:

  Quantity at Pmax

## Details

For the demand function: \$\$Q(p) = Q_0 \cdot \exp(k \cdot (\exp(-\alpha
\cdot p) - 1))\$\$

Expenditure is E(p) = p \* Q(p). Omax is the maximum of E(p) and Pmax is
the price at which this maximum occurs. These are found numerically.

The search range is automatically adjusted based on alpha to ensure the
maximum is found. For small alpha values, Pmax can be quite large.

## See also

[`calc_group_metrics`](https://brentkaplan.github.io/beezdemand/reference/calc_group_metrics.md),
[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
# Calculate for group-level parameters
calc_omax_pmax(Q0 = 10, k = 2, alpha = 0.5)
#> Warning: `calc_omax_pmax()` was deprecated in beezdemand 0.2.0.
#> ℹ Please use `beezdemand_calc_pmax_omax()` instead.
#> Warning: Note: k (2.000) < e (~2.718); the expenditure function has no interior maximum. Returning the maximum over a bounded search interval via numerical optimization.
#> $Pmax
#> [1] 19.99993
#> 
#> $Omax
#> [1] 27.06941
#> 
#> $Qmax
#> [1] 1.353476
#> 
#> $note
#> [1] "k < e: bounded-range maximum over [0.001, 20.000] via numerical optimization"
#> 

# With k >= e (~2.718), a local maximum exists
calc_omax_pmax(Q0 = 10, k = 3, alpha = 0.5)
#> $Pmax
#> [1] 1.238123
#> 
#> $Omax
#> [1] 3.100397
#> 
#> $Qmax
#> [1] 2.504112
#> 
```
