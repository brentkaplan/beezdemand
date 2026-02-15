# Calculate Group-Level Demand Metrics

Calculates group-level (population) Omax and Pmax from a fitted hurdle
demand model.

## Usage

``` r
calc_group_metrics(object)
```

## Arguments

- object:

  A fitted `beezdemand_hurdle` object.

## Value

A named list with group-level Pmax, Omax, and Qmax.

## See also

[`calc_omax_pmax`](https://brentkaplan.github.io/beezdemand/reference/calc_omax_pmax.md),
[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
group_metrics <- calc_group_metrics(fit)
} # }
```
