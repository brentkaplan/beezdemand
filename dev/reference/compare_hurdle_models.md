# Compare Nested Hurdle Demand Models

Performs a likelihood ratio test comparing two nested hurdle demand
models. Typically used to test whether adding the random effect on alpha
(c_i) significantly improves model fit (3-RE vs 2-RE models).

## Usage

``` r
compare_hurdle_models(model_full, model_reduced)
```

## Arguments

- model_full:

  A `beezdemand_hurdle` object with 3 random effects.

- model_reduced:

  A `beezdemand_hurdle` object with 2 random effects.

## Value

Invisibly returns a list with:

- lr_stat:

  Likelihood ratio test statistic

- df:

  Degrees of freedom

- p_value:

  P-value from chi-squared distribution

- model_comparison:

  Data frame with model comparison statistics

## See also

[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit3 <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id",
                          random_effects = c("zeros", "q0", "alpha"))
fit2 <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id",
                          random_effects = c("zeros", "q0"))
compare_hurdle_models(fit3, fit2)
} # }
```
