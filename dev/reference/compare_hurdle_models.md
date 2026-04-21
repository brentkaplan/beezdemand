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
# \donttest{
data(apt)
fit3 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                          random_effects = c("zeros", "q0", "alpha"))
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   Converged in 81 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 32.81
fit2 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                          random_effects = c("zeros", "q0"))
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 9, Recommended minimum: 45 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand2RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 9, Random effects per subject: 2
#>   Optimizing...
#>   Converged in 93 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 2.31
compare_hurdle_models(fit3, fit2)
#> 
#> Likelihood Ratio Test
#> =====================
#>           Model n_RE   LogLik df       AIC       BIC
#>     Full (3 RE)    3 32.81453 12 -41.62905 -4.726965
#>  Reduced (2 RE)    2  2.30934  9  13.38132 41.057884
#> 
#> LR statistic: 61.0104 
#> df: 3 
#> p-value: 3.5757e-13 
# }
```
