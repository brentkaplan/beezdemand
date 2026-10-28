# ANOVA Method for Hurdle Demand Models

Compare nested hurdle demand models using likelihood ratio tests.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
anova(object, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` model.

- ...:

  Additional `beezdemand_hurdle` models to compare.

## Value

An object of class `anova.beezdemand` containing:

- table:

  Data frame with model comparison statistics

- lrt:

  Likelihood ratio test results

## Details

All models must be fit to the same data. Models are ordered by degrees
of freedom, and sequential likelihood ratio tests are performed.

## Examples

``` r
# \donttest{
data(apt)
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
anova(fit2, fit3)
#> 
#> Analysis of Variance Table
#> ================================================== 
#> 
#>    Model n_RE df  logLik      AIC     BIC
#>  Model_1    2  9  2.3093  13.3813 41.0579
#>  Model_2    3 12 32.8145 -41.6291 -4.7270
#> 
#> Likelihood Ratio Tests:
#> ---------------------------------------- 
#>          Comparison LR_stat df Pr_Chisq
#>  Model_1 vs Model_2 61.0104  3 3.58e-13
# }
```
