# Print Method for TMB Mixed-Effects Demand Model

Print Method for TMB Mixed-Effects Demand Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
print(x, ...)
```

## Arguments

- x:

  An object of class `beezdemand_tmb`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
print(fit)
#> 
#> TMB Mixed-Effects Demand Model
#> 
#> Call:
#> fit_demand_tmb(data = apt, equation = "exponential", verbose = 0)
#> 
#> Equation: exponential 
#> Convergence: Yes 
#> Number of subjects: 10 
#> Number of observations: 146 
#> Observations dropped (zeros): 14 
#> Random effects: 2 (~, Q0 + alpha, 1) 
#> Log-likelihood: 40.65 
#> AIC: -67.3 
#> 
#> Fixed Effects:
#>       Q0.0    alpha.0      log_k   logsigma   logsigma logsigma_e    rho_raw 
#>     1.8737    -5.8011     0.8955    -0.9528    -0.7798    -1.9498    -0.4675 
#> 
#> Use summary() for full results.
# }
```
