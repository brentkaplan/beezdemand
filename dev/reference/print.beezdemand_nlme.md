# Print Method for beezdemand_nlme Objects

Provides a concise summary of a `beezdemand_nlme` object, typically
displaying the call, model specifications, and key results from the
`nlme` fit if successful.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- x:

  An object of class `beezdemand_nlme`.

- digits:

  Minimal number of significant digits, see `print.default`.

- ...:

  Additional arguments passed to `print.nlme` if the model exists.

## Value

Invisibly returns the original object `x`.

## Examples

``` r
# \donttest{
data(ko)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
                        id_var = "monkey", equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 2 (Q0: 1, alpha: 1)
print(fit)
#> Demand NLME Model Fit ('beezdemand_nlme' object)
#> ---------------------------------------------------
#> 
#> Call:
#> fit_demand_mixed(data = ko, y_var = "y_ll4", x_var = "x", id_var = "monkey", 
#>     equation_form = "zben")
#> 
#> Equation Form Selected:  zben 
#> NLME Model Formula:
#> y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> <environment: 0x55db5cfacac0>
#> Fixed Effects Structure (Q0 & alpha):  ~ 1 
#> Factors: None
#> ID Variable for Random Effects:  monkey 
#> 
#> Start Values Used (Fixed Effects Intercepts):
#>   Q0 Intercept (log10 scale):  2.271 
#>   alpha Intercept (log10 scale):  -3 
#> 
#> --- NLME Model Fit Summary (from nlme object) ---
#> Nonlinear mixed-effects model fit by maximum likelihood
#>   Model: nlme_model_formula_obj 
#>   Data: data 
#>   Log-likelihood: -25.98641
#>   Fixed: list(Q0 ~ 1, alpha ~ 1) 
#>        Q0     alpha 
#>  2.158507 -4.586304 
#> 
#> Random effects:
#>  Formula: list(Q0 ~ 1, alpha ~ 1)
#>  Level: monkey
#>  Structure: Diagonal
#>                   Q0      alpha  Residual
#> StdDev: 8.924541e-06 5.4422e-06 0.2933331
#> 
#> Number of Observations: 135
#> Number of Groups: 3 
#> 
#> --- Additional Fit Statistics ---
#> Log-likelihood:  -25.99 
#> AIC:  61.97 
#> BIC:  76.5 
#> ---------------------------------------------------
# }
```
