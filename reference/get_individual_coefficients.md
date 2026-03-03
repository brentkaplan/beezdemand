# Calculate Individual-Level Predicted Coefficients from beezdemand_nlme Model

This function extracts and combines fixed and random effects to
calculate individual-level predicted coefficients for all
parameter-factor combinations from a beezdemand_nlme model object. It
automatically detects the factor structure and calculates coefficients
for each individual and factor level.

## Usage

``` r
get_individual_coefficients(
  fit_obj,
  params = c("Q0", "alpha"),
  format = c("wide", "long")
)
```

## Arguments

- fit_obj:

  A `beezdemand_nlme` object returned by
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md).

- params:

  Character vector specifying which parameters to calculate. Options are
  "Q0", "alpha", or c("Q0", "alpha"). Default is c("Q0", "alpha").

- format:

  Character, output format. "wide" returns one row per individual with
  separate columns for each parameter-factor combination. "long" returns
  one row per individual-parameter-factor combination. Default is
  "wide".

## Value

A data frame with individual-level predicted coefficients.

- In "wide" format: rows are individuals, columns are parameter-factor
  combinations

- In "long" format: columns are id, parameter, condition,
  coefficient_value

Column naming convention for wide format:

- `estimated_\{param\}_intercept`: Baseline/reference level coefficient

- `estimated_\{param\}_\{factor\}\{level\}`: Factor level-specific
  coefficient

All coefficients are on the log10 scale (same as model estimation
scale). To convert to natural scale, use `10^coefficient`.

## Details

Individual-level coefficients represent the predicted parameter values
for each subject in the study. For models with factors, these
coefficients combine:

1.  The baseline intercept effect (fixed + random)

2.  The factor-specific effect (fixed + random) for each factor level

This is equivalent to manually calculating:
`coefficient = intercept_fixed + intercept_random + factor_fixed + factor_random`

The function automatically handles:

- Models with or without factors

- Any number of factor levels

- Missing random effects (defaults to 0)

- Complex factor structures with multiple factors

For models without factors, only intercept coefficients are calculated.
For models with factors, both intercept and factor-level coefficients
are provided.

## See also

[`fit_demand_mixed`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
for fitting the original model
[`coef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/coef.beezdemand_nlme.md)
for extracting model coefficients
[`get_demand_param_emms`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.md)
for estimated marginal means

## Examples

``` r
# \donttest{
data(ko)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
                        id_var = "monkey", factors = "drug",
                        equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 6 (Q0: 3, alpha: 3)
individual_coefs <- get_individual_coefficients(fit)
head(individual_coefs)
#>   id estimated_Q0_intercept estimated_Q0_drugFentanyl
#> 1  A               2.133994                  1.969328
#> 2  B               2.133994                  1.969328
#> 3  C               2.133994                  1.969328
#>   estimated_Q0_drugRemifentanil estimated_alpha_intercept
#> 1                      2.384881                 -4.663837
#> 2                      2.384881                 -4.663837
#> 3                      2.384881                 -4.663837
#>   estimated_alpha_drugFentanyl estimated_alpha_drugRemifentanil
#> 1                    -4.393874                        -4.708936
#> 2                    -4.393874                        -4.708936
#> 3                    -4.393874                        -4.708936
# }
```
