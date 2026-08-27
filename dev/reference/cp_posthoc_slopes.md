# Run pairwise slope comparisons for cross-price demand model

Performs pairwise comparisons of slopes between groups in a cross-price
demand model, but only when a significant interaction is present. The
emmeans table showing estimated marginal means for slopes is always
returned.

## Usage

``` r
cp_posthoc_slopes(object, alpha = 0.05, adjust = "tukey", ...)
```

## Arguments

- object:

  A cp_model_lmer object from fit_cp_linear

- alpha:

  Significance level for testing (default: 0.05)

- adjust:

  Method for p-value adjustment; see emmeans::contrast (default:
  "tukey")

- ...:

  Additional arguments passed to emmeans

## Value

List containing the emmeans table and optionally pairwise comparisons if
interaction is significant

## Examples

``` r
# \donttest{
data(etm)
fit <- fit_cp_linear(etm, type = "mixed", group_effects = TRUE)
cp_posthoc_slopes(fit)
#> Slope Estimates and Comparisons 
#> =============================== 
#> 
#> Estimated Marginal Means:
#>  group               x.trend         SE  df    lower.CL  upper.CL
#>  Cigarettes       0.01666667 0.04468425 223 -0.07139075 0.1047241
#>  Combustibles     0.08994314 0.04468425 223  0.00188572 0.1780006
#>  E-Cigarettes     0.02764748 0.04468425 223 -0.06040994 0.1157049
#>  Non-Combustibles 0.09257285 0.04468425 223  0.00451543 0.1806303
#> 
#> Degrees-of-freedom method: kenward-roger 
#> Confidence level used: 0.95 
#> 
#> Significant interaction: No 
#> 
#> No significant interaction detected (alpha = 0.05 ). Pairwise slope comparisons not performed. 
#> P-value adjustment method: tukey 
# }
```
