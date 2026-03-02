# Run pairwise intercept comparisons for cross-price demand model

This function performs pairwise comparisons of intercepts between groups
in a cross-price demand model, but only when a significant interaction
is present. The emmeans table showing estimated marginal means for
intercepts is always returned.

## Usage

``` r
cp_posthoc_intercepts(object, alpha = 0.05, adjust = "tukey", ...)
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
cp_posthoc_intercepts(fit)
#> Cannot use mode = "kenward-roger" because *pbkrtest* package is not installed
#> Cannot use mode = "satterthwaite" because *lmerTest* package is not installed
#> NOTE: Results may be misleading due to involvement in interactions
#> Intercept Estimates and Comparisons 
#> =================================== 
#> 
#> Estimated Marginal Means:
#>  group              emmean       SE  df asymp.LCL asymp.UCL
#>  Cigarettes       0.400000 2.045999 Inf -3.610085  4.410085
#>  Combustibles     4.994527 2.045999 Inf  0.984443  9.004612
#>  E-Cigarettes     1.986070 2.045999 Inf -2.024015  5.996154
#>  Non-Combustibles 3.972637 2.045999 Inf -0.037448  7.982722
#> 
#> Degrees-of-freedom method: asymptotic 
#> Confidence level used: 0.95 
#> 
#> Significant interaction: No 
#> 
#> No significant interaction detected (alpha = 0.05 ). Pairwise intercept comparisons not performed. 
#> P-value adjustment method: tukey 
# }
```
