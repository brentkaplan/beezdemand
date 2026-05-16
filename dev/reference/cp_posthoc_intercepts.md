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
#> NOTE: Results may be misleading due to involvement in interactions
#> Intercept Estimates and Comparisons 
#> =================================== 
#> 
#> Estimated Marginal Means:
#>  group              emmean       SE    df  lower.CL upper.CL
#>  Cigarettes       0.400000 2.045999 22.93 -3.833188 4.633188
#>  Combustibles     4.994527 2.045999 22.93  0.761340 9.227715
#>  E-Cigarettes     1.986070 2.045999 22.93 -2.247118 6.219257
#>  Non-Combustibles 3.972637 2.045999 22.93 -0.260551 8.205824
#> 
#> Degrees-of-freedom method: kenward-roger 
#> Confidence level used: 0.95 
#> 
#> Significant interaction: No 
#> 
#> No significant interaction detected (alpha = 0.05 ). Pairwise intercept comparisons not performed. 
#> P-value adjustment method: tukey 
# }
```
