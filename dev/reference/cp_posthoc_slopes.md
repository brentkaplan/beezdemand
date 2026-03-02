# Run pairwise slope comparisons for cross-price demand model

This function performs pairwise comparisons of slopes between groups in
a cross-price demand model, but only when a significant interaction is
present. The emmeans table showing estimated marginal means for slopes
is always returned.

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
#> Cannot use mode = "kenward-roger" because *pbkrtest* package is not installed
#> Cannot use mode = "satterthwaite" because *lmerTest* package is not installed
#> Slope Estimates and Comparisons 
#> =============================== 
#> 
#> Estimated Marginal Means:
#>  group               x.trend         SE  df   asymp.LCL asymp.UCL
#>  Cigarettes       0.01666667 0.04468425 Inf -0.07091286 0.1042462
#>  Combustibles     0.08994314 0.04468425 Inf  0.00236362 0.1775227
#>  E-Cigarettes     0.02764748 0.04468425 Inf -0.05993205 0.1152270
#>  Non-Combustibles 0.09257285 0.04468425 Inf  0.00499333 0.1801524
#> 
#> Degrees-of-freedom method: asymptotic 
#> Confidence level used: 0.95 
#> 
#> Significant interaction: No 
#> 
#> No significant interaction detected (alpha = 0.05 ). Pairwise slope comparisons not performed. 
#> P-value adjustment method: tukey 
# }
```
