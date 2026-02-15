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
