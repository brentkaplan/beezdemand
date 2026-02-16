# Compare Demand Models

Compare multiple demand models using information criteria and likelihood
ratio tests (when applicable). Works with all beezdemand model classes.

## Usage

``` r
compare_models(..., test = c("auto", "lrt", "none"))
```

## Arguments

- ...:

  Two or more model objects of class `beezdemand_hurdle`,
  `beezdemand_nlme`, or `beezdemand_fixed`.

- test:

  Character; type of statistical test. One of:

  - `"auto"` (default): Use LRT if models are nested and comparable,
    otherwise IC-only.

  - `"lrt"`: Force likelihood ratio test (requires nested models from
    same backend).

  - `"none"`: Only report information criteria, no p-value.

## Value

An object of class `beezdemand_model_comparison` containing:

- comparison:

  Data frame with model fit statistics

- test_type:

  Type of test performed

- lrt_results:

  LRT results if performed (NULL otherwise)

- best_model:

  Index of best model by BIC

- notes:

  Character vector of notes/warnings

- nesting_verified:

  Logical; always FALSE since nesting is not automatically verified.
  Users must ensure models are properly nested for valid LRT
  interpretation.

## Details

Models are compared using AIC and BIC. For models from the same
statistical backend (e.g., two hurdle models or two NLME models),
likelihood ratio tests can be performed if the models are nested.

When comparing models from different backends (e.g., hurdle vs NLME),
only information criteria comparisons are possible since the likelihoods
are not directly comparable for LRT purposes.

### Backend Compatibility

|           |           |                    |
|-----------|-----------|--------------------|
| Backend 1 | Backend 2 | LRT Possible?      |
| hurdle    | hurdle    | Yes (if nested)    |
| nlme      | nlme      | Yes (if nested)    |
| fixed     | fixed     | No (no likelihood) |
| hurdle    | nlme      | No                 |
| hurdle    | fixed     | No                 |
| nlme      | fixed     | No                 |

## Statistical Notes

The likelihood ratio test (LRT) assumes that:

1.  The models are **nested** (the reduced model is a special case of
    the full model obtained by constraining parameters).

2.  Both models are fit to **identical data**.

3.  Under the null hypothesis, the LR statistic follows a chi-square
    distribution with degrees of freedom equal to the difference in the
    number of parameters.

**Important caveat for mixed-effects models:** When variance components
are tested at the boundary (e.g., testing whether a random effect
variance is zero), the standard chi-square distribution is not
appropriate. The correct null distribution is a mixture of chi-squares
(Stram & Lee, 1994). The p-values reported here use the standard
chi-square approximation, which is conservative (p-values are too large)
for boundary tests.

This function does **not** automatically verify that models are nested.
Users should ensure models are properly nested before interpreting LRT
p-values.

## References

Stram, D. O., & Lee, J. W. (1994). Variance components testing in the
longitudinal mixed effects model. *Biometrics*, 50(4), 1171-1177.

## See also

[`compare_hurdle_models()`](https://brentkaplan.github.io/beezdemand/reference/compare_hurdle_models.md)
for the legacy hurdle-specific comparison

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare two hurdle models with different random effects
fit2 <- fit_demand_hurdle(data, random_effects = c("zeros", "q0"))
fit3 <- fit_demand_hurdle(data, random_effects = c("zeros", "q0", "alpha"))
compare_models(fit2, fit3)

# Compare hurdle vs NLME (IC-only)
fit_h <- fit_demand_hurdle(data, random_effects = c("zeros", "q0"))
fit_n <- fit_demand_mixed(data, equation_form = "zben")
compare_models(fit_h, fit_n, test = "none")
} # }
```
