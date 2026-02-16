# beezdemand Docs Map

## What This Package Does

beezdemand fits behavioral economic demand curves to purchase task data.
It models how consumption of a commodity (e.g., alcohol, cigarettes) declines
as price increases, estimating demand parameters (Q0, alpha, Pmax, Omax).
It supports individual fits, group mixed-effects models, two-part hurdle models
for zero-heavy data, and cross-price substitution models.

## Data Format

All modeling functions expect **long-format** data with three required columns:
- `id` — subject/series identifier
- `x` — price (independent variable, numeric, >= 0)
- `y` — consumption (dependent variable, numeric, >= 0)

Wide-to-long: use `tidyr::pivot_longer()`. Zero replacement: use `ChangeData()`.
For mixed models, consider `ll4()` transformation to handle zeros and wide range.

## Workflows (Vignettes)

| Goal | Vignette |
|------|----------|
| Getting started / full walkthrough | `vignette("beezdemand")` |
| Choosing the right model | `vignette("model-selection")` |
| Group comparisons (extra F-test) | `vignette("group-comparisons")` |
| Mixed-effects demand | `vignette("mixed-demand")` |
| Advanced mixed-effects | `vignette("mixed-demand-advanced")` |
| Hurdle models (zero-heavy data) | `vignette("hurdle-demand-models")` |
| Cross-price demand | `vignette("cross-price-models")` |
| Migrating from FitCurves | `vignette("migration-guide")` |

## Key Functions

### Model Fitting (modern API)
- `fit_demand_fixed()` — individual NLS fits (no random effects)
- `fit_demand_mixed()` — hierarchical NLME with random effects on Q0 and/or alpha
- `fit_demand_hurdle()` — two-part (logistic + NLME) via TMB for zero-heavy data
- `fit_cp_nls()` / `fit_cp_linear()` / `fit_cp_hurdle()` — cross-price models

### Data Quality
- `check_systematic_demand()` — flag nonsystematic purchase task patterns (modern API)
- `check_systematic_cp()` — flag nonsystematic cross-price patterns (modern API)
- `get_descriptive_summary()` — descriptive statistics for purchase task data
- `ChangeData()` — replace zeros for NLS models (legacy)

### Post-Fit Inspection
- `coef()`, `summary()`, `print()`, `plot()` — standard S3 methods
- `get_subject_pars()` — extract per-subject parameters from mixed/hurdle models
- `extract_coefficients()` — tidy tibble of fixed + random effects
- `get_demand_param_emms()` — emmeans for group comparisons

### Transformations
- `ll4()` / `ll4_inv()` — log-logistic compression for wide-range / zero data
- `scale_ll4()` — ggplot2 axis scale

## Reference Index

Full function reference: <https://brentkaplan.github.io/beezdemand/reference/>

LLM entry point: <https://brentkaplan.github.io/beezdemand/llms.txt>
