# beezdemand Cheatsheet

Quick reference for behavioral economic demand analysis in R.

## Installation

```r
# CRAN
install.packages("beezdemand")

# Development version
remotes::install_github("brentkaplan/beezdemand")
```

## Data Format

All functions expect **long-format** data with three columns:

```r
#   id    x     y
#   P001  0     10.0
#   P001  0.5    8.5
#   P001  1.0    7.0
#   P001  2.0    4.0
#   P001  5.0    0.0
```

- `id` — subject identifier
- `x` — price (>= 0)
- `y` — consumption (>= 0)

Column names are customizable in most functions via `id_var`, `x_var`, `y_var` parameters.
**Exception:** `fit_cp_nls()` and `fit_cp_linear()` require hardcoded column names `x`, `y` (and `group`, `target`, `id` for cross-price).

## Data Screening

```r
library(beezdemand)

# Check for nonsystematic responding (Stein et al., 2015)
syst <- check_systematic_demand(
  data,
  trend_threshold  = 0.025,
  bounce_threshold = 0.10,
  max_reversals    = 0,
  x_var = "x", y_var = "y", id_var = "id"
)
print(syst)
tidy(syst)     # per-subject results tibble
glance(syst)   # summary counts

# Cross-price version
syst_cp <- check_systematic_cp(data)
```

## Fixed-Effect Demand (Individual NLS Fits)

```r
fit <- fit_demand_fixed(
  data,
  equation = "hs",          # "hs", "koff", "simplified", "linear", "exponential", "exponentiated"
  k        = 2,             # numeric or "fit" or "share"
  id_var   = "id",
  x_var    = "x",
  y_var    = "y"
)

summary(fit)
tidy(fit)                   # per-subject parameter estimates
glance(fit)                 # model-level summary
augment(fit)                # fitted values + residuals
plot(fit)
```

## Mixed-Effects Demand (NLME)

Best for group-level inference with random effects.

```r
# Transform y for zero handling
data$y_ll4 <- ll4(data$y, lambda = 4)

fit_mix <- fit_demand_mixed(
  data,
  y_var         = "y_ll4",
  x_var         = "x",
  id_var        = "id",
  factors       = "group",         # optional between-subjects factor(s)
  equation_form = "zben",          # "zben", "simplified", "exponentiated"
  param_space   = "log10",         # "log10" or "natural"
  random_effects = Q0 + alpha ~ 1, # random effects formula
  method        = "ML"
)

summary(fit_mix)
tidy(fit_mix)                      # fixed-effect estimates
tidy(fit_mix, effects = "random")  # random effects
augment(fit_mix)
plot(fit_mix, inv_fun = ll4_inv)   # back-transform for display

# Post-hoc group comparisons (emmeans)
get_demand_param_emms(fit_mix, param = "Q0")
get_demand_comparisons(fit_mix, param = "alpha")
```

## Hurdle Demand (TMB, for Zero-Heavy Data)

Two-part model: logistic (zero vs. nonzero) + nonlinear mixed (consumption intensity).

```r
fit_h <- fit_demand_hurdle(
  data,
  y_var          = "y",
  x_var          = "x",
  id_var         = "id",
  random_effects = c("zeros", "q0", "alpha"),  # 3-RE; or c("zeros", "q0") for 2-RE
  epsilon        = 0.001,
  part2          = "zhao_exponential"           # "zhao_exponential", "exponential", "simplified_exponential"
)

summary(fit_h)
tidy(fit_h)                        # fixed effects
tidy(fit_h, effects = "random")    # random effects (per-subject)
glance(fit_h)                      # convergence, AIC, BIC
augment(fit_h)                     # .fitted, .fitted_prob_zero, .resid
plot(fit_h)

# Per-subject derived parameters
get_subject_pars(fit_h)            # Q0, alpha, breakpoint, Pmax, Omax per subject
```

## Cross-Price Models

For modeling substitution between commodities.

```r
# Data must have columns: x, y, group, target, id (hardcoded names)

# Nonlinear cross-price
fit_cp <- fit_cp_nls(
  data,
  equation = "exponentiated"  # "exponentiated", "exponential", "additive"
)

# Linear cross-price
fit_lm <- fit_cp_linear(
  data,
  type = "fixed"              # "fixed" (lm) or "mixed" (lmer)
)

fit_lmer <- fit_cp_linear(
  data,
  type         = "mixed",
  log10x       = TRUE,
  random_slope = TRUE
)

summary(fit_cp)
coef(fit_cp)
plot(fit_cp)
```

## Derived Metrics

```r
# Analytical Pmax and Omax from demand parameters
calc_omax_pmax(Q0 = 10, k = 2, alpha = 0.001)
# Returns list: $Pmax, $Omax, $Qmax

# Observed metrics from raw data
calc_observed_pmax_omax(data, x_var = "x", y_var = "y", id_var = "id")

# Group-level metrics from hurdle model
calc_group_metrics(fit_h)
```

## Transformations

```r
# Log-logistic compression: handles zeros and wide dynamic range
y_trans <- ll4(y, lambda = 4, base = 10)
y_back  <- ll4_inv(y_trans, lambda = 4, base = 10)

# ggplot2 scale for LL4 axis
library(ggplot2)
ggplot(data, aes(x, y_ll4)) +
  geom_point() +
  scale_y_continuous(trans = scale_ll4())
```

## Visualization

```r
# All model objects support plot()
plot(fit)
plot(fit_mix)
plot(fit_h)
plot(fit_cp)

# Customization
plot(fit, x_trans = "log10", y_trans = "pseudo_log")
plot(fit_mix, inv_fun = ll4_inv)

# Diagnostics
plot_residuals(fit_mix)
plot_qq(fit_mix)
plot_subject(fit_h, subject_id = "P001")

# APA theme
plot(fit) + theme_apa()
```

## Equation Reference

| Key       | Name                          | Citation                      |
|-----------|-------------------------------|-------------------------------|
| `"hs"`    | Exponential                   | Hursh & Silberberg (2008)     |
| `"koff"`  | Exponentiated          | Koffarnus et al. (2015)       |
| `"simplified"` | Simplified / SND         | Rzeszutek et al. (2025)                            |
| `"exponentiated"` | Exponentiated          | Koffarnus et al. (2015)                             |
| `"linear"` | Linear                       | Hursh et al. (1989)           |
| `"zben"`  | Zero-Bounded Exponential      | For NLME, pairs with `ll4()`  |

## Vignettes

| Vignette | Title |
|----------|-------|
| `vignette("beezdemand")` | Using beezdemand |
| `vignette("fixed-demand")` | Fixed-Effect Demand Modeling with beezdemand |
| `vignette("model-selection")` | Choosing the Right Demand Model |
| `vignette("group-comparisons")` | Group Comparisons with Extra Sum-of-Squares F-Test |
| `vignette("mixed-demand")` | Mixed-Effects Demand Modeling with beezdemand |
| `vignette("mixed-demand-advanced")` | Advanced Mixed-Effects Demand Modeling |
| `vignette("hurdle-demand-models")` | Hurdle Demand Models |
| `vignette("cross-price-models")` | How to Use Cross-Price Demand Model Functions |
| `vignette("migration-guide")` | Migration Guide: FitCurves to fit_demand_fixed |

## Example Datasets

```r
data(apt, package = "beezdemand")  # Alcohol Purchase Task (Kaplan & Reed, 2018)
data(ko, package = "beezdemand")   # Nonhuman primate demand
data(etm, package = "beezdemand")  # E-cigarette cross-price data
```
