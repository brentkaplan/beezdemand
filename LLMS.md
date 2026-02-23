# beezdemand — LLM API Card

> Behavioral economic demand analysis in R. Models how consumption declines as price increases using purchase task data.

Package version: 0.2.0
Docs: <https://brentkaplan.github.io/beezdemand/>

## Entry Points

| Function | Purpose | Returns |
|----------|---------|---------|
| `fit_demand_fixed()` | Individual NLS demand curves | `beezdemand_fixed` |
| `fit_demand_mixed()` | Hierarchical NLME with random effects | `beezdemand_nlme` |
| `fit_demand_hurdle()` | Two-part hurdle model via TMB | `beezdemand_hurdle` |
| `fit_cp_nls()` | Nonlinear cross-price elasticity | `cp_model_nls` |
| `fit_cp_linear()` | Linear or mixed cross-price | `cp_model_lm` / `cp_model_lmer` |

All model objects support: `tidy()`, `glance()`, `augment()`, `coef()`, `summary()`, `print()`, `plot()`.

## Data Shape

All demand functions expect **long-format** data:

| Column | Description | Customizable? |
|--------|-------------|---------------|
| `id` | Subject identifier | Yes — `id_var` param |
| `x` | Price (>= 0) | Yes — `x_var` param |
| `y` | Consumption (>= 0) | Yes — `y_var` param |

**Cross-price exception:** `fit_cp_nls()` and `fit_cp_linear()` require hardcoded column names `x`, `y`, `group`, `target`, `id` — no `_var` parameters.

## Return Object Contracts

### `tidy()` columns

| Class | Key columns |
|-------|-------------|
| `beezdemand_fixed` | `id`, `term`, `estimate`, `std.error`, `statistic`, `p.value`, `component`, `estimate_scale` |
| `beezdemand_nlme` | `term`, `estimate`, `std.error`, `statistic`, `p.value`, `component`, `estimate_scale` |
| `beezdemand_hurdle` | `term`, `estimate`, `std.error`, `statistic`, `p.value`, `component`, `estimate_scale` |

### `glance()` columns

| Class | Key columns |
|-------|-------------|
| `beezdemand_fixed` | `model_class`, `equation`, `k_spec`, `nobs`, `n_subjects`, `n_success`, `n_fail` |
| `beezdemand_nlme` | `model_class`, `equation`, `nobs`, `n_subjects`, `converged`, `logLik`, `AIC`, `BIC` |
| `beezdemand_hurdle` | `model_class`, `part2`, `random_effects`, `n_subjects`, `nobs`, `converged`, `logLik`, `AIC`, `BIC` |

### `augment()` columns

| Class | Key columns |
|-------|-------------|
| `beezdemand_fixed` | original data + `.fitted`, `.resid` |
| `beezdemand_nlme` | original data + `.fitted`, `.resid` |
| `beezdemand_hurdle` | original data + `.fitted_prob_zero`, `.fitted_logQ`, `.fitted`, `.resid` |

## Common Pitfalls

1. **Zeros handling:** NLS models (`fit_demand_fixed`) cannot fit exact zeros in log space. Use `ChangeData()` to replace zeros, or use `fit_demand_hurdle()` which handles zeros natively via its logistic component.

2. **Log-space confusion:** `fit_demand_mixed()` with `equation_form = "zben"` expects LL4-transformed y values. Pass raw y through `ll4()` first, then use `inv_fun = ll4_inv` in `plot()` to back-transform.

3. **Equation aliases:** `"hs"` (in `fit_demand_fixed`) corresponds to Hursh & Silberberg (2008). The same model is called `"zben"` in `fit_demand_mixed` and `"zhao_exponential"` in `fit_demand_hurdle`. Different names, same underlying exponential demand equation.

4. **Cross-price column names:** Unlike demand functions, `fit_cp_nls()` and `fit_cp_linear()` do **not** accept `x_var`/`y_var` parameters. Columns must be named exactly `x`, `y`, `group`, `target`, `id`.

5. **Parameter space:** Many functions default to `param_space = "log10"` for estimation stability. Use `tidy(fit, report_space = "natural")` to get natural-scale estimates.

## More Documentation

- Cheatsheet with runnable code: `system.file("llm/cheatsheet.md", package = "beezdemand")`
- Function map: `system.file("llm/docs-map.md", package = "beezdemand")`
- Full function reference: <https://brentkaplan.github.io/beezdemand/reference/>
