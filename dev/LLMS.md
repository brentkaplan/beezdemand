# beezdemand — LLM API Card

> Behavioral economic demand analysis in R. Models how consumption
> declines as price increases using purchase task data.

Package version: 0.2.0 Docs: <https://brentkaplan.github.io/beezdemand/>

## Entry Points

| Function                                                                                         | Purpose                               | Returns                         |
|--------------------------------------------------------------------------------------------------|---------------------------------------|---------------------------------|
| [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)   | Individual NLS demand curves          | `beezdemand_fixed`              |
| [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)   | Hierarchical NLME with random effects | `beezdemand_nlme`               |
| [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md) | Two-part hurdle model via TMB         | `beezdemand_hurdle`             |
| [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)               | Nonlinear cross-price elasticity      | `cp_model_nls`                  |
| [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)         | Linear or mixed cross-price           | `cp_model_lm` / `cp_model_lmer` |

All model objects support:
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html),
[`augment()`](https://generics.r-lib.org/reference/augment.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Data Shape

All demand functions expect **long-format** data:

| Column | Description         | Customizable?        |
|--------|---------------------|----------------------|
| `id`   | Subject identifier  | Yes — `id_var` param |
| `x`    | Price (\>= 0)       | Yes — `x_var` param  |
| `y`    | Consumption (\>= 0) | Yes — `y_var` param  |

Cross-price fitters
[`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)
and
[`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)
now also accept `x_var`/`y_var` (and `id_var`/`group_var`/`target_var`
for
[`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md));
defaults remain `x`/`y`/`id`/`group`/`target`.

## Return Object Contracts

### `tidy()` columns

| Class               | Key columns                                                                                  |
|---------------------|----------------------------------------------------------------------------------------------|
| `beezdemand_fixed`  | `id`, `term`, `estimate`, `std.error`, `statistic`, `p.value`, `component`, `estimate_scale` |
| `beezdemand_nlme`   | `term`, `estimate`, `std.error`, `statistic`, `p.value`, `component`, `estimate_scale`       |
| `beezdemand_hurdle` | `term`, `estimate`, `std.error`, `statistic`, `p.value`, `component`, `estimate_scale`       |

### `glance()` columns

| Class               | Key columns                                                                                         |
|---------------------|-----------------------------------------------------------------------------------------------------|
| `beezdemand_fixed`  | `model_class`, `equation`, `k_spec`, `nobs`, `n_subjects`, `n_success`, `n_fail`                    |
| `beezdemand_nlme`   | `model_class`, `equation`, `nobs`, `n_subjects`, `converged`, `logLik`, `AIC`, `BIC`                |
| `beezdemand_hurdle` | `model_class`, `part2`, `random_effects`, `n_subjects`, `nobs`, `converged`, `logLik`, `AIC`, `BIC` |

### `augment()` columns

| Class               | Key columns                                                              |
|---------------------|--------------------------------------------------------------------------|
| `beezdemand_fixed`  | original data + `.fitted`, `.resid`                                      |
| `beezdemand_nlme`   | original data + `.fitted`, `.resid`                                      |
| `beezdemand_hurdle` | original data + `.fitted_prob_zero`, `.fitted_logQ`, `.fitted`, `.resid` |

## Common Pitfalls

1.  **Zeros handling:** NLS models (`fit_demand_fixed`) cannot fit exact
    zeros in log space. Use
    [`ChangeData()`](https://brentkaplan.github.io/beezdemand/reference/ChangeData.md)
    to replace zeros, or use
    [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
    which handles zeros natively via its logistic component.

2.  **Log-space confusion:**
    [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
    with `equation_form = "zben"` expects LL4-transformed y values. Pass
    raw y through
    [`ll4()`](https://brentkaplan.github.io/beezdemand/reference/ll4.md)
    first, then use `inv_fun = ll4_inv` in
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to
    back-transform.

3.  **Equation aliases:** `"hs"` (in `fit_demand_fixed`) corresponds to
    Hursh & Silberberg (2008). The same model is called `"zben"` in
    `fit_demand_mixed` and `"zhao_exponential"` in `fit_demand_hurdle`.
    Different names, same underlying exponential demand equation.

4.  **Cross-price column names:**
    [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)
    and
    [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)
    accept `x_var`/`y_var` (and `id_var`/`group_var`/`target_var` for
    [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md))
    to map non-standard column names to canonical ones. Defaults are
    `x`, `y`, `id`, `group`, `target`. Canonical names are always used
    in the returned `$data`.

5.  **Parameter space:** Many functions default to
    `param_space = "log10"` for estimation stability. Use
    `tidy(fit, report_space = "natural")` to get natural-scale
    estimates.

## More Documentation

- Cheatsheet with runnable code:
  `system.file("llm/cheatsheet.md", package = "beezdemand")`
- Function map: `system.file("llm/docs-map.md", package = "beezdemand")`
- Full function reference:
  <https://brentkaplan.github.io/beezdemand/reference/>
