# Changelog

## beezdemand 0.3.0

This release ships the TMB mixed-effects modeling tier
([`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md))
and clears the outstanding ticket queue against the
`feat/tmb-mixed-effects` branch. See the “TMB tier” section under New
Features for orientation.

### New Features

#### TMB mixed-effects modeling tier

- [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  is the new modern mixed-effects path for behavioral economic demand
  models, alongside the existing NLME tier
  ([`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)).
  Backed by Template Model Builder (TMB) with automatic differentiation
  and Laplace approximation. Supports four equations (`exponential`,
  `exponentiated`, `simplified`, `zben`), 1-RE (Q0 only) or 2-RE
  (correlated Q0 + alpha) random-effect structures, estimated or fixed
  `k`, factor and continuous covariates with asymmetric
  `collapse_levels`, and three data-adaptive starting-value strategies
  via `multi_start = TRUE` (default).

- [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  exposes full optimizer controls via `tmb_control`:

  - `optimizer`: `"nlminb"` (default) or `"L-BFGS-B"` for recovering
    from convergence failures (code 1 or 8).
  - `rel_tol`: Convergence tolerance for nlminb (default 1e-10).
  - `lower` / `upper`: Named numeric vectors for parameter bounds on the
    optimizer scale; applied to all occurrences of repeated parameter
    names (e.g., `beta_q0`).
  - `warm_start`: Restart optimization from a previous `fit$opt$par`
    vector. Automatically disables `multi_start`.
  - `trace`: Optimizer trace output (default 0).

- The `beezdemand_tmb` class ships a comprehensive S3 method suite:
  `print`, `summary`, `coef`, `fixef`, `ranef`, `logLik`, `AIC`, `BIC`,
  `nobs`, `predict` (`response`, `parameters`, `demand`), `confint`,
  `residuals`, `fitted`, `vcov`, broom (`tidy`, `glance`, `augment`),
  `get_subject_pars`, `calc_group_metrics`, `get_demand_param_emms`,
  `get_demand_comparisons`, and visualization (`plot`, `plot_qq`,
  `plot_loss_surface`, `plot_loss_profile`, `plot_re_diagnostics`,
  `plot_alpha_distribution`, `plot_elasticity`, `plot_expenditure`,
  `plot_demand_overlay`).

- `vignettes/tmb-mixed-effects.Rmd` walks through the full TMB workflow
  (equations, random-effect structures, diagnostics) with cache-aware
  fast/full mode.

- `vignettes/convergence-guide.Rmd` documents convergence
  troubleshooting for both the TMB and NLME tiers.

#### Hurdle: marginal P(zero) and unconditional Pmax/Omax

- `predict.beezdemand_hurdle(type = "probability", marginal = TRUE)`
  computes population-averaged P(zero) by integrating over the random
  intercept distribution. Methods: `"kde"` (default), `"normal"`,
  `"empirical"`.

- `plot.beezdemand_hurdle(type = "probability")` now shows the marginal
  (population-averaged) P(zero) curve by default. `marginal = FALSE`
  reverts to the old conditional (RE = 0) behavior.

- `calc_group_metrics.beezdemand_hurdle()` now returns both conditional
  and unconditional Pmax / Omax (TICKET-003). Conditional (`$Pmax`,
  `$Omax`) keeps its long-standing Part-II-only meaning. New
  `$Pmax_unconditional` / `$Omax_unconditional` come from optimizing
  `p * (1 - P0(p)) * Q(p)` over the observed price domain. Subject-level
  `subject_pars` likewise gains `Pmax_unconditional` /
  `Omax_unconditional` columns.
  [`summary()`](https://rdrr.io/r/base/summary.html)’s `derived_metrics`
  reports both sets, with `component = "unconditional"` on the new rows.

- [`plot_expenditure.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/plot_expenditure.md)
  gains `demand_type = c("unconditional", "conditional")` (default
  unconditional). The displayed expenditure curve and the Pmax/Omax
  reference lines now come from the same metric set, so they always
  align — fixing the visible misalignment where the curve used
  `(1 - P0) * Q` but the reference lines used the Part-II-only metrics.

#### Other

- New `calculate_amplitude_persistence.beezdemand_tmb()` method
  (TICKET-004) lets users compute amplitude/persistence factors directly
  from
  [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  results (default persistence components: `c("Pmax", "Omax", "alpha")`
  — TMB `subject_pars` does not include `breakpoint`).

- New cross-price S3 methods (TICKET-005): `print`, `augment`, `confint`
  (lm + lmer), and `nobs` for `cp_model_nls`, `cp_model_lm`, and
  `cp_model_lmer`. Augment for lmer additionally includes a `.fixed`
  column (population-level prediction with random effects = 0). All
  methods handle a NULL underlying model gracefully.

- Linearized marginal NLL surface for
  [`plot_loss_surface()`](https://brentkaplan.github.io/beezdemand/reference/plot_loss_surface.md)
  on NLME models (commit `2bedd29`).

- Visualization helpers added to TMB and NLME vignettes (commit
  `75a202a`).

### Bug Fixes

- [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  now drops rows with `NA` values in any modeling column (`id`, price,
  response, factors, continuous covariates) before entering the TMB
  pipeline, matching the
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
  behavior. Previously a single `NA` in the response crashed
  [`.tmb_prepare_data()`](https://brentkaplan.github.io/beezdemand/reference/dot-tmb_prepare_data.md)
  with `"missing value where TRUE/FALSE needed"`, and `NA` in factors or
  covariates could propagate into
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) and
  trigger a TMB segfault during `MakeADFun()`.

- [`predict.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md)
  now rebuilds the fixed-effect linear predictor from `newdata` instead
  of reusing training-time `subject_pars$Q0` / `alpha`. Predictions for
  any model with factors or continuous covariates now correctly reflect
  the values supplied in `newdata`; unknown subjects fall back to the
  newdata fixed effects with random effects = 0 (with a warning).
  Previously the function silently used cached subject parameters for
  known subjects and the reference-level intercepts for unknown ones,
  producing systematically biased `.fitted` values.
  [`augment.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_tmb.md)
  inherits the fix. Predict now also errors clearly when `newdata` is
  missing a required modeling column or contains factor levels not seen
  in training.

- [`get_demand_param_emms.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.beezdemand_tmb.md)
  and
  [`get_demand_comparisons.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.beezdemand_tmb.md)
  now include continuous covariates in the reference grid, matching the
  dimensionality of the fitted `beta` coefficients. Covariates default
  to their training-data mean (matching
  [`emmeans::ref_grid`](https://rvlenth.github.io/emmeans/reference/ref_grid.html))
  and can be overridden via `at = list(covname = value)`. Previously,
  TMB fits that mixed factors and continuous covariates produced
  `non-conformable arguments` in the Wald variance calculation or
  silently used the wrong model basis.

- [`get_demand_comparisons()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.md)
  now restricts pairwise contrasts to observed factor combinations.
  Previously, with unbalanced designs (e.g., different dose levels per
  drug), the function computed contrasts on the full factorial grid,
  producing phantom comparisons for non-existent factor combinations and
  identical estimates across `contrast_by` groups in additive models.

- [`summary.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_nlme.md)
  with `report_space != internal_space` now preserves nlme’s
  containment-based degrees of freedom across the delta-method parameter
  transformation, computing p-values via
  [`stats::pt()`](https://rdrr.io/r/stats/TDist.html) instead of
  [`stats::pnorm()`](https://rdrr.io/r/stats/Normal.html). Small-N
  inference is no longer anti-conservative on natural-scale summaries
  (TICKET-006).

- `summary.beezdemand_hurdle()$coefficients_matrix` is now labelled
  `"z value"` (was `"t value"`) to match the pnorm-based p-value
  computation. TMB-based hurdle models use Laplace approximation, so the
  z-test is the correct asymptotic inference (matches the glmmTMB
  convention) — only the label was wrong (TICKET-006).

- [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
  and
  [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
  now expose `hessian_pd` on the fit object and warn at fit time when
  the Hessian is not positive definite (`pdHess = FALSE`).
  [`summary()`](https://rdrr.io/r/base/summary.html) adds a
  corresponding note;
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) sets a
  `hessian_warning` attribute on its output. Previously these models
  silently reported unreliable standard errors / p-values / Wald
  intervals when the Hessian was singular (TICKET-008). Backwards
  compatible: legacy fit objects without the field still work.

- [`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
  no longer crashes when called on `beezdemand_tmb` objects (TICKET-002
  — the underlying name + residual fields were already corrected in
  commit `719c0ed`; this release adds the regression coverage that pins
  the fix).

- [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
  correctly handles `NA` values in the input data and strips spurious
  names from start values (commit `b39c24a`).

- Visualization improvements — smoothing, value clamping, and APA
  styling for diagnostic plots (commit `bf125f6`).

- Comprehensive package audit fixes — boundary detection, data
  validation, heuristic improvements (commit `60b13a2`).

### Quality / Tooling

- Bare [`stop()`](https://rdrr.io/r/base/stop.html) /
  [`warning()`](https://rdrr.io/r/base/warning.html) /
  [`message()`](https://rdrr.io/r/base/message.html) calls in non-legacy
  R files replaced with their cli equivalents
  ([`cli::cli_abort`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_warn`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_inform`](https://cli.r-lib.org/reference/cli_abort.html))
  and the package’s structured error helpers (`validation_error`,
  `fitting_error`, `missing_package_error` — TICKET-009). Errors now
  carry stable class tags (e.g., `"beezdemand_validation_error"`) for
  programmatic catch-handling. Bare calls in legacy `R/analyze.R` are
  intentionally left in place; that file is slated for removal in v1.0.
  The error helpers themselves were refactored to use
  [`cli::cli_abort`](https://cli.r-lib.org/reference/cli_abort.html)
  internally so callers can pass cli inline markup (`{.arg}`,
  `{.field}`, `{.val}`, `{.fn}`).

- Test coverage added for four previously-untested exported functions
  (TICKET-007):
  [`cp_posthoc_slopes()`](https://brentkaplan.github.io/beezdemand/reference/cp_posthoc_slopes.md),
  [`cp_posthoc_intercepts()`](https://brentkaplan.github.io/beezdemand/reference/cp_posthoc_intercepts.md),
  [`extract_coefficients()`](https://brentkaplan.github.io/beezdemand/reference/extract_coefficients.md),
  and
  [`get_demand_param_trends()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_trends.md).
  23 new tests across three new test files using the `etm` and `ko`
  example datasets.

- New regression tests for
  [`check_demand_model.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
  (1-RE and 2-RE), `calculate_amplitude_persistence.beezdemand_tmb()`,
  [`summary.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_nlme.md)
  p-value preservation,
  [`summary.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_hurdle.md)
  coefficient-matrix labelling, `hessian_pd` propagation on TMB and
  hurdle fits, the cross-price S3 methods, hurdle unconditional
  Pmax/Omax, and the structured error class hierarchy.

## beezdemand 0.2.0

CRAN release: 2026-03-03

### Deprecations

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  is now superseded by
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  will continue to work but emits a soft deprecation warning. The new
  function provides a modern S3 interface with
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods. See
  [`vignette("migration-guide")`](https://brentkaplan.github.io/beezdemand/articles/migration-guide.md)
  for migration instructions.

- [`FitMeanCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitMeanCurves.md)
  is now superseded by `fit_demand_fixed(agg = "Mean")` or
  `fit_demand_fixed(agg = "Pooled")`.

### New Features

#### Koffarnus Equation for Mixed-Effects Models

- [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
  now supports the Koffarnus et al. (2015) exponentiated equation via
  `equation_form = "koff"`. This enables fitting demand curves using the
  same equation form available in
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  within the modern hierarchical mixed-effects framework. The `k`
  parameter can be user-specified or auto-calculated from data range.

#### broom Integration

- New [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods for all model classes provide fitted values and residuals in a
  tidy tibble:
  - [`augment.beezdemand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_fixed.md):
    Returns `.fitted`, `.resid`
  - [`augment.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_hurdle.md):
    Returns `.fitted`, `.fitted_link`, `.fitted_prob`, `.resid`,
    `.resid_response`
  - [`augment.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/augment.beezdemand_nlme.md):
    Returns `.fitted`, `.resid`, `.fixed`

#### Model Comparison Framework

- New
  [`compare_models()`](https://brentkaplan.github.io/beezdemand/reference/compare_models.md)
  function for unified model comparison across all beezdemand model
  classes. Reports AIC, BIC, delta_AIC, delta_BIC, and performs
  likelihood ratio tests when models are from the same backend and
  nested.

- New [`anova()`](https://rdrr.io/r/stats/anova.html) S3 methods for
  comparing nested models:

  - [`anova.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/anova.beezdemand_hurdle.md):
    LRT for nested hurdle models
  - [`anova.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/anova.beezdemand_nlme.md):
    Delegates to nlme::anova.lme()

#### Model Diagnostics Suite

- New
  [`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
  generic with methods for all model classes. Performs comprehensive
  diagnostics including convergence checks, boundary condition
  detection, random effect variance assessment, and residual outlier
  detection. Returns structured diagnostics object with issues and
  recommendations. (Named
  [`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
  to avoid conflict with
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html).)

- New
  [`plot_residuals()`](https://brentkaplan.github.io/beezdemand/reference/plot_residuals.md)
  function creates diagnostic plots: residuals vs fitted, histogram of
  residuals, and Q-Q plots. Works with all model classes via the
  [`augment()`](https://generics.r-lib.org/reference/augment.html)
  infrastructure.

- New
  [`plot_qq()`](https://brentkaplan.github.io/beezdemand/reference/plot_qq.md)
  function creates Q-Q plots for random effects to assess normality
  assumptions in hurdle and NLME models.

#### Normalized Alpha (Alpha Star)

- All model classes now compute `alpha_star` (normalized alpha, Strategy
  B; Rzeszutek et al., 2025), which makes the elasticity parameter
  comparable across different values of `k`. Available in
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  output (columns `alpha_star` and `alpha_star_se`),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) on
  `beezdemand_fixed` objects, and
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) on
  `beezdemand_hurdle` objects. Standard errors are obtained via the
  delta method. See `?param-registry` for details.

#### Modern Wrappers for Legacy Functions

- New
  [`get_empirical_measures()`](https://brentkaplan.github.io/beezdemand/reference/get_empirical_measures.md)
  as a modern replacement for
  [`GetEmpirical()`](https://brentkaplan.github.io/beezdemand/reference/GetEmpirical.md).
  Returns a `beezdemand_empirical` S3 object; access the results via
  `$measures`.

- New
  [`get_descriptive_summary()`](https://brentkaplan.github.io/beezdemand/reference/get_descriptive_summary.md)
  as a modern replacement for
  [`GetDescriptives()`](https://brentkaplan.github.io/beezdemand/reference/GetDescriptives.md).
  Returns a `beezdemand_descriptive` S3 object; access the results via
  `$statistics`.

- New
  [`get_k()`](https://brentkaplan.github.io/beezdemand/reference/get_k.md)
  as a modern replacement for
  [`GetK()`](https://brentkaplan.github.io/beezdemand/reference/GetK.md).
  Returns a single numeric k value with optional verbose output.

#### Other New Features

- New [`confint()`](https://rdrr.io/r/stats/confint.html) methods for
  extracting confidence intervals from all model classes:
  `beezdemand_fixed`, `beezdemand_hurdle`, `beezdemand_nlme`, and
  `cp_model_nls`.

- New migration guide vignette
  ([`vignette("migration-guide")`](https://brentkaplan.github.io/beezdemand/articles/migration-guide.md))
  documenting the transition from
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  to
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).

### Breaking Changes

- [`summary()`](https://rdrr.io/r/base/summary.html) methods for
  `beezdemand_hurdle` and `beezdemand_nlme` now return structured
  summary objects instead of printing directly. Use
  `print(summary(fit))` for console output. Programmatic access is now
  possible: `s <- summary(fit); s$coefficients`.

- [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
  now fits demand parameters in natural-log space (`log_q0`,
  `log_alpha`, `log_k`) and reports back-transformed values; the
  `param_space` argument has been removed.

- [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)
  now uses log10-parameterized optimizer coefficients (`log10_qalone`,
  `I`, `log10_beta`) across equation forms; the `"exponential"` form
  fits on the `log10(y)` response scale and filters `y <= 0` with a
  warning.
  [`predict.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/predict.cp_model_nls.md)
  now always returns `y_pred` on the natural `y` scale; for
  `"exponential"` it additionally returns `y_pred_log10` (and no longer
  returns `y_pred_natural`).

#### Additional New Features

- New
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
  function provides a modern interface for individual demand curve
  fitting. Returns a structured S3 object with
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
  [`glance()`](https://generics.r-lib.org/reference/glance.html)
  methods. This wrapper offers the same functionality as
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  but with a standardized API.

- New systematicity wrappers with unified output vocabulary:

  - [`check_systematic_demand()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_demand.md)
    for purchase task data (wraps
    [`CheckUnsystematic()`](https://brentkaplan.github.io/beezdemand/reference/CheckUnsystematic.md))
  - [`check_systematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_systematic_cp.md)
    for cross-price data (wraps
    [`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md))

  Both return `beezdemand_systematicity` objects with identical column
  schemas (differing only in NA values for domain-specific fields).

- First-class [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  and [`glance()`](https://generics.r-lib.org/reference/glance.html)
  support is now guaranteed across all beezdemand model classes. All
  methods return tibbles with standardized columns including
  `model_class` and `backend`.

- All summary objects now inherit from `beezdemand_summary` base class,
  enabling shared fallback behavior and consistent field availability.

### API Standardization

This release introduces **Stability Contracts** for all model classes:

- **summary() objects** now return structured S3 objects with class
  `c("summary.<class>", "beezdemand_summary")`. Required fields include:
  `call`, `model_class`, `backend`, `nobs`, `n_subjects`, `converged`,
  `logLik`, `AIC`, `BIC`, `coefficients` (tibble), `notes`.

- **tidy() methods** return tibbles with columns: `term`, `estimate`,
  `std.error`, `statistic`, `p.value`. Multi-part models include a
  `component` column (e.g., “fixed”, “variance”, “derived”).

- **glance() methods** return 1-row tibbles with columns: `model_class`,
  `backend`, `nobs`, `n_subjects`, `converged`, `logLik`, `AIC`, `BIC`.

### API Changes

- [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md)
  and
  [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)
  now accept `x_var`/`y_var` to map non-standard column names to
  canonical ones (`"x"`, `"y"`).
  [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)
  additionally accepts `id_var`, `group_var`, and `target_var`. Default
  behavior is unchanged when these arguments are omitted.

- [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md)
  gains explicit `filter_target` and `target_level` top-level arguments
  (previously these were handled implicitly via
  [`validate_cp_data()`](https://brentkaplan.github.io/beezdemand/reference/validate_cp_data.md)).
  Existing calls without these arguments are unaffected.

- `fit_cp_nls(start_vals=)` is deprecated in favor of `start_values=`.
  The old argument still works but emits a deprecation warning.

------------------------------------------------------------------------

## beezdemand 0.1.3

### Deprecations

The following deprecations will take effect in version 0.2.0:

- [`beezdemand::pull()`](https://brentkaplan.github.io/beezdemand/reference/pull.md)
  is deprecated in favor of
  [`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html).
  The beezdemand version was a legacy helper that predates the dplyr
  function.

- The `inverse_fun` argument in
  [`summary.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/summary.cp_model_nls.md),
  [`plot.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/plot.cp_model_nls.md),
  and
  [`predict.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/predict.cp_model_nls.md)
  is deprecated in favor of `inv_fun` for consistency with mixed-effects
  model methods.

### API Improvements

- Standardized argument names across cross-price model methods
  (`inv_fun` instead of `inverse_fun`)

- Cross-price plot methods now have consistent argument ordering across
  [`plot.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/plot.cp_model_nls.md),
  [`plot.cp_model_lm()`](https://brentkaplan.github.io/beezdemand/reference/plot.cp_model_lm.md),
  and
  [`plot.cp_model_lmer()`](https://brentkaplan.github.io/beezdemand/reference/plot.cp_model_lmer.md)

- Key user-facing functions now return tibbles for better compatibility
  with

  tidyverse workflows:
  [`predict.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/predict.cp_model_nls.md),
  [`predict.cp_model_lm()`](https://brentkaplan.github.io/beezdemand/reference/predict.cp_model_lm.md),
  [`tidy.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/tidy.cp_model_nls.md),
  [`glance.cp_model_nls()`](https://brentkaplan.github.io/beezdemand/reference/glance.cp_model_nls.md)

- Added standardized error helpers (`validation_error()`,
  `fitting_error()`, `missing_package_error()`) for consistent error
  messaging

- [`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md)
  now returns an object of class `cp_unsystematic` with proper
  [`summary()`](https://rdrr.io/r/base/summary.html) method dispatch (no
  longer overrides `summary.tbl_df()`)

### New Features

#### Two-Part Mixed Effects Hurdle Demand Models

- Added comprehensive hurdle model functionality using TMB (Template
  Model Builder):

  - [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md):
    Fit two-part hurdle models with 2 or 3 random effects

  - Part I models probability of zero consumption (logistic regression
    with random intercept)

  - Part II models log-consumption given positive response (nonlinear
    mixed effects)

- S3 methods for `beezdemand_hurdle` objects:

  - [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`coef()`](https://rdrr.io/r/stats/coef.html),
    [`logLik()`](https://rdrr.io/r/stats/logLik.html),
    [`AIC()`](https://rdrr.io/r/stats/AIC.html),
    [`BIC()`](https://rdrr.io/r/stats/AIC.html)

  - [`predict()`](https://rdrr.io/r/stats/predict.html): Extract subject
    parameters or predict demand/probability

  - [`plot()`](https://rdrr.io/r/graphics/plot.default.html): Visualize
    demand curves, zero probability, parameter distributions

- Utility functions:

  - [`calc_omax_pmax()`](https://brentkaplan.github.io/beezdemand/reference/calc_omax_pmax.md):
    Calculate Pmax and Omax from demand parameters

  - [`get_subject_pars()`](https://brentkaplan.github.io/beezdemand/reference/get_subject_pars.md):
    Extract subject-specific parameter estimates

  - [`compare_hurdle_models()`](https://brentkaplan.github.io/beezdemand/reference/compare_hurdle_models.md):
    Likelihood ratio test for model comparison

  - [`get_hurdle_param_summary()`](https://brentkaplan.github.io/beezdemand/reference/get_hurdle_param_summary.md):
    Summary statistics for individual parameters

- Simulation functions:

  - [`simulate_hurdle_data()`](https://brentkaplan.github.io/beezdemand/reference/simulate_hurdle_data.md):
    Generate synthetic hurdle model data

  - [`run_hurdle_monte_carlo()`](https://brentkaplan.github.io/beezdemand/reference/run_hurdle_monte_carlo.md):
    Monte Carlo simulation studies

- New vignette “Hurdle Demand Models” with comprehensive examples

- New dataset `apt_full`: Full alcohol purchase task data with 1,100
  subjects and demographic covariates

#### Cross-Price Demand Models

- Added comprehensive cross-price demand model functionality:

  - [`check_unsystematic_cp()`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md):
    Check for unsystematic data in cross-price models

  - [`fit_cp_nls()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_nls.md):
    Nonlinear model fitting for cross-price demand data

  - [`fit_cp_linear()`](https://brentkaplan.github.io/beezdemand/reference/fit_cp_linear.md):
    Linear model fitting for cross-price demand data with options for
    fixed effects and mixed-effects models

  - New utility functions for cross-price model objects:

    - [`summary()`](https://rdrr.io/r/base/summary.html),
      [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
      [`glance()`](https://generics.r-lib.org/reference/glance.html),
      and [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
      methods

  - [`extract_coefficients()`](https://brentkaplan.github.io/beezdemand/reference/extract_coefficients.md):
    Extract model coefficients in tidy format

  - [`cp_posthoc_slopes()`](https://brentkaplan.github.io/beezdemand/reference/cp_posthoc_slopes.md)
    and
    [`cp_posthoc_intercepts()`](https://brentkaplan.github.io/beezdemand/reference/cp_posthoc_intercepts.md):
    Post-hoc comparisons for model parameters

  - [`validate_cp_data()`](https://brentkaplan.github.io/beezdemand/reference/validate_cp_data.md):
    Validate and filter cross-price demand data

- Added new vignette “How to Use Cross-Price Demand Model Functions”
  demonstrating:

  - Required data structure for cross-price analyses

  - Checking for unsystematic data

  - Both two-stage and pooled model fitting approaches

  - Linear and mixed-effects modeling options

  - Model visualization and coefficient extraction

  - Post-hoc comparisons

## beezdemand 0.1.2

CRAN release: 2023-08-26

- No longer relies on `nlmrt` and instead relies on `nlsr`

- Fixes an issue where CheckUnsystematic may not flag certain cases when
  data are passed as `tibble`

- Fixes deprecated arguments in `ggplot2`

- Add ability to specify a start value for alpha in
  [`ExtraF()`](https://brentkaplan.github.io/beezdemand/reference/ExtraF.md)
  function

## beezdemand 0.1.1

- Add experimental features for
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md).
  These arguments are `constrainq0`, `startq0`, and `startalpha`. These
  arguments allow Q0 to be constrained so alpha is the only fitted
  parameter and allow for user-specified starting values.

## beezdemand 0.1.0

CRAN release: 2018-07-31

- Package successfully on CRAN!

## beezdemand 0.1.00

- Package should be ready for CRAN and is being submitted

## beezdemand 0.0.95

### New updates

- One major change that might affect previous scripts is that in output
  summary tables, the column formally named ID is now named id
  (lowercase)

- Cleaned up a few things here and there. The package is close for
  submission to CRAN as it passes R CMD check with no errors, warnings,
  or notes

## beezdemand 0.0.91

### New updates

- [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)
  updated to work better and faster at finding a reasonable value

- Internal helper functions added to optimize
  [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)

## beezdemand 0.0.90

### New updates

- [`ExtraF()`](https://brentkaplan.github.io/beezdemand/reference/ExtraF.md)
  now compares alpha and Q0

- A number of functions now allows you to specify the column names

## beezdemand 0.0.85

### New updates

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  correctly pulls alpha and q0 standard errors when k is fitted as a
  free parameter. Also no longer accepts data transformations. Must be
  done prior to fitting using
  [`ChangeData()`](https://brentkaplan.github.io/beezdemand/reference/ChangeData.md).

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  now fits mean/pooled data based on `method` argument.

- [`GetSharedK()`](https://brentkaplan.github.io/beezdemand/reference/GetSharedK.md)
  no longer accepts data transformations.

## beezdemand 0.0.84

### New updates

- New
  [`ChangeData()`](https://brentkaplan.github.io/beezdemand/reference/ChangeData.md)
  will soon serve as the replacement to
  [`ReplaceZeros()`](https://brentkaplan.github.io/beezdemand/reference/ReplaceZeros.md)
  and other arguments specified in
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md).

- For the time being,
  [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  will actually output a list object. This may cause failures with old
  scripts. Try modifying scripts to take the first element out of the
  list. This will soon be taken care of.

### Tidying

- Email contact has been changed and some minor updates to .rd files.

## beezdemand 0.0.6

### New features

- New
  [`FitMeanCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitMeanCurves.md)
  will fit curve to averaged or pooled data. Can also make plots.

- [`FitCurves()`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md)
  can now make plots.

- [`GetDescriptives()`](https://brentkaplan.github.io/beezdemand/reference/GetDescriptives.md)
  can make box and whisker plots.

### Cleanup

- Old code from previous workflow is removed. Now all functions use
  longform data.
