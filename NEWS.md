# beezdemand 0.3.0 (development)

This release ships the TMB mixed-effects modeling tier
(`fit_demand_tmb()`) along with TICKET-011 — factor-expanded and
multi-block random-effects support, expanded subject-level reporting,
and group-metrics conditioning. The "TMB mixed-effects modeling tier"
section below is the original 0.3.0 introduction; subsequent sections
cover TICKET-011 phase work added under this development cycle.

## TMB post-fit fixes (TICKET-011 Phase 0)

* `fit_demand_tmb()` now validates that every column of the fixed-effect
  design matrix is constant within each `id`. When a factor or continuous
  covariate varies within subject, `subject_pars$Q0`, `$alpha`, `$Pmax`,
  and `$Omax` are set to `NA_real_` for affected subjects and a
  `cli::cli_warn()` names the offending columns. Previously the function
  silently returned row-order-dependent values. New
  `validate_subject_pars = TRUE` argument provides an escape hatch for
  users who have reasoned about the behavior. Factor-expanded random
  slopes landed in TICKET-011 Phase 2 (single-block) and Phase 3
  (multi-block); the silent first-observed-row fallback was retired
  in Phase 5A in favor of NA-on-within-id-variation plus an opt-in
  `expanded` argument on `get_subject_pars()`.
* `get_demand_param_emms()` on a `beezdemand_tmb` fit now honors
  `continuous_covariates` even when no factors are present. Previously,
  the early-return for factor-less models ignored the `at` argument and
  always returned the intercept.
* `get_demand_param_emms()` on a `beezdemand_tmb` fit now raises a clear
  error when `factors_in_emm` drops any fitted factor. Previously, the
  shorter reference row was silently recycled against the full
  coefficient vector, either producing wrong numbers or crashing
  downstream with a generic "non-conformable arguments" error. Proper
  marginalization over omitted factors lands in TICKET-011 Phase 5.

## TMB random-effects API (TICKET-011 Phase 1)

* `fit_demand_tmb()` now accepts formula-based `random_effects`
  arguments. The default signature is `random_effects = Q0 + alpha ~ 1`,
  equivalent to the previous `c("q0", "alpha")` default. Single-parameter
  `Q0 ~ 1`, `pdMat` objects, lists of `pdMat`, and `nlme::pdBlocked` are
  all parsed and attached to the fit object's
  `$param_info$random_effects_parsed` as a canonical block representation.
* `fit_demand_tmb()` gains `covariance_structure = c("pdSymm", "pdDiag")`
  matching the `fit_demand_mixed()` argument of the same name.
* Character-vector inputs to `random_effects` (e.g. `c("q0", "alpha")`)
  are soft-deprecated via `lifecycle::deprecate_soft()` and internally
  translated to the equivalent formula. A hard deprecation follows in
  0.4.0.
* Formula shapes richer than intercept-only (e.g.
  `Q0 + alpha ~ condition`, `pdBlocked(list(...))`) are now fully
  supported. Template generalization to a Z-matrix-driven covariance
  landed in TICKET-011 Phase 2 (single-block factor expansion) and
  Phase 3 (multi-block `pdBlocked`).
* New internal helpers in `R/random-effects-utils.R`
  (`.classify_re_input`, `.normalize_re_input`, `.validate_re_input`,
  `.re_is_phase1_fittable`, `.deprecate_character_re`,
  `.re_shape_summary`, `.re_parsed_to_character`) factor out the
  formula/pdMat parsing so both `fit_demand_mixed()` and
  `fit_demand_tmb()` consume the same canonical representation.

## Additional TMB post-fit fixes (TICKET-011 Phase 0.4-0.5)

Adversarial review surfaced two more silent wrong-answer paths in the
TMB post-fit layer; both are sister bugs to Phase 0.2 / 0.3 fixes and
land here as the foundation for the Phase 2 factor-RE work.

* `get_demand_comparisons()` on a `beezdemand_tmb` fit now consumes the
  same conditioned reference grid as `get_demand_param_emms()`. New
  internal helper `.tmb_build_emm_ref_grid()` in `R/tmb-methods.R`
  ensures both functions honor `at` (factor-level filters AND
  continuous-covariate value overrides) and `factors_in_emm`. Before
  this fix the wrapper forwarded `...` to `emms` but rebuilt its own
  contrast grid from the unfiltered training data, producing off-grid
  contrasts and `"NA"` labels when `at` filtered factor levels.
* `calc_group_metrics()` on a `beezdemand_tmb` fit now warns and
  returns a `conditioned_on` field when continuous covariates are
  present. The numeric output is unchanged (intercept-only `Q0` /
  `alpha`, i.e. covariates held at 0) but the warning matches the
  convention from `predict(type = "demand")` so users cannot
  silently misread reference-intercept metrics as population means.
  `summary.beezdemand_tmb()` propagates the warning through the
  standard summary path. Phase 5 will replace warn-and-label with
  explicit conditioning via the `.tmb_build_emm_ref_grid()` helper.

## Factor-expanded TMB random effects (TICKET-011 Phase 2)

* `fit_demand_tmb()` now fits formula-based random effects with
  factor-expanded slopes (e.g. `pdDiag(Q0 + alpha ~ condition)` or
  `pdSymm(Q0 + alpha ~ condition)`). Single-block pdDiag and pdSymm
  with arbitrary RHS terms are accepted. Multi-block `pdBlocked` /
  `list()` of pdMats lands in Phase 3 (also in this release).
* `src/MixedDemand.h` rewritten with a block-aware DATA interface
  (Z_q0, Z_alpha, block-structure metadata) and a generalized
  per-block Cholesky loop. pdSymm blocks of size > 2 use the
  Lewandowski-Kurowicka-Joe Cholesky construction; the d == 2 case
  reduces exactly to the previous `tanh(rho_bc_raw)` parameterization,
  so existing intercept-only fits produce bit-identical loglik /
  coefficients (verified to 1e-10 on `apt`).
* New internal helpers `.tmb_build_z_matrices()` and
  `.tmb_build_block_map()` in `R/tmb-demand.R` consume the canonical
  block representation and emit the design matrices and metadata the
  template needs.
* `.tmb_compute_subject_pars()` generalized into a per-block
  reconstruction. For factor-expanded fits, the Phase 2 implementation
  used first-observed-row `X` and `Z` for subject-level `Q0` / `alpha`;
  Phase 5A in this release supersedes that with NA-on-within-id-
  variation plus an opt-in `expanded = TRUE` argument on
  `get_subject_pars()` for per-(subject, condition) rows.
* New simulator `.simulate_within_subject_demand()` and parity tests
  confirm TMB Laplace approximation agrees with NLME's iterative
  algorithm to within ~1% on the loglik across all four target specs.

## Multi-block pdBlocked random effects (TICKET-011 Phase 3)

* `fit_demand_tmb()` now accepts multi-block `nlme::pdBlocked(list(...))`
  and bare `list(pdMat, pdMat, ...)` `random_effects` specifications.
  The motivating use case is the load-bearing M1 spec from in-house
  manuscript work:
  `pdBlocked(list(pdSymm(Q0+alpha~1), pdDiag(Q0+alpha~condition-1)))`,
  which combines a correlated subject-baseline block with an
  uncorrelated subject-by-condition slopes block. The intercepts-only
  alternative inverts the cigarette Q0 direction on that data; the
  multi-block spec recovers it.
* The Phase 2 C++ template, parser, and R glue already supported
  `n_blocks > 1`; Phase 3 lifts the gate that rejected those shapes.
  The renamed gate helper `.re_is_phase3_fittable()` accepts any number
  of single-grouping-level blocks of class `pdDiag` or `pdSymm`. Other
  pdMat classes (`pdCompSymm`, `pdIdent`, `pdLogChol`, ...) remain
  unsupported pending a triggering use case; use `fit_demand_mixed()`
  for those.
* Bit-identical regression: `pdBlocked(list(pdSymm(Q0+alpha~1)))`
  (single-block-wrapped) produces the same loglik and coefficients as
  bare `pdSymm(Q0+alpha~1)` to optimizer tolerance.
* Acceptance: on simulated within-subject data with a known per-condition
  Q0 ordering, the M1 spec recovers the truth ordering and matches NLME
  EMMs within 5% on natural scale (manuscript-repo parity protocol
  remains the integration gate against actual study data).

## Documentation: advanced random-effects vignette (TICKET-011 Phase 5B)

* New vignette `tmb-advanced-random-effects.Rmd` covers all
  random-effects structures beyond intercepts-only:
  - Decision tree for picking between intercepts-only,
    factor-expanded single-block (Phase 2), and multi-block
    `pdBlocked` (Phase 3).
  - Worked example of the cigarette M1 spec on simulated within-
    subject data, demonstrating per-condition Q0 ordering recovery.
  - Reading subject-level results: long-form
    `get_subject_pars(fit, expanded = TRUE)` and `attr(re_q0_mat)` /
    `re_alpha_mat` access for power users.
  - Group metric conditioning with the `at` argument and the
    parameter-first marginalization convention.
  - Diagnostics for variance components per block and convergence
    troubleshooting.
* Existing `tmb-mixed-effects.Rmd` stays as the intro tier
  (intercept-only and basic 2-RE).

## Group-level metric conditioning (TICKET-011 Phase 5C)

* `calc_group_metrics.beezdemand_tmb()` gains an `at` argument for
  explicit conditioning on continuous covariates and factor levels.
  When `at = NULL` (default), continuous covariates are evaluated at
  their training mean and factors are marginalized across observed
  levels (equal weights). The argument shape matches
  `get_demand_param_emms()` / `get_demand_comparisons()`:
  `at = list(age = 30, gender = "Male")`.
* The Phase 0.5 `cli::cli_warn()` for "covariates held at 0" is retired
  entirely. The new training-mean default is statistically defensible,
  so the warning would only train users to ignore warnings. The
  `conditioned_on` field in the return list still labels the actual
  conditioning point (covariate values used; per-factor treatment, with
  `"marginal"` for the default and the supplied level when `at` is
  given).
* For derived metrics that depend nonlinearly on (Q0, alpha) jointly
  (Pmax, Omax, Qmax), this function uses **parameter-first
  marginalization**: log-Q0 and log-alpha EMMs are computed on the
  reference grid via the shared `.tmb_build_emm_ref_grid()` helper,
  marginalized with equal weights, then Pmax/Omax/Qmax are derived from
  the marginalized parameter values. This matches the parameter-level
  marginalization convention used by `get_demand_param_emms()`. (Note:
  this differs from "compute metrics per cell, then average"; the two
  approaches give different answers for nonlinear transforms.)
* `summary.beezdemand_tmb()` now prints a single line under the
  Population Demand Metrics block — `Metrics conditioned at: <cov>=<X>,
  <factor>=marginal` — surfacing the conditioning point so a printed
  summary is self-describing.

## Subject-level reporting for factor-expanded fits (TICKET-011 Phase 5A)

* `get_subject_pars()` gains an opt-in `expanded` argument. When
  `expanded = TRUE` and the fit's random-effects design varies within
  id (e.g. M1-style multi-block fits with `condition - 1` slopes), the
  function returns a long-form table with one row per (subject,
  factor-level) combination. Columns include the within-subject factor
  names plus model-derived per-cell `Q0`, `alpha`, `Pmax`, and `Omax`.
  Numeric within-id-varying RE-RHS terms are conditioned at the
  subject's mean rather than expanded.
* Default `expanded = FALSE` returns the wide one-row-per-subject table
  unchanged; `predict()`, `ranef()`, and other consumers that depend on
  unique IDs continue to work bit-for-bit.
* The Phase 0 within-id check now also examines `Z_q0` / `Z_alpha`
  (random-effects design) columns, not just `X_q0` / `X_alpha`. Prior
  to this fix, M1-style fits where `condition` appeared only in
  `random_effects` (not in `factors`) silently returned first-observed-
  row Q0/alpha values without warning. Now those fits emit the
  `subject_pars` validation warning and set affected subjects'
  `Q0`/`alpha`/`Pmax`/`Omax` to `NA` in the default wide table, with a
  pointer to `expanded = TRUE` for per-(subject, condition) values.
* Downstream consumers that read `subject_pars$Q0` / `$alpha` directly
  (`plot(fit, type = "individual")` and
  `calculate_amplitude_persistence()`) now abort with a targeted message
  when those columns are NA, pointing users at
  `get_subject_pars(fit, expanded = TRUE)`. Native expanded-shape
  support in those consumers is deferred to a follow-up release.
* Generic signatures: `get_subject_pars()` and `calc_group_metrics()`
  generics gain `...` so the new `expanded` and `at` arguments dispatch
  through `UseMethod()` correctly. All existing methods updated.

## Formula, design-matrix, and update introspection (TICKET-028)

* Added `formula()`, `model.matrix()`, and `update()` methods for
  `beezdemand_tmb`, plus `formula()` and `model.matrix()` for
  `beezdemand_hurdle` (five new S3 methods total).
* `formula(fit_tmb)` returns `list(Q0, alpha, random)` — one-sided
  formulas for Q0 and alpha (reconstructed from
  `fit$formula_details$rhs_q0` / `$rhs_alpha`, so they reflect any
  asymmetric `collapse_levels`) plus the original `random_effects`
  spec preserved at fit time and round-trippable back to
  `fit_demand_tmb()`.
* `model.matrix(fit_tmb)` returns a **named list** of four matrices
  (`X_q0`, `X_alpha`, `Z_q0`, `Z_alpha`); use `what = ...` to select
  one. The list-rather-than-matrix return is intentional and
  documented: the TMB tier has two fixed-effect linear predictors
  (one per nonlinear parameter), not one. `X_q0` / `X_alpha` are
  zero-copy references to `fit$formula_details`; `Z_q0` / `Z_alpha`
  are recomputed via the internal `.tmb_build_z_matrices()` helper.
  Degenerate Z requests (e.g., `what = "Z_alpha"` on a Q0-only fit)
  return `NULL` with an informational message.
* `update(fit_tmb, ...)` re-fits with named arguments substituted
  into the original call (e.g., `update(fit, factors = NULL)`).
  Honors `evaluate = FALSE` per the `stats::update.default`
  convention. Does **not** support formula-update syntax
  (`. - term`); `fit_demand_tmb()` is argument-driven, not
  formula-driven.
* `formula(fit_hurdle)` returns `list(binary, consumption, random)`
  with both component formulas intercept-only today. Future support
  for factor/covariate effects on hurdle components will enrich
  these without changing the API.
* `model.matrix(fit_hurdle)` returns intercept-only design matrices
  (`X_binary`, `X_consumption`) for parity with
  `model.matrix.beezdemand_tmb()`.
* `beezdemand_tmb` fits now store the original `fit_demand_tmb()`
  call as `fit$call` so `update()` can rebuild it. `fit_demand_hurdle()`
  already captured this slot.
* Does **not** unlock `emmeans` / `effects` / `ggeffects`
  (need `recover_data` + `emm_basis` methods), `drop1` / `add1` /
  `MuMIn::dredge` (need `terms()` + formula-update `update()` form),
  or any tool that dispatches via `coef()` after the planned
  TICKET-019 default change. Those each remain follow-up tickets.

## Variance-covariance, fitted, and residual accessors (TICKET-026)

* Added `vcov()`, `fitted()`, and `residuals()` methods for both
  `beezdemand_tmb` and `beezdemand_hurdle` (six new S3 methods total).
  TMB methods default to the model's native likelihood scale (log scale
  for `"exponential"`, natural/LL4 scale for others), matching
  `broom::augment(fit)$.fitted` / `$.resid`. `scale = "natural"` opts
  into back-transformed values; `type = "pearson"` divides by the
  residual SD on the model scale. Requesting `type = "pearson"` with
  `scale = "natural"` falls back to `type = "response"` with an
  informational message because a response-scale residual SD is not
  identified for the exponential/zben variants without a separate
  variance assumption.
* `vcov.beezdemand_hurdle()` returns the joint fixed-effect VCOV with
  row/column names prefixed by component (`zero_probability.<term>`,
  `consumption.<term>`, `variance.<term>`) so downstream tools can
  index by component without an ad-hoc string match.
* `fitted.beezdemand_hurdle(marginal = TRUE)` (default) returns
  marginal expected consumption `P(y > 0) * E(y | y > 0)` (the
  `.fitted` column of `predict(fit, type = "demand")`);
  `marginal = FALSE` returns the conditional-on-positive expectation.
* `coef.beezdemand_tmb()` gained a forward-compatible `type = "internal"`
  alias that returns the optimizer's flat parameterization (current
  default behavior). This preserves the numeric-vector escape hatch
  consumed by `car::deltaMethod`, `multcomp::glht`, and similar tooling
  across the planned `coef()` default change to a per-subject tibble.
* `augment.beezdemand_tmb()` was refactored to share an internal
  `.tmb_fitted_resid()` helper with the new `fitted()` and `residuals()`
  methods, eliminating the duplicate predict() call and guaranteeing
  the three accessors cannot drift apart on scale convention. Output
  is bit-identical to the previous implementation.
* Does **not** automatically unlock `parameters::standard_error`
  (needs `insight::get_parameters` class-aware dispatch),
  `DHARMa::simulateResiduals` (needs a `simulate()` method), or
  `performance::check_residuals` (needs class-specific dispatch). The
  explicit numeric-vector forms of `car::deltaMethod` and
  `multcomp::glht` are unlocked when the caller pre-extracts
  `coef(fit, type = "internal")`.

## Universal-accessor parity for hurdle fits (TICKET-027)

* Added `nobs.beezdemand_hurdle()` for universal-accessor parity with
  `nobs.beezdemand_tmb()` and the cross-price classes. `broom::glance(fit)$nobs`
  and `BIC(fit)` were already correct via their own paths
  (`param_info$n_obs` and the `nobs` attribute on `logLik()`, respectively);
  this method closes the gap for any caller that consumes `nobs()` directly.

## Joint Wald and nested LRT tests for TMB fits (TICKET-013)

* New `anova.beezdemand_tmb()`: joint Wald-chi-square tests on grouped
  fixed-effect terms for a single fit, and sequential likelihood-ratio
  tests for nested fits (TICKET-013).

## TMB variance components reported on the log10 scale (TICKET-015)

* **Bug fix.** `summary(fit_tmb)$variance_components` now reports the Q0 and
  alpha random-effect SDs on the log10 scale. TMB estimates these SDs on the
  natural-log scale internally (`src/MixedDemand.h` evaluates
  `Q0 = exp(log_q0)`); `summary()` previously reported them raw, off by a
  factor of `log(10) ~= 2.303` from `nlme::VarCorr()` on a structurally
  matched `fit_demand_mixed()` fit using the default `param_space = "log10"`.
  The two backends' random-effect SDs are now directly comparable. The
  residual SD (on the model's likelihood scale) and the random-effect
  correlations (scale-invariant) are unchanged.
* **Breaking change.** The Q0/alpha RE SD rows of
  `summary(fit_tmb)$variance_components$Estimate` change in value by a factor
  of `1 / log(10)`. Analysis code that divided TMB RE SDs by `~2.303` to
  compare them with `nlme::VarCorr()` should drop that manual conversion --
  it is now applied internally.

## Population and subject prediction levels (TICKET-014)

* `predict.beezdemand_tmb()` gains a `level` argument for
  `type = "response"`. `level = "subject"` (default) preserves the previous
  behavior -- it conditions on each subject's random effects, requires the
  model's ID column in `newdata`, and returns a `.fitted` column. `level =
  "population"` evaluates at the fixed-effect coefficients with all random
  effects set to zero (the population-mean curve), does not require the ID
  column, and returns a `predict.fixed` column. Passing
  `c("population", "subject")` returns both `predict.fixed` and
  `predict.id` columns in one call, matching the
  `nlme::predict.lme(level = 0:1)` schema so `nlme`-based plotting code runs
  unchanged.
* Unlike `predict.beezdemand_nlme()`, which accepts the `nlme`-style numeric
  `level` (`0` / `1`), the TMB method takes the character form only; a
  numeric `level` is rejected with a `match.arg()`-style error.
* `fitted()` and `residuals()` for `beezdemand_tmb` fits honor the same
  `level` argument: `level = "population"` now returns population-mean
  fitted values and the corresponding residuals. Previously this argument
  was an unimplemented stub that returned subject-level values.

## VarCorr() accessor for TMB fits (TICKET-021)

* `VarCorr()` now has a `beezdemand_tmb` method. `VarCorr(fit_tmb)` returns
  the random-effect variance components in the matrix layout produced by
  `nlme::VarCorr()` -- a `"VarCorr.lme"`-class object with `Variance`,
  `StdDev`, and (for `pdSymm` fits) `Corr` columns plus a final `Residual`
  row -- so users coming from `nlme` or `lme4` can introspect a TMB fit with
  a familiar accessor. The values match
  `summary(fit_tmb)$variance_components`: the Q0/alpha random-effect SDs on
  the log10 scale and the residual SD on the model's likelihood scale.

## Diagnostics random-effect scale alignment (TICKET-002)

* `check_demand_model()` on a `beezdemand_tmb` fit now reports
  `$random_effects$variances` on the log10 scale, consistent with
  `summary(fit_tmb)$variance_components` (the TICKET-015 convention).
  Previously these were raw natural-log-scale SDs, a factor of `log(10)`
  larger. The raw internal SDs -- still used for the near-zero degeneracy
  check -- are now exposed separately as `$random_effects$sd_internal_log`.

## Initial 0.3.0 features (TMB mixed-effects modeling tier)

These sections capture the original 0.3.0 release scope (TMB mixed-effects
modeling tier, hurdle improvements, bug fixes, quality / tooling). The
TICKET-011 phases above were added under the same 0.3.0 development cycle.

### TMB mixed-effects modeling tier

* `fit_demand_tmb()` is the new modern mixed-effects path for behavioral
  economic demand models, alongside the existing NLME tier
  (`fit_demand_mixed()`). Backed by Template Model Builder (TMB) with
  automatic differentiation and Laplace approximation. Supports four
  equations (`exponential`, `exponentiated`, `simplified`, `zben`),
  1-RE (Q0 only) or 2-RE (correlated Q0 + alpha) random-effect structures,
  estimated or fixed `k`, factor and continuous covariates with asymmetric
  `collapse_levels`, and three data-adaptive starting-value strategies via
  `multi_start = TRUE` (default).

* `fit_demand_tmb()` exposes full optimizer controls via `tmb_control`:
  - `optimizer`: `"nlminb"` (default) or `"L-BFGS-B"` for recovering from
    convergence failures (code 1 or 8).
  - `rel_tol`: Convergence tolerance for nlminb (default 1e-10).
  - `lower` / `upper`: Named numeric vectors for parameter bounds on the
    optimizer scale; applied to all occurrences of repeated parameter names
    (e.g., `beta_q0`).
  - `warm_start`: Restart optimization from a previous `fit$opt$par` vector.
    Automatically disables `multi_start`.
  - `trace`: Optimizer trace output (default 0).

* The `beezdemand_tmb` class ships a comprehensive S3 method suite:
  `print`, `summary`, `coef`, `fixef`, `ranef`, `logLik`, `AIC`, `BIC`,
  `nobs`, `predict` (`response`, `parameters`, `demand`), `confint`,
  `residuals`, `fitted`, `vcov`, broom (`tidy`, `glance`, `augment`),
  `get_subject_pars`, `calc_group_metrics`, `get_demand_param_emms`,
  `get_demand_comparisons`, and visualization (`plot`, `plot_qq`,
  `plot_loss_surface`, `plot_loss_profile`, `plot_re_diagnostics`,
  `plot_alpha_distribution`, `plot_elasticity`, `plot_expenditure`,
  `plot_demand_overlay`).

* `vignettes/tmb-mixed-effects.Rmd` walks through the full TMB workflow
  (equations, random-effect structures, diagnostics) with cache-aware
  fast/full mode.

* `vignettes/convergence-guide.Rmd` documents convergence troubleshooting
  for both the TMB and NLME tiers.

### Hurdle: marginal P(zero) and unconditional Pmax/Omax

* `predict.beezdemand_hurdle(type = "probability", marginal = TRUE)`
  computes population-averaged P(zero) by integrating over the random
  intercept distribution. Methods: `"kde"` (default), `"normal"`,
  `"empirical"`.

* `plot.beezdemand_hurdle(type = "probability")` now shows the marginal
  (population-averaged) P(zero) curve by default. `marginal = FALSE`
  reverts to the old conditional (RE = 0) behavior.

* `calc_group_metrics.beezdemand_hurdle()` now returns both conditional
  and unconditional Pmax / Omax (TICKET-003). Conditional (`$Pmax`,
  `$Omax`) keeps its long-standing Part-II-only meaning. New
  `$Pmax_unconditional` / `$Omax_unconditional` come from optimizing
  `p * (1 - P0(p)) * Q(p)` over the observed price domain. Subject-level
  `subject_pars` likewise gains `Pmax_unconditional` /
  `Omax_unconditional` columns. `summary()`'s `derived_metrics` reports
  both sets, with `component = "unconditional"` on the new rows.

* `plot_expenditure.beezdemand_hurdle()` gains
  `demand_type = c("unconditional", "conditional")` (default
  unconditional). The displayed expenditure curve and the Pmax/Omax
  reference lines now come from the same metric set, so they always
  align — fixing the visible misalignment where the curve used
  `(1 - P0) * Q` but the reference lines used the Part-II-only metrics.

### Other

* New `calculate_amplitude_persistence.beezdemand_tmb()` method
  (TICKET-004) lets users compute amplitude/persistence factors directly
  from `fit_demand_tmb()` results (default persistence components:
  `c("Pmax", "Omax", "alpha")` — TMB `subject_pars` does not include
  `breakpoint`).

* New cross-price S3 methods (TICKET-005): `print`, `augment`, `confint`
  (lm + lmer), and `nobs` for `cp_model_nls`, `cp_model_lm`, and
  `cp_model_lmer`. Augment for lmer additionally includes a `.fixed`
  column (population-level prediction with random effects = 0). All
  methods handle a NULL underlying model gracefully.

* Linearized marginal NLL surface for `plot_loss_surface()` on NLME
  models (commit `2bedd29`).

* Visualization helpers added to TMB and NLME vignettes (commit
  `75a202a`).

* `get_demand_param_emms.beezdemand_nlme()` gains a `param` argument
  (`"both"`, `"Q0"`, `"alpha"`) for API parity with the
  `beezdemand_tmb` method (TICKET-012). Default `"both"` preserves the
  historical return shape; `"Q0"` and `"alpha"` narrow the output to a
  single parameter's columns for easier pivoting and plotting.

## Initial 0.3.0 bug fixes

* `fit_demand_tmb()` now drops rows with `NA` values in any modeling
  column (`id`, price, response, factors, continuous covariates) before
  entering the TMB pipeline, matching the `fit_demand_mixed()` behavior.
  Previously a single `NA` in the response crashed `.tmb_prepare_data()`
  with `"missing value where TRUE/FALSE needed"`, and `NA` in factors or
  covariates could propagate into `model.matrix()` and trigger a TMB
  segfault during `MakeADFun()`.

* `predict.beezdemand_tmb()` now rebuilds the fixed-effect linear
  predictor from `newdata` instead of reusing training-time
  `subject_pars$Q0` / `alpha`. Predictions for any model with factors or
  continuous covariates now correctly reflect the values supplied in
  `newdata`; unknown subjects fall back to the newdata fixed effects
  with random effects = 0 (with a warning). Previously the function
  silently used cached subject parameters for known subjects and the
  reference-level intercepts for unknown ones, producing systematically
  biased `.fitted` values. `augment.beezdemand_tmb()` inherits the fix.
  Predict now also errors clearly when `newdata` is missing a required
  modeling column or contains factor levels not seen in training.

* `get_demand_param_emms.beezdemand_tmb()` and
  `get_demand_comparisons.beezdemand_tmb()` now include continuous
  covariates in the reference grid, matching the dimensionality of the
  fitted `beta` coefficients. Covariates default to their training-data
  mean (matching `emmeans::ref_grid`) and can be overridden via
  `at = list(covname = value)`. Previously, TMB fits that mixed factors
  and continuous covariates produced `non-conformable arguments` in the
  Wald variance calculation or silently used the wrong model basis.

* `get_demand_comparisons()` now restricts pairwise contrasts to observed
  factor combinations. Previously, with unbalanced designs (e.g.,
  different dose levels per drug), the function computed contrasts on the
  full factorial grid, producing phantom comparisons for non-existent
  factor combinations and identical estimates across `contrast_by` groups
  in additive models.

* `summary.beezdemand_nlme()` with `report_space != internal_space` now
  preserves nlme's containment-based degrees of freedom across the
  delta-method parameter transformation, computing p-values via
  `stats::pt()` instead of `stats::pnorm()`. Small-N inference is no
  longer anti-conservative on natural-scale summaries (TICKET-006).

* `summary.beezdemand_hurdle()$coefficients_matrix` is now labelled
  `"z value"` (was `"t value"`) to match the pnorm-based p-value
  computation. TMB-based hurdle models use Laplace approximation, so the
  z-test is the correct asymptotic inference (matches the glmmTMB
  convention) — only the label was wrong (TICKET-006).

* `fit_demand_tmb()` and `fit_demand_hurdle()` now expose `hessian_pd` on
  the fit object and warn at fit time when the Hessian is not positive
  definite (`pdHess = FALSE`). `summary()` adds a corresponding note;
  `tidy()` sets a `hessian_warning` attribute on its output. Previously
  these models silently reported unreliable standard errors / p-values /
  Wald intervals when the Hessian was singular (TICKET-008). Backwards
  compatible: legacy fit objects without the field still work.

* `check_demand_model()` no longer crashes when called on
  `beezdemand_tmb` objects (TICKET-002 — the underlying name + residual
  fields were already corrected in commit `719c0ed`; this release adds
  the regression coverage that pins the fix).

* `fit_demand_mixed()` correctly handles `NA` values in the input data
  and strips spurious names from start values (commit `b39c24a`).

* Visualization improvements — smoothing, value clamping, and APA styling
  for diagnostic plots (commit `bf125f6`).

* Comprehensive package audit fixes — boundary detection, data
  validation, heuristic improvements (commit `60b13a2`).

## Initial 0.3.0 quality / tooling

* Bare `stop()` / `warning()` / `message()` calls in non-legacy R files
  replaced with their cli equivalents (`cli::cli_abort`, `cli::cli_warn`,
  `cli::cli_inform`) and the package's structured error helpers
  (`validation_error`, `fitting_error`, `missing_package_error` —
  TICKET-009). Errors now carry stable class tags (e.g.,
  `"beezdemand_validation_error"`) for programmatic catch-handling. Bare
  calls in legacy `R/analyze.R` are intentionally left in place; that
  file is slated for removal in v1.0. The error helpers themselves were
  refactored to use `cli::cli_abort` internally so callers can pass cli
  inline markup (`{.arg}`, `{.field}`, `{.val}`, `{.fn}`).

* Test coverage added for four previously-untested exported functions
  (TICKET-007): `cp_posthoc_slopes()`, `cp_posthoc_intercepts()`,
  `extract_coefficients()`, and `get_demand_param_trends()`. 23 new
  tests across three new test files using the `etm` and `ko` example
  datasets.

* New regression tests for `check_demand_model.beezdemand_tmb()` (1-RE
  and 2-RE), `calculate_amplitude_persistence.beezdemand_tmb()`,
  `summary.beezdemand_nlme()` p-value preservation,
  `summary.beezdemand_hurdle()` coefficient-matrix labelling,
  `hessian_pd` propagation on TMB and hurdle fits, the cross-price S3
  methods, hurdle unconditional Pmax/Omax, and the structured error
  class hierarchy.

# beezdemand 0.2.0

## Deprecations

* `FitCurves()` is now superseded by `fit_demand_fixed()`. `FitCurves()` will
  continue to work but emits a soft deprecation warning. The new function
  provides a modern S3 interface with `summary()`, `tidy()`, `glance()`,
  `predict()`, and `plot()` methods. See `vignette("migration-guide")` for
  migration instructions.

* `FitMeanCurves()` is now superseded by `fit_demand_fixed(agg = "Mean")` or
  `fit_demand_fixed(agg = "Pooled")`.

## New Features

### Koffarnus Equation for Mixed-Effects Models

* `fit_demand_mixed()` now supports the Koffarnus et al. (2015) exponentiated
  equation via `equation_form = "koff"`. This enables fitting demand curves
  using the same equation form available in `FitCurves()` within the modern
  hierarchical mixed-effects framework. The `k` parameter can be user-specified
  or auto-calculated from data range.

### broom Integration

* New `augment()` methods for all model classes provide fitted values and
  residuals in a tidy tibble:
  - `augment.beezdemand_fixed()`: Returns `.fitted`, `.resid`
  - `augment.beezdemand_hurdle()`: Returns `.fitted`, `.fitted_link`,
    `.fitted_prob`, `.resid`, `.resid_response`
  - `augment.beezdemand_nlme()`: Returns `.fitted`, `.resid`, `.fixed`

### Model Comparison Framework

* New `compare_models()` function for unified model comparison across all
  beezdemand model classes. Reports AIC, BIC, delta_AIC, delta_BIC, and
  performs likelihood ratio tests when models are from the same backend
  and nested.

* New `anova()` S3 methods for comparing nested models:
  - `anova.beezdemand_hurdle()`: LRT for nested hurdle models
  - `anova.beezdemand_nlme()`: Delegates to nlme::anova.lme()

### Model Diagnostics Suite

* New `check_demand_model()` generic with methods for all model classes. Performs
  comprehensive diagnostics including convergence checks, boundary condition
  detection, random effect variance assessment, and residual outlier detection.
  Returns structured diagnostics object with issues and recommendations. (Named
  `check_demand_model()` to avoid conflict with `performance::check_model()`.)

* New `plot_residuals()` function creates diagnostic plots: residuals vs fitted,
  histogram of residuals, and Q-Q plots. Works with all model classes via
  the `augment()` infrastructure.

* New `plot_qq()` function creates Q-Q plots for random effects to assess
  normality assumptions in hurdle and NLME models.

### Normalized Alpha (Alpha Star)

* All model classes now compute `alpha_star` (normalized alpha, Strategy B;
  Rzeszutek et al., 2025), which makes the elasticity parameter comparable
  across different values of `k`. Available in `FitCurves()` output (columns
  `alpha_star` and `alpha_star_se`), `tidy()` on `beezdemand_fixed` objects,
  and `tidy()` on `beezdemand_hurdle` objects. Standard errors are obtained
  via the delta method. See `?param-registry` for details.

### Modern Wrappers for Legacy Functions

* New `get_empirical_measures()` as a modern replacement for `GetEmpirical()`.
  Returns a `beezdemand_empirical` S3 object; access the results via
  `$measures`.

* New `get_descriptive_summary()` as a modern replacement for
  `GetDescriptives()`. Returns a `beezdemand_descriptive` S3 object; access
  the results via `$statistics`.

* New `get_k()` as a modern replacement for `GetK()`. Returns a single numeric
  k value with optional verbose output.

### Other New Features

* New `confint()` methods for extracting confidence intervals from all model
  classes: `beezdemand_fixed`, `beezdemand_hurdle`, `beezdemand_nlme`, and
  `cp_model_nls`.

* New migration guide vignette (`vignette("migration-guide")`) documenting the
  transition from `FitCurves()` to `fit_demand_fixed()`.

## Breaking Changes

* `summary()` methods for `beezdemand_hurdle` and `beezdemand_nlme` now return
  structured summary objects instead of printing directly. Use
  `print(summary(fit))` for console output. Programmatic access is now
  possible: `s <- summary(fit); s$coefficients`.

* `fit_demand_hurdle()` now fits demand parameters in natural-log space
  (`log_q0`, `log_alpha`, `log_k`) and reports back-transformed values; the
  `param_space` argument has been removed.

* `fit_cp_nls()` now uses log10-parameterized optimizer coefficients
  (`log10_qalone`, `I`, `log10_beta`) across equation forms; the `"exponential"`
  form fits on the `log10(y)` response scale and filters `y <= 0` with a warning.
  `predict.cp_model_nls()` now always returns `y_pred` on the natural `y` scale;
  for `"exponential"` it additionally returns `y_pred_log10` (and no longer returns
  `y_pred_natural`).

### Additional New Features

* New `fit_demand_fixed()` function provides a modern interface for individual
  demand curve fitting. Returns a structured S3 object with `summary()`,
  `tidy()`, and `glance()` methods. This wrapper offers the same functionality
  as `FitCurves()` but with a standardized API.

* New systematicity wrappers with unified output vocabulary:
  - `check_systematic_demand()` for purchase task data (wraps `CheckUnsystematic()`)
  - `check_systematic_cp()` for cross-price data (wraps `check_unsystematic_cp()`)

  Both return `beezdemand_systematicity` objects with identical column
  schemas (differing only in NA values for domain-specific fields).

* First-class `tidy()` and `glance()` support is now guaranteed across all
  beezdemand model classes. All methods return tibbles with standardized
  columns including `model_class` and `backend`.

* All summary objects now inherit from `beezdemand_summary` base class,
  enabling shared fallback behavior and consistent field availability.

## API Standardization

This release introduces **Stability Contracts** for all model classes:

* **summary() objects** now return structured S3 objects with class
  `c("summary.<class>", "beezdemand_summary")`. Required fields include:
  `call`, `model_class`, `backend`, `nobs`, `n_subjects`, `converged`,
  `logLik`, `AIC`, `BIC`, `coefficients` (tibble), `notes`.

* **tidy() methods** return tibbles with columns: `term`, `estimate`,
  `std.error`, `statistic`, `p.value`. Multi-part models include a
  `component` column (e.g., "fixed", "variance", "derived").

* **glance() methods** return 1-row tibbles with columns: `model_class`,
  `backend`, `nobs`, `n_subjects`, `converged`, `logLik`, `AIC`, `BIC`.

## API Changes

* `fit_cp_nls()` and `fit_cp_linear()` now accept `x_var`/`y_var` to map
  non-standard column names to canonical ones (`"x"`, `"y"`). `fit_cp_linear()`
  additionally accepts `id_var`, `group_var`, and `target_var`. Default behavior
  is unchanged when these arguments are omitted.

* `fit_cp_linear()` gains explicit `filter_target` and `target_level` top-level
  arguments (previously these were handled implicitly via `validate_cp_data()`).
  Existing calls without these arguments are unaffected.

* `fit_cp_nls(start_vals=)` is deprecated in favor of `start_values=`. The old
  argument still works but emits a deprecation warning.

---

# beezdemand 0.1.3

## Deprecations

The following deprecations will take effect in version 0.2.0:

* `beezdemand::pull()` is deprecated in favor of `dplyr::pull()`. The beezdemand
  version was a legacy helper that predates the dplyr function.

* The `inverse_fun` argument in `summary.cp_model_nls()`, `plot.cp_model_nls()`,
  and `predict.cp_model_nls()` is deprecated in favor of `inv_fun` for consistency
  with mixed-effects model methods.

## API Improvements

* Standardized argument names across cross-price model methods (`inv_fun` instead
  of `inverse_fun`)

* Cross-price plot methods now have consistent argument ordering across
  `plot.cp_model_nls()`, `plot.cp_model_lm()`, and `plot.cp_model_lmer()`

* Key user-facing functions now return tibbles for better compatibility with

  tidyverse workflows: `predict.cp_model_nls()`, `predict.cp_model_lm()`,
  `tidy.cp_model_nls()`, `glance.cp_model_nls()`

* Added standardized error helpers (`validation_error()`, `fitting_error()`,
  `missing_package_error()`) for consistent error messaging

* `check_unsystematic_cp()` now returns an object of class `cp_unsystematic`
  with proper `summary()` method dispatch (no longer overrides `summary.tbl_df()`)

## New Features

### Two-Part Mixed Effects Hurdle Demand Models

* Added comprehensive hurdle model functionality using TMB (Template Model Builder):

  * `fit_demand_hurdle()`: Fit two-part hurdle models with 2 or 3 random effects

  * Part I models probability of zero consumption (logistic regression with random intercept)

  * Part II models log-consumption given positive response (nonlinear mixed effects)

* S3 methods for `beezdemand_hurdle` objects:

  * `print()`, `summary()`, `coef()`, `logLik()`, `AIC()`, `BIC()`

  * `predict()`: Extract subject parameters or predict demand/probability

  * `plot()`: Visualize demand curves, zero probability, parameter distributions

* Utility functions:

  * `calc_omax_pmax()`: Calculate Pmax and Omax from demand parameters

  * `get_subject_pars()`: Extract subject-specific parameter estimates

  * `compare_hurdle_models()`: Likelihood ratio test for model comparison

  * `get_hurdle_param_summary()`: Summary statistics for individual parameters

* Simulation functions:

  * `simulate_hurdle_data()`: Generate synthetic hurdle model data

  * `run_hurdle_monte_carlo()`: Monte Carlo simulation studies

* New vignette "Hurdle Demand Models" with comprehensive examples

* New dataset `apt_full`: Full alcohol purchase task data with 1,100 subjects and demographic covariates

### Cross-Price Demand Models

* Added comprehensive cross-price demand model functionality:

  * `check_unsystematic_cp()`: Check for unsystematic data in cross-price models

  * `fit_cp_nls()`: Nonlinear model fitting for cross-price demand data

  * `fit_cp_linear()`: Linear model fitting for cross-price demand data with options for fixed effects and mixed-effects models

  * New utility functions for cross-price model objects:

    * `summary()`, `plot()`, `glance()`, and `tidy()` methods

  * `extract_coefficients()`: Extract model coefficients in tidy format

  * `cp_posthoc_slopes()` and `cp_posthoc_intercepts()`: Post-hoc comparisons for model parameters

  * `validate_cp_data()`: Validate and filter cross-price demand data

* Added new vignette "How to Use Cross-Price Demand Model Functions" demonstrating:

  * Required data structure for cross-price analyses

  * Checking for unsystematic data

  * Both two-stage and pooled model fitting approaches

  * Linear and mixed-effects modeling options

  * Model visualization and coefficient extraction

  * Post-hoc comparisons

# beezdemand 0.1.2

* No longer relies on `nlmrt` and instead relies on `nlsr`

* Fixes an issue where CheckUnsystematic may not flag certain cases when data are passed as `tibble`

* Fixes deprecated arguments in `ggplot2`

* Add ability to specify a start value for alpha in `ExtraF()` function

# beezdemand 0.1.1

* Add experimental features for `FitCurves()`. These arguments are `constrainq0`, `startq0`, and `startalpha`. These arguments allow Q0 to be constrained so alpha is the only fitted parameter and allow for user-specified starting values.

# beezdemand 0.1.0

* Package successfully on CRAN!

# beezdemand 0.1.00

* Package should be ready for CRAN and is being submitted

# beezdemand 0.0.95

## New updates

* One major change that might affect previous scripts is that in output summary tables, the column formally named ID is now named id (lowercase)

* Cleaned up a few things here and there. The package is close for submission to CRAN as it passes R CMD check with no errors, warnings, or notes

# beezdemand 0.0.91

## New updates

* `GetSharedK()` updated to work better and faster at finding a reasonable value

* Internal helper functions added to optimize `GetSharedK()`

# beezdemand 0.0.90

## New updates

* `ExtraF()` now compares alpha and Q0

* A number of functions now allows you to specify the column names

# beezdemand 0.0.85

## New updates

* `FitCurves()` correctly pulls alpha and q0 standard errors when k is fitted as a free parameter. Also no longer accepts data transformations. Must be done prior to fitting using `ChangeData()`.

* `FitCurves()` now fits mean/pooled data based on `method` argument.

* `GetSharedK()` no longer accepts data transformations.

# beezdemand 0.0.84

## New updates

* New `ChangeData()` will soon serve as the replacement to
  `ReplaceZeros()` and other arguments specified in `FitCurves()`.

* For the time being, `FitCurves()` will actually output a list
  object. This may cause failures with old scripts. Try modifying
  scripts to take the first element out of the list. This will soon be
  taken care of.

## Tidying

* Email contact has been changed and some minor updates to .rd files.

# beezdemand 0.0.6

## New features

* New `FitMeanCurves()` will fit curve to averaged or pooled
  data. Can also make plots.

* `FitCurves()` can now make plots.

* `GetDescriptives()` can make box and whisker plots.

## Cleanup

* Old code from previous workflow is removed. Now all functions use
  longform data.
