# beezdemand 0.3.0

Feature release covering everything since 0.2.0. Headline changes:

* **TMB mixed-effects modeling tier** (`fit_demand_tmb()`) with automatic
  differentiation, a Laplace approximation, multi-start optimization,
  optional estimation of `k`, factor-expanded / multi-block (`pdBlocked`)
  and continuous within-subject random-slope structures, and the full
  post-hoc surface (EMMs, contrasts, subject-level parameters, parametric
  bootstrap CIs, diagnostics).
* **Monte Carlo power analysis** (`power_demand()`, `find_n_demand()`) for
  within- and between-subject designs.
* **Multi-start rescue is the default in `fit_demand_fixed()`** and the zben
  `Pmax`/`Omax` are computed numerically -- see "Bug fixes that can change
  estimates" for exactly which outputs can differ from 0.2.0 and how to pin
  the old numbers.
* **Inference gates and diagnostic honesty**: TMB/hurdle/NLME inference
  surfaces now refuse or flag results from non-converged or non-PD fits
  instead of reporting them silently; `check_demand_model()` no longer
  reports failed checks as passing.
* **Silent-failure fixes** in the hurdle covariance path, cross-price
  fitters, extractors and plots, plus legacy-fitter batch robustness.
* A hurdle-SND data simulator (`simulate_hurdle_data(part2 = "snd")`), a
  breaking `predict.beezdemand_hurdle()` default (`type = "demand"`), and
  the harmonized `get_demand_comparisons()` / EMM API across backends.

The subsections below give the per-change detail, oldest at the bottom.

## Monte Carlo power analysis

* `power_demand()` estimates statistical power for detecting a condition
  difference in `Q0` or `alpha`, by simulating from the package's
  mixed-effects demand model and refitting each replicate with
  `fit_demand_tmb()`. Supports both a two-condition within-subject design
  (default) and, via `design_type = "between"`, a two-arm between-subject
  design testing the group difference; the between design composes the two
  arms from the same simulator (one condition per arm), uses `df = n - 2`,
  and refits per-subject intercept random effects that match its
  data-generating process exactly. Reports the power estimate with a Wilson
  Monte Carlo confidence interval, p-value and CI-exclusion hit rates, and
  convergence diagnostics; non-usable fits are excluded from the denominator
  and surfaced, never counted as misses.
* `find_n_demand()` searches for the smallest `n_subjects` reaching a target
  power via bisection, adding replicates adaptively where the Monte Carlo
  verdict is ambiguous and re-confirming the selected N before reporting.
  Also takes `design_type`.
* Type I error calibration (both designs), convergence handling, closed-form
  benchmarks against `pwr::pwr.t.test()` (one- and two-sample), monotonicity,
  and seed reproducibility are verified in the test suite; see
  `vignette("power-analysis")` for scope and validity notes.

## Bug fixes that can change estimates

The fixes in this subsection correct wrong numbers rather than add features, so
outputs can differ from 0.2.0 under the stated conditions. Single-subject fits
and paths that were already correct are unchanged. To reproduce the old numbers
exactly, pin the previous release:
`remotes::install_version("beezdemand", "0.2.0")`.

* **Multi-start is now the default fitting protocol in `fit_demand_fixed()`
  (TICKET-047).** Previously each subject was fit from a single
  production-heuristic starting value; a subject whose start led to a
  failed or at-a-bound fit was simply reported as non-converged. Now, any
  subject whose production-heuristic fit is not strict-converged
  (`converged_strict`: optimizer convergence AND finite coefficients/
  objective AND not sitting on a user-supplied bound) is automatically
  re-fit from several additional sampled starting values (8 for
  2-parameter equations `hs`/`koff`/`simplified` with a fixed `k`, 32 when
  `k = "fit"`), and the best strict-converged result is kept. Condition
  under which output differs from 0.2.0: **only** subjects whose
  production-heuristic fit previously failed to converge or landed on a
  bound — some previously non-converged/`NA` rows may now report a
  converged fit. Subjects whose production fit was already
  strict-converged are **never** refit and are byte-identical (this is
  guaranteed by construction, not just tested). A sampled starting value is
  only ever accepted as a rescue if it is BOTH strict-converged AND
  domain-valid (natural-scale `Q0 > 0` and `Alpha > 0`); a sampled start
  that only "succeeds" by landing in a domain-invalid region (e.g.
  negative alpha) is never preferred over leaving the subject
  non-converged. `fit_demand_fixed(..., multistart = FALSE)` or `S = 1`
  restores the exact legacy single-start behavior. `FitCurves()` itself is
  completely unchanged.

* **Essential value (EV) in `boot_demand()` and `get_demand_param_emms()`.**
  The TMB and NLME tiers computed `EV = 1 / (100 * alpha)` for every equation
  form, silently dropping the `k^1.5` term for k-bearing forms and applying a
  spurious factor of 100 to the k-free SND form. Both now mirror `analyze.R`
  exactly: `EV = 1 / (100 * alpha * k^1.5)` for k-bearing forms (exponential /
  exponentiated, with `k` fixed or fitted) and `EV = 1 / alpha` for the SND
  ("simplified" / `zben`) form. Condition under which output differs: every
  `EV` point estimate, CI, and bootstrap draw from these two functions; `Pmax`,
  `Omax`, `Qmax`, and `elasticity_at_pmax` are unaffected. `FitCurves()` /
  `fit_demand_fixed()` EV values were already correct and do not change. For
  NLME fits built with a `custom_model_formula` the equation form is unknown,
  so `get_demand_param_emms()` now reports `EV`/`LCL_EV`/`UCL_EV` as `NA` with
  a warning instead of applying a guessed formula.
* **`residuals(fit, scale = "natural")` was scale-mixed for `equation =
  "zben"` in the TMB tier.** `zben`'s `y_var` is the caller-supplied
  LL4-transformed response (`ll4(y, lambda = 4)`); the natural-scale residual
  subtracted the natural-scale fitted value from that still-LL4-transformed
  `y_var` instead of from `ll4_inv(y_var)`, so the "natural" residual was
  really `LL4(y) - Q_hat_natural`. Condition under which output differs:
  `equation = "zben"` fits only, and only `residuals(fit, scale =
  "natural")`; `scale = "model"` (the default), `fitted()`, `predict()`, and
  `augment()` were already correct. All other equation forms are unchanged
  (#18).
* **`equation = "zben"` per-subject and group `Pmax`/`Omax` used the SND
  closed form in the TMB tier.** `get_subject_pars()` (both the fit-time and
  the `expanded = TRUE` computation), `calc_group_metrics()`, and
  `boot_demand()` passed `zben` fits through `model_type = "snd"`, i.e.
  `Pmax = 1 / (alpha * Q0)`. That closed form assumes SND's `(Q0, alpha)`
  coupling, which does not hold for `zben`'s LL4-scale exponential decay;
  stored `Pmax` was up to ~3.4x too small at the median and rank-inverted
  relative to the true expenditure-maximizing price. All four call sites now
  numerically optimize expenditure on the back-transformed (`ll4_inv()`-ed)
  natural-scale curve via `beezdemand_calc_pmax_omax(model_type = "zben")`.
  That numerical search also adaptively expands beyond the observed price
  domain (up to 6 decades) when the true expenditure-maximizing price lies
  outside it, rather than silently returning the domain edge as `Pmax` (the
  fix as first landed still had this domain-truncation defect; two curves
  observed through different price ranges could report different `Pmax` for
  an otherwise-identical fitted curve). `get_subject_pars()` and
  `calc_group_metrics()` gain a `pmax_at_bound` field/column, `TRUE` only
  when the search's expansion cap was hit without finding the true
  (interior) maximum, in which case `Pmax`/`Omax` are a lower-bound
  estimate rather than a converged value; `boot_demand()` does not gain a
  `pmax_at_bound` column (its output schema is unchanged) but emits a
  `cli::cli_warn()` naming the count of affected bootstrap draws when any
  occur among a requested `Pmax`/`Omax`/`Qmax`/`elasticity_at_pmax`
  statistic. Condition under which output differs: `equation = "zben"` fits
  only; `Pmax`, `Omax`, and (where reported) `Qmax` and
  `elasticity_at_pmax` from `get_subject_pars()`, `calc_group_metrics()`,
  and `boot_demand()`. All other equation forms are unchanged (#19).

* **`FitCurves()` / `fit_demand_fixed()` batch fitting (legacy fixed-effect
  engine).** Two interacting defects in the per-subject loop: (1) the default
  Q0/alpha start values were computed once from the first subject's data and
  then reused (sticky) for every subsequent subject instead of being
  recomputed per subject, silently making batch estimates order- and
  scale-dependent; (2) when the entire nls fallback chain (`wrapnlsr` →
  `nlxb` → `nls2` brute-force) failed for a subject, an unguarded
  `fit$m$Rmat()` dereference on the resulting try-error crashed the whole
  batch call instead of recording that subject as non-converged. Condition
  under which output differs from 0.2.0: any batch call (2+ subjects) where
  a subject after the first has a different consumption scale, or where any
  subject's nls fallback chain fully fails. A batch that previously crashed
  now returns one row per subject with the failing subject(s) flagged
  non-converged; a batch that previously "succeeded" but silently used a
  wrong start value for subjects 2..N may now report different (correct,
  order-invariant) estimates for those subjects. Single-subject calls are
  unchanged (they never had a sticky prior subject to inherit from).

* **`FitCurves()` / `fit_demand_fixed()` with `k = "fit"` and
  `param_space = "log10"`.** The k start value's `log10()` transform was
  applied inside the per-subject loop instead of once before it, so the
  transform compounded on every iteration: subject 1 started from
  `log10(K)` (correct), subject 2 from `log10(log10(K))` (silently wrong for
  typical `K < 10`), and subject 3+ from `log10(<negative>)` = `NaN`,
  producing non-converged rows for later subjects. Condition under which
  output differs from 0.2.0: batch calls with `k = "fit"`,
  `param_space = "log10"`, and 2 or more subjects — subject 1's estimates
  are unchanged, subjects 2+ now converge (previously mis-started or
  NaN-started). Natural-space and fixed/individual/shared-k paths are
  unaffected.

* **`FitCurves()` / `fit_demand_fixed()` reported unverified fallback
  endpoints as estimates, with no way to tell a genuine fit from a stalled
  one.** When `wrapnlsr` failed and the chain fell back to `nlxb`, the old
  code re-fit that endpoint with `nls2::nls2(..., algorithm = "brute-force")`
  and a single-point start — this is a snapshot, not a fit; it always
  "succeeds" and reports whatever point `nlxb` stalled at, including points
  with a singular Jacobian, as `Notes = "wrapnls failed to converge,
  reverted to nlxb"` (indistinguishable from a genuine rescue). Separately,
  a numerically converged fit could still land on a physiologically
  impossible point (`Q0 <= 0` or `Alpha <= 0`, e.g. for flat or otherwise
  degenerate data) and be reported as `Notes = "converged"` with no flag.
  The fallback endpoint is now verified with a genuine iterative refit
  (`stats::nls(algorithm = "port")`) that must itself report
  `convInfo$isConv`; an endpoint that fails this verification is recorded as
  a non-converged row (`Notes = "endpoint unverified: fallback refit did
  not converge"`) instead of being reported as an estimate; a verified
  rescue is recorded as `Notes = "wrapnls failed to converge; nlxb endpoint
  verified by port refit"`. Results now carry `converged` (the optimizer's
  own `isConv` verdict) and `converged_strict` (`isConv` AND finite
  coefficients/objective AND not at a user-supplied bound) columns. A
  "converged" fit with non-positive Q0 and/or Alpha now also raises a
  `warning()` naming the subject and which parameter is non-positive —
  domain validity is signaled **only** by that warning: `Notes` is never
  modified and `converged_strict` is never demoted for a domain-invalid
  estimate (it is only reachable in `param_space = "natural"` — the log10
  parameterization's `10^x` back-transform is always positive), so a single
  subject that converges on the first `wrapnlsr` attempt keeps byte-identical
  `Notes`/`converged`/`converged_strict` regardless of domain validity.
  `fit_demand_fixed()$results$converged` now derives from
  `converged_strict` instead of grepping `Notes` for failure keywords —
  including for domain-invalid-but-numerically-converged fits, which are
  therefore reported as `converged = TRUE` (flagged only by the warning,
  not excluded from downstream success counts). Default bounds are
  unchanged (still `c(-Inf, -Inf)`/`c(Inf, Inf)` unless `lobound`/`hibound`
  are supplied — this release does not add default non-negativity bounds).
  Condition under which output differs from 0.2.0: any subject whose
  `wrapnlsr` fit fails and falls back to `nlxb` (now either genuinely
  verified or reported as non-converged, not a raw snapshot), and any
  subject reported "converged" with a non-positive Q0 or Alpha (now also
  raises a warning; `Notes`, `converged`, and `converged_strict` are
  otherwise unaffected). Subjects that converge cleanly on the first
  `wrapnlsr` attempt are unchanged.
  With the brute-force refit gone, `nls2` is no longer used anywhere in the
  package and has been dropped from `Imports`.

* **`GetValsForSim()` (used by `SimulateDemand()`'s Koffarnus et al., 2015
  simulation workflow) misaligned or dropped per-price residuals.** Residual
  columns are keyed to the global price set (`unique(dat$x)` order);
  `resid(fit)` is returned in the fitted subject's own row order. The old
  code assigned residuals to price columns by POSITION
  (`dfres[i, 4:NCOL(dfres)] <- resid(fit)`), which either errored ("replacement
  has N items, need M") when a subject was missing a price row, or — when a
  subject had a full but differently-ordered price grid — silently placed
  residuals under the wrong price column with no error. Since `sdindex`
  (per-price residual SD, which directly controls simulated variance) is
  computed from these columns, a subject whose row order didn't match
  `unique(dat$x)` order silently corrupted `sdindex` without any warning.
  Residuals are now matched to price columns by price VALUE
  (`adf$x`, tolerating a missing price as `NA`, which `sdindex`'s existing
  `na.rm = TRUE` already handles) instead of by position. A subject whose
  fit fails now also raises a `warning()` naming the subject and cause
  instead of silently contributing an all-NA row. Condition under which
  output differs from 0.2.0: any call where a subject's row order (within
  that subject) doesn't match `unique(dat$x)` order, or a subject is missing
  one or more price rows (previously miscomputed `sdindex` or crashed
  outright; now computed correctly, with `NA` for missing price cells).
  Subjects with a complete price grid in canonical order are unchanged.
  Separately, `SimulateDemand()` now works in a fresh R session that has
  never touched the RNG (`RunOneSim()` previously read `.Random.seed`
  unconditionally, which does not exist until the RNG has been used once).

* **`run_hurdle_monte_carlo()` summary statistics can change.** See
  "Silent-failure fixes" below (TICKET-062): converged replicates with a
  non-positive-definite Hessian are no longer counted as valid Monte Carlo
  evidence in `$summary`'s bias/coverage calculations.

## Inference gates and diagnostic honesty

The fixes in this subsection change *status/diagnostic* output (warnings,
issue lists) rather than point estimates -- fits that were correct before
still return the same numbers. Two exceptions, both scoped to NLME
`param_space = "natural"` fits: the `get_demand_param_emms()` bullet below
fixes a wrong-by-orders-of-magnitude back-transformation, and the
`get_demand_comparisons()` bullet (TICKET-075) changes what a natural-space
contrast reports (a difference, not a `10^`-exponentiated ratio); `param_space
= "log10"` fits (the default) are unaffected by both.

* **TMB and hurdle inference surfaces now honor `hessian_pd`.** When
  `TMB::sdreport()` reports a non-positive-definite Hessian
  (`fit$hessian_pd == FALSE`), `sdr$cov.fixed` is a pseudo-inverse of an
  indefinite matrix -- standard errors, confidence intervals, p-values, and
  parametric draws computed from it are unreliable even though the point
  estimates are unaffected. `vcov()`, `confint()`, `anova()` (single-fit
  Wald test), `get_demand_param_emms()`, `get_demand_comparisons()`, and
  `boot_demand()` now each emit one classed warning
  (`beezdemand_hessian_not_pd_warning` / `beezdemand_warning`) the first time
  they consume such a covariance, for both the `beezdemand_tmb` and
  `beezdemand_hurdle` classes. Previously only `summary()`'s print method and
  `check_demand_model()` surfaced this; a user calling `confint()` or
  `boot_demand()` directly received unreliable intervals with no indication
  anything was wrong. Values are unchanged; healthy (PD-Hessian) fits emit no
  new conditions.
* **NLME inference surfaces now honor `.check_nlme_convergence()`.**
  `summary()`, `glance()`, and `check_demand_model()` already gated on
  whether an NLME fit's final apVar inverted cleanly; `get_demand_param_emms()`,
  `get_demand_comparisons()`, `calc_group_metrics()`, `confint()`,
  `get_subject_pars()`, `tidy()`, `get_individual_coefficients()`, and
  `anova()` (per compared model) now each emit one classed warning
  (`beezdemand_nlme_convergence_warning` / `beezdemand_warning`) on a
  non-converged fit instead of computing inference silently.
  `calc_group_metrics()` also replaces a blanket
  `suppressWarnings(suppressMessages(...))` around its internal Q0/alpha
  EMM calls with targeted muffling of two specifically-matched benign
  conditions, so a real warning raised inside those calls (e.g. the
  estimate-column-guess fallback, which flags a possibly-wrong Pmax/Omax)
  now reaches the caller instead of being silently dropped.
  `get_demand_param_trends()` now warns once, naming every dropped
  `(parameter, covariate)` combination and its cause, instead of silently
  shrinking the returned table. Values are unchanged; converged fits emit
  no new conditions. See also the `get_demand_param_emms()` /
  `param_space = "natural"` bullet below, which fixes a related but
  distinct back-transformation bug on the same NLME surface.
* **`get_demand_param_emms()` back-transformed with `10^` unconditionally,
  giving wrong `*_natural` columns (and `EV`) for NLME fits made with
  `param_space = "natural"`.** `fit_demand_mixed(..., param_space =
  "natural")` fits Q0/alpha directly on the natural scale (supported for
  `equation_form = "simplified"`/`"exponentiated"`), so the raw emmeans
  summary is already natural-scale; exponentiating it again with `10^`
  inflated `Q0_natural`/`alpha_natural` by orders of magnitude and
  propagated into `EV`, with no warning. `get_demand_param_emms()` now
  resolves `param_space` the same way every other NLME surface does and,
  for a natural-space fit, uses the emmeans summary directly for the
  `*_natural` columns and fills `*_param_log10` with `log10()` of those
  values (keeping the same column set across both spaces). Because a
  natural-space fit is an unconstrained parameterization, a Wald CI bound
  (or, rarely, the point estimate) can be non-positive; `*_param_log10` is
  `NA` (not `NaN`, and without a raw "NaNs produced" warning) wherever the
  corresponding `*_natural` value is `<= 0` -- `*_natural` itself is never
  affected. Condition under which output differs from 0.2.0: `beezdemand_nlme`
  fits made with `param_space = "natural"`, only in `get_demand_param_emms()`
  (and anything built on its EV branch). `param_space = "log10"` fits (the
  default) and the TMB tier (already space-aware) are unaffected.
* **`get_demand_comparisons()` (NLME) had the same `param_space = "natural"`
  gap as the bullet above, in `$contrasts_ratio`.** (TICKET-075.)
  `emmeans::contrast()`'s `estimate`/CI in `$contrasts_log10` are on the
  fit's internal scale (log10 for `param_space = "log10"`, natural for
  `param_space = "natural"`); `$contrasts_ratio` always computed
  `ratio_estimate = 10^estimate` to turn a log10-scale difference into a
  multiplicative fold-change -- meaningless for an already-natural-scale
  difference. For `param_space = "natural"` fits, `$contrasts_ratio` now
  reports the difference again (same column names/shape:
  `ratio_estimate`/`LCL_ratio`/`UCL_ratio`) instead of exponentiating it a
  second time; the returned object's new `contrasts_ratio_scale` attribute
  (`"ratio"` or `"difference"`) says which content a given call got.
  `$contrasts_log10` itself was already correct (unaffected); only
  `$contrasts_ratio`'s *content* for natural-space fits changes, from a
  previously-meaningless number to a documented, correct one.
  `param_space = "log10"` fits (the default) are unaffected.
* **`check_demand_model()` no longer reports a failed internal check as a
  passing one.** The fixed and hurdle residual sub-checks, and the NLME
  random-effects sub-check, converted an internal `augment()` / `VarCorr()`
  error (or a missing/all-NA `.resid` column) into the same clean-looking
  "no outliers found" / "nothing near zero" result the check returns when it
  actually ran and found nothing -- so a report that never examined
  residuals or random-effect variances printed "No issues detected"
  indistinguishably from one that genuinely checked and passed. Each
  sub-check now sets an explicit `computation_failed` flag, raises one
  classed warning naming the cause, and `check_demand_model()` adds a
  "...could not be computed" issue instead of silently passing. Mirrors the
  pattern the TMB tier's residual check already used. Healthy fits are
  byte-identical; only fits where one of these internal checks errors are
  affected.
* **`fit_demand_tmb()` failure paths now name their causes.** Three gaps on
  the failure/error-reporting side of the TMB fitter:
  - The multi-start terminal abort ("All starting value sets failed.")
    previously discarded every per-start cause (hard errors were `message()`d
    only at `verbose >= 2`; optimizer-sentinel causes in `opt$message` were
    never re-read once a start was rejected). It now appends a `Causes:`
    section naming at least one underlying cause per failed start,
    regardless of verbosity.
  - Total `TMB::sdreport()` failure now warns regardless of `verbose`
    (previously gated on `verbose >= 1`, so a `verbose = 0` fit failed
    completely silently); the warning includes the fallback attempt's
    message when it differs from the first; the same classed warning now
    also fires on ADREPORT/variance-component extraction failure (a
    previously fully silent path); and `hessian_pd = NA` (meaning
    "unknowable because sdreport failed") is now explained via `tidy()`'s
    `hessian_warning` attribute, not just `summary()`'s print note.
  - Data whose rows are entirely dropped by `equation = "exponential"`'s
    zero-consumption filter (e.g. all-zero `y`) now aborts immediately with
    an informative message naming the equation and the dropped-row count,
    instead of proceeding to "fit" a 0-observation model, reporting a
    spurious "Converged (NLL = 0.00)", and then crashing during SE
    extraction with a cryptic `no 'dimnames' attribute for array` (the only
    output a `verbose = 0` caller saw). Healthy fits are unaffected; no
    numeric output changes.
* **`print()`/`summary()` for `beezdemand_hurdle` fits now surface a
  false-converged 3-random-effect fit prominently.** The 3RE spec
  (`random_effects = c("zeros", "q0", "alpha")`) can converge according to
  `nlminb()`'s reported code while the Hessian is not positive definite on
  real purchase task data (weak identification of the alpha random effect,
  not a broken spec); `fit$opt$message`/`fit$opt$convergence` and
  `fit$hessian_pd` already existed but were easy to miss. Both print methods
  now show a warning block (quoting the optimizer message) whenever
  `converged` is `FALSE` or `hessian_pd` is `FALSE`, naming the recommended
  stability check: refit with `random_effects = c("zeros", "q0")` and
  compare empirical-Bayes subject parameters.
  `summary.beezdemand_hurdle()`'s `notes` field (previously computed but
  never printed by `print.summary.beezdemand_hurdle()`) now reaches the
  console. Converged, PD-Hessian fits print byte-identically to before. No
  change to `converged`/`hessian_pd` semantics, EB parameter extraction, or
  the TMB templates/likelihoods.

* `confint(fit, method = "simulate")` and `boot_demand()` on a TMB fit whose
  sdreport covariance contains non-finite values (non-PD Hessian /
  non-converged fit, platform-dependent) now stop with a classed,
  informative error (`beezdemand_nonfinite_vcov_error`) pointing at
  `check_demand_model()` and `fit$hessian_pd`, instead of an opaque
  `eigen()` "infinite or missing values in 'x'" failure.

## Silent-failure fixes (hurdle, cross-price, extractors, plots)

* **Hurdle random-effects covariance `chol()` failure silently substituted an
  uncorrelated diagonal Sigma at five sites** (the RE-transform helpers, the
  live `fit_demand_hurdle()` inline path for 2- and 3-RE models, and
  `.compute_marginal_demand()`'s Monte Carlo draws). When the assembled
  covariance was not positive definite (near-boundary rhos, overflow, or
  `tanh(raw)` rounding to exactly +/-1), subject-level effects and marginal
  demand curves were silently computed from zeroed correlations while the
  reported `correlations`/summary rho estimates continued to show the fitted
  values. All five sites now share `.hurdle_chol_or_fallback()`, which emits
  one classed warning (`beezdemand_hurdle_chol_fallback_warning`) when the
  fallback fires. Condition under which output differs: only calls where the
  assembled Sigma is not positive definite (rare given the partial-correlation
  parameterization); healthy (PD) fits are bit-identical and silent
  (TICKET-061).

* **`run_hurdle_monte_carlo()` discarded per-replicate diagnostics and
  counted non-PD-Hessian fits as valid Monte Carlo evidence.** Failed and
  non-converged replicates collapsed to `NULL` with no record of why; the
  return value carried no per-replicate status at all. Separately, a
  replicate that converged with `hessian_pd = FALSE` passed the same filter
  as a clean fit and contributed its (unreliable) estimate and SE to the
  bias/coverage summary. The return value now includes `$diagnostics` (one
  row per replicate: `sim_id`, `status` -- `"error"`, `"nonconverged"`,
  `"converged_non_pd"`, `"converged_hessian_unavailable"`, or `"clean"` --
  `converged`, `hessian_pd`, `opt_convergence`, `opt_message`) and
  `$n_hessian_not_pd`/`$n_hessian_unavailable`; `$estimates` gains a
  `hessian_pd` column. `hessian_pd = NA` (i.e. `sdreport()` itself failed) is
  kept distinct from an explicit `hessian_pd = FALSE`, since they are
  different conditions, though both are excluded from `$summary` the same
  way. A classed warning (`beezdemand_hurdle_mc_hessian_excluded_warning`)
  fires naming both excluded counts (e.g. "1 non-PD, 1 Hessian unavailable")
  when either happens. **This changes `$summary` output** for any prior run
  that had converged-but-non-PD-Hessian or Hessian-unavailable replicates
  (TICKET-062).

* **`fit_cp_nls()` discarded all backend convergence diagnostics, and
  `summary.cp_model_nls()`/`confint.cp_model_nls()` reported SEs, p-values,
  and CIs with no convergence gate.** A maxiter-capped or otherwise
  non-converged `nlsLM`/`nls.multstart` fit produced a clean-looking
  coefficient table with zero warnings. `fit_cp_nls()` now returns a
  `convergence` field (`isConv`, `finIter`, `stopCode`, `stopMessage` for the
  winning backend, read from `model$convInfo`; populated for `nls`/`nlsLM`
  fits and for `nlsr::wrapnlsr()` fits too when it returns a plain
  `nls`-class object, its usual successful case; `isConv = NA` only when the
  winning fit carries no `convInfo` at all). `summary()` and `confint()` now
  emit a classed warning
  (`beezdemand_cp_nls_nonconverged_warning`) when `isConv` is explicitly
  `FALSE`. Separately, `nlsLM_fit` is now `NULL` (never the caught error
  condition) in the branch where `wrapnlsr` won after `nlsLM` failed
  (TICKET-065).

* **Requested plot annotations, extractor rows, and a summary CI section
  disappeared with no condition when their computation errored.**
  `plot_expenditure()` (hurdle and TMB): with `show_pmax = TRUE` /
  `show_omax = TRUE` explicitly requested, a `calc_group_metrics()` error
  silently omitted the annotation; now warns
  (`beezdemand_plot_annotation_warning`) naming the cause, matching the
  existing pattern used by `plot_compare()`. `coef.beezdemand_fixed()`:
  subject rows for a stored `try-error`/`NULL` fit, or one whose `coef()`
  call failed, were dropped with no way to tell "subject absent" from
  "subject failed"; now warns (`beezdemand_fixed_coef_omitted_warning`)
  naming the dropped ids. `augment.cp_model_nls()` /
  `augment.cp_model_lm()` / `augment.cp_model_lmer()`: the documented
  `.fitted`/`.resid`/`.fixed` columns vanished on a `fitted()`/
  `residuals()`/`predict()` error or a length mismatch with no indication;
  now warn (`beezdemand_cp_augment_omitted_warning`) naming the omitted
  column(s). `summary.cp_model_nls()`: a genuine `nlstools::confint2()`
  error (as opposed to `nlstools` simply not being installed, which stays
  silent) silently dropped the confidence-interval section; now warns
  (`beezdemand_cp_summary_ci_omitted_warning`). Healthy paths are
  unaffected and remain silent throughout (TICKET-068).

## Legacy fitter robustness (batch failures)

* `FitCurves(equation = "linear")` (and `fit_demand_fixed(equation =
  "linear")`) now degrades a per-subject fit failure to an NA-parameter row
  with an informative `Notes` message instead of crashing. The linear
  extractor (`ExtractCoefs.linear()`) was missing the try-error guard its
  nonlinear sibling (`ExtractCoefs()`) already has; on a failed `wrapnlsr`
  fit it dereferenced the resulting try-error immediately
  (`coef(fit)[c("l", "b", "a")]`), which raised `$ operator is invalid for
  atomic vectors` and aborted the entire batch with no per-subject failure
  record -- reproducible even for a single unfittable subject called alone.
  The extraction is now wrapped end-to-end so a mid-extraction failure
  (`summary()`, `nlstools::confint2()`, `deviance()`) also degrades
  gracefully rather than only the initial `coef()` call.
* `ExtraF()` now reports which group's per-group fit failed
  (`"ExtraF: unable to fit group '<name>': ..."`) instead of an opaque
  `no applicable method for 'predict' applied to an object of class
  "try-error"`. `GetSharedK()`'s shared-k search (start-value grid
  construction and the final `nlxb()` fit, neither of which was previously
  guarded) is now wrapped so any internal failure reaches its designed
  sentinel return (`"Unable to find a shared k."`) instead of escaping raw;
  `FitCurves(k = "share")` can therefore actually reach its documented
  fallback to `GetK()` with a warning, which previously could not fire
  because the sentinel path was unreachable (the final `nlxb()` call was
  never wrapped in `try()`, so the `inherits(fit, "try-error")` check after
  it could never be true). A shared-k group set that drops below 2 usable
  groups (after the existing <3-row-per-group drop) now returns an
  informative sentinel instead of proceeding into a nonsensical
  single-group contrast.

## New features

* **`simulate_hurdle_data(part2 = "snd")` (TICKET-044).** Adds an SND
  positive-part generator to the hurdle simulator, matching
  `src/HurdleDemand3RE_SND.h` / `src/HurdleDemand2RE_SND.h` exactly: a
  log-linear (no `k`) mean with lognormal errors on the positive part, and
  the same zero-inflation logistic as the existing (now `part2 = "koff"`)
  generator. Random-effect correlations for `part2 = "snd"` are specified
  via `rho_ab_raw`/`rho_ac_raw`/`rho_bc_raw`, mirroring the TMB model's own
  raw-parameter coefficients exactly (`rho_ab = tanh(rho_ab_raw)`, `rho_ac
  = tanh(rho_ac_raw)`, `rho_bc` via the LKJ-Cholesky partial-correlation
  transform), so a fitted `fit_demand_hurdle(part2 = "snd")` model's own
  coefficients can be plugged directly into the simulator for a parametric
  bootstrap or recovery study. `part2` defaults to `"koff"`, and its output
  is byte-identical to previous releases.

* `simulate_hurdle_data(seed = )` and `run_hurdle_monte_carlo(seed = )` no
  longer overwrite the caller's RNG stream: the global `.Random.seed` is
  restored on exit (or removed if none existed), matching what
  `power_demand()`, `boot_demand()` and the TMB parametric-draw helpers
  already guaranteed. Simulated outputs for a given `seed` are unchanged.

## Continuous within-subject random slopes in `fit_demand_tmb()`

* `fit_demand_tmb()` now treats a continuous within-subject covariate as a
  first-class random slope (dose-response demand). Specify it with a numeric
  random-effects term, e.g.
  `random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c)`, and pair it with
  `continuous_covariates = "dose_c"` for the population (fixed) dose slope.
  Each subject's intensity and elasticity then change with the covariate at
  their own rate, with far fewer parameters than the equivalent multi-level
  factor random slope.
* `get_subject_pars()` and `predict(type = "parameters")` expose the
  per-subject slope deviations (`q0_<term>` / `alpha_<term>`, matching
  `ranef()`) and accept an `at =` argument to evaluate per-subject `Q0`/`alpha`
  at a chosen covariate value (e.g. `at = c(dose_c = 0)`); the two surfaces now
  agree and no longer return `NA`.
* `summary()`, `VarCorr()`, and `tidy(effects = "ran_pars")` label the
  continuous-term variance component and its intercept correlation by the
  covariate name rather than a positional index.
* The identifiability guard runs on the complete-case fit data with concrete
  messages: a hard error when fewer than two subjects vary in the covariate, a
  warning when fewer than 80% do, and a centering reminder when the covariate
  is not centered. Slope-variance start values are scaled to the covariate's
  spread, and `check_demand_model()` flags a near-singular intercept/slope
  covariance.
* Existing factor-expanded, multi-block, and intercept-only fits are
  unaffected (output is byte-identical).

## Breaking change: hurdle predict() default (TICKET-042)

* `predict.beezdemand_hurdle()` now defaults to `type = "demand"` (the
  marginal expectation `(1 - p0) * E[Y | Y > 0]`) instead of
  `type = "response"` (the conditional positive mean `E[Y | Y > 0]`).
  Rationale: observed consumption includes zeros, so scoring predictions
  against raw data with the conditional mean systematically overstates
  hurdle error wherever the probability of zero consumption is large — a
  documented cross-validation erratum. This is a deliberate breaking
  change from 0.2.0: the marginal expectation is the statistically
  correct default for prediction scoring, and a clean flip (with
  communication) was judged better than shipping the footgun for another
  release. Omitting `type` emits a once-per-session message naming the
  change; every `type`'s computation is unchanged, so
  `type = "response"` restores the old behavior exactly. See the new
  "Scoring predictions" section in `?predict.beezdemand_hurdle`.

## Condition hygiene: sdreport warnings (TICKET-046)

* Weakly identified TMB and hurdle fits no longer leak raw "NaNs
  produced" warnings from `TMB::sdreport()`/`summary.sdreport()` SE
  extraction (one per offending block). They are replaced by at most ONE
  classed warning per fit (`beezdemand_sdreport_warning`, with parent
  class `beezdemand_warning`) pointing at `$hessian_pd` and `summary()`
  diagnostics. Matching is on the warning's `sqrt()` call rather than the
  locale-dependent message text, and unrelated warnings pass through
  untouched. The existing "Standard error computation failed" warning now
  carries the same classes. No fit results, SEs, or `hessian_pd` logic
  changed.

## Statistical-accuracy fixes (pre-release audit)

* **Bug fix.** `lambertW()` now accepts vector input. The Halley-iteration
  convergence check combined possibly-length-`n` operands with `&&`, which
  errors under R >= 4.3 ("length > 1 in coercion to logical(1)"); it now uses a
  vectorized `all(... &)`. Scalar results are unchanged, so existing scalar
  callers are unaffected. This hardens every analytic `Pmax`/`Omax` path that
  vectorizes over price.

* **Bug fix.** `get_empirical_measures()` now computes the breakpoint `BP0` and
  the empirical `Pmaxe` order-invariantly by sorting each series by price before
  walking consumption. Previously a subject whose rows were not in
  ascending-price order could receive an incorrect breakpoint; results are
  unchanged for already price-ordered input (the typical case). The deprecated
  legacy path `GetEmpirical()` received the same per-subject sort, so the two
  functions stay in agreement on unsorted input.

* **Consistency fix.** The analytic `Pmax`/`Omax` existence threshold is now a
  strict interior maximum across all equations. Both hurdle variants previously
  returned a finite `Pmax` at the exact boundary `k = e`; they now return `NA`
  there, matching the Hursh & Silberberg path (already strict at
  `k = e / ln(10)`). At the boundary the two stationary points merge into a
  tangent inflection, so no strict interior maximum exists. The practical effect
  is measure-zero. The deprecated numerical helper `calc_omax_pmax()` now
  classifies `k = e` the same way: it takes its bounded-range fallback (with
  the usual warning and `note`) instead of the analytic root path.

## NLME summary/tidy reporting fixes

* **Reporting change (broom convention).** `summary()` and `tidy()` now report the
  Wald `statistic` and `p.value` on the **estimation scale** (log/log10; logit for
  hurdle participation rows) for every `report_space`, including the default
  `"natural"`; only `estimate` and
  `std.error` are back-transformed. Previously the natural scale recomputed the
  test from the back-transformed estimate/SE, which is degenerate for log-scale
  parameters — the statistic reduces to `1/(c·SE)`, independent of (and dropping
  the sign of) the estimate, and for factor effects tests an impossible null
  (ratio = 0 rather than ratio = 1). Keeping the test on the estimation scale
  matches broom/emmeans/glmmTMB: for NLME the natural-scale test is nlme's native
  containment-t (DF-aware) test; for the TMB and hurdle tiers it is the `pnorm()`
  z-test. **This changes the default reported `statistic`/`p.value` for the core
  demand parameters (Q0/alpha/k); `estimate` and `std.error` are unchanged.**

* **Bug fix.** `summary(fit_nlme)$converged` now reflects the same operational
  convergence gate as `glance(fit_nlme)$converged` (positive-definite `apVar`
  **and** no terminal error; TICKET-020). It previously hard-coded `TRUE`, so a
  summary could report convergence on a fit that `glance()` correctly flagged as
  unusable for inference. Any diagnostic message now appears in the summary's
  `notes`.

## NLME comparison metadata

* **Bug fix.** The `compare_specs_used` attribute on
  `get_demand_comparisons(fit_nlme, ...)` output now records the user's
  requested comparison spec (or `"all fitted factors"` when none is given),
  matching the TMB backend. It previously stored the last parameter's
  internally-derived per-parameter formula, which under asymmetric
  `collapse_levels` was both misleading (it could show a collapsed
  intercept-only `~1`, or an internal collapsed column name) and dependent on
  the order of `param`.

## TMB VarCorr multi-block correlation placement

* **Bug fix.** `VarCorr(fit_tmb)` now places random-effect correlations on the
  correct rows for multi-block `pdBlocked` fits. The `Corr` entries were
  previously positioned using indices local to each covariance block, which was
  correct only when the correlated (`pdSymm`) block came first; a correlated
  block in any later position was placed on the wrong row. Positions are now
  derived from the same block map used to compute `summary(fit)$correlations`,
  adding each block's global row offset. Single-block fits are unaffected.

## Subject-level parameters for NLME fits (TICKET-034)

* **New method.** `get_subject_pars()` now has a `beezdemand_nlme` method,
  closing the last cross-backend asymmetry (every other demand-model extractor
  already had one). It returns the same columns as
  `get_subject_pars.beezdemand_tmb()` — wide form `id`, `b_i`, `c_i` (if alpha
  has random effects), `Q0`, `alpha`, `Pmax`, `Omax`; expanded form adds the
  within-subject factor column(s) with one row per (subject, factor-level)
  cell. `Q0`, `alpha`, `Pmax`, `Omax` are on the natural scale.

* Honors the same `expanded` contract as the TMB method: `NULL` (default)
  auto-detects within-id variation and expands when present; `TRUE` always
  attempts expansion; `FALSE` returns the wide shape and warns (with affected
  subjects' `Q0`/`alpha`/`Pmax`/`Omax` set to `NA`) when within-id variation
  exists. Per-subject parameters are reconstructed from the fixed effects plus
  each subject's random-effect deviations and back-transformed from the model's
  `param_space` (log10 by default) to the natural scale.

* The `b_i` / `c_i` random-effect aliases are reported on the natural-log
  linear-predictor scale to match the TMB method (the log10 deviations are
  multiplied by `log(10)` for `param_space = "log10"`). The full
  per-coefficient random effects remain available via `ranef()`. The
  backend-agnostic expansion scaffold is currently duplicated from the TMB
  method; unifying the two into a shared internal is a planned follow-up.

## NLME convergence reporting (TICKET-020)

* **Bug fix.** `glance(fit_nlme)$converged` no longer flips to `FALSE` merely
  because nlme emitted iteration-level convergence warnings (false convergence,
  singular precision matrix, step-halving, iteration limit, ...) during its
  PNLS-LME alternation. nlme prints these routinely while iterating even when the
  fit settles to a usable optimum, so the old heuristic produced false negatives
  on fits that were perfectly usable for inference (positive-definite `apVar`,
  valid emmeans/contrasts/predictions, no terminal error). `converged` now
  reports the operational gate: `apVar` is positive-definite **and** there is no
  terminal error.

* `glance(fit_nlme)` gains two additive NLME-only columns: **`final_fit_ok`**
  (the canonical usable-for-inference gate; identical to `converged`) and
  **`fit_warned`** (a diagnostic flag, `TRUE` when iteration-level convergence
  warnings were emitted — informational only, does not gate `converged`).

* **Breaking change (semantic).** Some fits previously flagged
  `converged = FALSE` (despite a usable `apVar`) now correctly read `TRUE`. Code
  that scripted around the old false-`FALSE` behavior should gate on
  `final_fit_ok` (the canonical check) and inspect `fit_warned` for iteration
  diagnostics. `check_demand_model(fit_nlme)` likewise no longer reports
  "Model did not converge" for warned-but-usable fits. The genuine-failure path
  is unchanged: a non-positive-definite `apVar` (or a terminal error) still reads
  `converged = FALSE`.

## get_demand_comparisons() NLME nested by-column naming (TICKET-033)

* **Breaking (NLME, narrow):** when `contrast_by` conditions on a factor that
  was asymmetrically collapsed via `collapse_levels`, the nested
  `$contrasts_log10` / `$contrasts_ratio` by-column(s) now use the
  **user-requested original** factor name (e.g. `age_group`) instead of the
  collapse-mapped name (e.g. `age_group_alpha`). This completes the TICKET-016
  harmonization: the NLME nested object now matches the TMB backend and the
  flat `tidy()` output, all of which already used the original name. The
  by-column **values** (the collapsed level labels) are unchanged. Only code
  that reads the suffixed nested by-column directly
  (`comps$alpha$contrasts_log10$age_group_alpha`) is affected — replace with the
  original name (`...$age_group`). `tidy()` output and the uncollapsed-fit case
  are unaffected.

* Two robustness guards were added to the NLME `contrast_by` path: supplying two
  by-variables that resolve to the same effective column (e.g.
  `c("age_group", "age_group_alpha")` under collapse) now aborts, **mirroring the
  TMB backend**; and a factor name that would collide with a reserved contrast
  column during the nested rename (e.g. a factor literally named `estimate`) now
  aborts with a clear message rather than failing cryptically (this collision
  guard is NLME-specific — it arises from the rename introduced here).

## get_demand_comparisons() by-grouped contrasts on TMB (TICKET-032)

* `get_demand_comparisons()` now supports `contrast_by` on the **TMB** backend,
  completing the NLME/TMB harmonization begun in TICKET-016. Within each
  observed combination of the by-level(s), pairwise (or `trt.vs.ctrl`)
  contrasts are computed over the remaining factors, with p-value adjustment
  applied **per by-cell**. Results match the NLME backend in shape, direction,
  by-cell labels, and message UX (numerics differ by design: TMB uses
  asymptotic *z*, NLME *t*). A by-cell of a single contrast reproduces the
  corresponding `at = `-filtered call exactly.

* Nested contrast tables (`$contrasts_log10`, `$contrasts_ratio`) and the flat
  `tidy()` frame gain leading by-column(s) using the **user-requested original**
  factor name (e.g. `age_cut`, not the collapse-mapped `age_cut_alpha`). The
  `tidy()` schema is unchanged (the canonical 9 columns) when `contrast_by` is
  inactive. `print()` shows the by-column(s) before `contrast`.

* Both backends now populate a `contrast_by_map` attribute (a per-parameter
  named map from the original by-name to the effective, possibly
  collapse-mapped, column). `contrast_by` resolution is **soft** per parameter
  (a by-variable absent from a parameter's design is skipped) whereas
  `compare_specs` resolution remains **strict** (an unresolvable name aborts).

* **Behavior change (NLME):** supplying a `contrast_by` factor that is not in
  `compare_specs` now aborts loudly with a parameter-scoped message, on both
  backends. Previously the NLME backend silently returned an empty contrasts
  table (with a `$contrasts_log10_error` note). No released code relied on the
  silent-empty path.

## get_demand_comparisons() backend harmonization (TICKET-016)

* `get_demand_comparisons()` now returns a classed `beezdemand_comparison`
  object on **both** the NLME and TMB backends, and a new
  `tidy.beezdemand_comparison()` method gives a backend-agnostic flat
  contrasts frame (`param`, `contrast`, `estimate`, `std.error`, `statistic`,
  `df`, `conf.low`, `conf.high`, `p.value`) with identical columns regardless
  of backend. Estimates and CIs are reported on the log10 scale on both
  backends; `tidy(res, exponentiate = TRUE)` returns base-invariant ratios.
  (Per-backend inference is unchanged: NLME reports a *t* statistic with finite
  `df`, TMB an asymptotic *z* with `df = Inf`.)

* `get_demand_comparisons()` now compares **both** `Q0` and `alpha` by default
  on both backends (the TMB backend previously returned `Q0` only).

* The TMB backend gains `compare_specs`, a formal `at` argument,
  `report_ratios`, factor-level contrast ordering (previously
  data-appearance order, which could flip signs when input rows were
  reordered), and **equal-weight marginalization over omitted factors**
  (averaging across the full crossing of their levels, emmeans' default
  `weights = "equal"`, matching the NLME backend). `get_demand_param_emms()`
  for TMB fits likewise marginalizes when `factors_in_emm` names a subset of
  the fitted factors, rather than erroring.

* **Behavior change (NLME):** the default p-value adjustment is now `"holm"`
  (was `"tukey"`); pass `adjust = "tukey"` to retain the previous default.
  Rationale: cross-backend reproducibility and the base-R pairwise default.

* **Deprecation (NLME):** `get_demand_comparisons(params_to_compare = )` is
  deprecated in favor of `param`. Supplying both is an error.

* TMB API (new in this release, so no released code is affected):
  `get_demand_comparisons.beezdemand_tmb()` renames `p_adjust` to `adjust`
  (no alias) and validates it against `stats::p.adjust.methods`; emmeans-only
  methods (e.g. `"tukey"`, `"sidak"`) are rejected.

* `contrast_by` (by-grouped contrasts) is now supported on the TMB backend
  (TICKET-032, see below), completing the backend harmonization.

* **Correctness fixes (post-review).** (1) The NLME backend now validates
  `compare_specs` against the union of the model's fitted factors (originals
  and per-parameter collapsed columns) and errors on names not in that union,
  matching the TMB boundary check; cross-parameter aliases that pass the
  boundary but cannot resolve for a given parameter (e.g. `~ age_group_alpha`
  with `param = "Q0"`) abort with a parameter-scoped message rather than
  silently producing an intercept-only EMM. Factors whose collapsed column has
  fewer than 2 levels for a parameter still return empty contrasts for that
  parameter without error (the existing intentional behavior). (2) Under
  asymmetric `collapse_levels`, naming the **original** factor in
  `compare_specs`/`factors_in_emm` (e.g. `~ age_group`) now resolves to that
  parameter's collapsed column on the TMB backend (`age_group_Q0` /
  `age_group_alpha`), as it already did on NLME, instead of silently returning
  zero contrasts; a name that cannot be resolved for the parameter is rejected
  with an error. (3) TMB EMM and contrast reference grids are now built using
  the **fitted** design's contrasts, so changing the global
  `options("contrasts")` between fitting and calling no longer silently changes
  the estimates.

## New features (TICKET-023)

* `fit_demand_tmb()` fits are now substantially smaller on disk by default
  (often >80% smaller via `saveRDS()` on large datasets). The full
  covariance matrix of all ADREPORT'd quantities (`$sdr$cov`), which is read
  by no method, is no longer materialized; `$sdr$cov` is a scalar `NA` unless
  the new `store_report_cov = TRUE` argument is supplied. Standard errors,
  `cov.fixed`, variance components, and all inference (`coef()`, `vcov()`,
  `tidy()`, `confint()`, `get_demand_param_emms()`, `get_demand_comparisons()`,
  `boot_demand()`) are unchanged. Pass `store_report_cov = TRUE` to restore
  the prior behavior.

## New features (TICKET-025)

* `calc_group_metrics()` gains a `beezdemand_nlme` method, giving
  `fit_demand_mixed()` fits the same population-level metric summary
  (`Pmax`, `Omax`, `Qmax`, `elasticity_at_pmax`) already available for
  `beezdemand_tmb` fits. Fixed-effect `Q0` and `alpha` estimated marginal
  means are marginalized parameter-first (a geometric mean across the
  reference grid; continuous covariates at their training mean and factor
  levels equally weighted, unless conditioned via `at`), then the scalar
  metrics are derived through the shared Pmax/Omax engine. Returns the same
  flat list (`Pmax`, `Omax`, `Qmax`, `elasticity_at_pmax`, `method`,
  `conditioned_on`) as the TMB method.

## New features (TICKET-024)

* `boot_demand()` computes parametric-bootstrap confidence intervals on
  derived demand metrics (`Pmax`, `Omax`, `Qmax`, `EV`,
  `elasticity_at_pmax`) for `beezdemand_tmb` fits. Draws of the
  fixed-effect parameter vector are taken from the joint asymptotic
  posterior, mapped to per-condition `(Q0, alpha, k)` through the model
  design, and summarized by empirical quantiles. Returns one row per
  `(statistic, condition)` (`statistic`, `condition`, `estimate`,
  `conf.low`, `conf.high`, `level`); the per-cell point estimate matches
  `calc_group_metrics(fit, at = cell)`. Uncertainty in `k` is propagated
  when `k` is estimated. NLME fits and nonparametric resampling are
  planned for a follow-up.

## Breaking changes (TICKET-022)

* `get_subject_pars()` on `beezdemand_tmb` fits now auto-detects fits
  with within-id-varying design columns (factor-expanded random
  effects, within-id continuous covariates, or multi-block
  `pdBlocked` specs) and runs the expansion machinery **by default**.
  The returned shape depends on the kind of within-id variation:
  for fits with within-id **factors**, rows are expanded across factor
  levels (one row per (subject, factor-level) cell with per-cell
  `Q0`, `alpha`, `Pmax`, `Omax`); for fits whose only within-id
  variation is in **numeric covariates**, numerics are conditioned at
  the subject's mean and the return is one row per subject with
  finite (non-`NA`) `Q0`/`alpha`. Previously the default returned the
  wide one-row-per-subject shape with `NA` in `Q0`, `alpha`, `Pmax`,
  and `Omax` for affected subjects — a UX dead-end. The new default
  signature is `expanded = NULL` (auto-detect); pass `expanded = TRUE`
  or `expanded = FALSE` for explicit override. For fits without
  within-id variation the behavior is unchanged (the auto-detect
  path resolves to the wide shape).
* `get_subject_pars(fit, expanded = FALSE)` on a fit with within-id
  variation now emits a one-line warning to flag that the returned
  `Q0` / `alpha` / `Pmax` / `Omax` columns are `NA`. Pre-change this
  case was silent.
* If your existing code relied on the wide NA-filled output to detect
  within-id variation programmatically, switch to passing
  `expanded = FALSE` explicitly (and `suppressWarnings()` the new
  one-line warning) or check `any(is.na(fit$subject_pars$Q0))`
  directly. The fit-time 4-line warning at fit time
  (`R/tmb-demand.R`) is unchanged.

## New features (TICKET-019)

* `coef()` on `beezdemand_tmb` fits gains
  `type = c("internal", "subject", "combined", "fixed")`. The default
  (`"internal"`) is **unchanged** — it still returns the raw optimizer
  coefficient vector, so `fixef()` and tooling that dispatches via
  `coef()` (e.g. `car::deltaMethod`, `multcomp::glht`) are unaffected.
  `type = "subject"` (alias `"combined"`) returns the per-subject
  parameter tibble (`get_subject_pars(fit)`, auto-detecting within-id
  factor expansion); `type = "fixed"` returns a one-row tibble of the
  fixed-effect coefficients. Supplying `report_space` through `...` is an
  error (no scale conversion in `coef()`).

## New features (TICKET-018)

* `confint()` on `beezdemand_tmb` fits gains
  `method = c("wald", "simulate")`. The default (`"wald"`) is
  **unchanged**. `"simulate"` draws `R` parametric Monte Carlo samples
  (default `R = 1000`, with an optional `seed`) from the joint asymptotic
  Gaussian posterior `N(coef(fit), vcov(fit))` and reports per-coefficient
  empirical quantiles. It is *diagnostic*: the simulated intervals are
  asymptotically Wald-equivalent on per-coefficient CIs (useful as a
  side-by-side check on the Gaussian approximation), do not improve on
  Wald at boundary cases, and carry no positivity guarantee on the
  internal scale. No new package dependency is added.

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
  or any tool that dispatches via `coef()` after a future
  `coef()` default change. Those each remain follow-up tickets.

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
  across a future `coef()` default change to a per-subject tibble.
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

## broom-method harmonization across NLME and TMB (TICKET-017)

The `tidy()` and `glance()` introspection methods now expose the same
column names, default arguments, and component labels on the
`beezdemand_nlme` and `beezdemand_tmb` backends, so backend-agnostic code
needs no dispatch glue.

* **Breaking change.** `glance(fit_tmb)$equation` is renamed to
  `equation_form`, matching `glance(fit_nlme)`. There is no aliased
  `equation` column. The `fit_demand_tmb()` API is new in 0.3.0 (this
  rename happened during its development), so no released code depends on
  the old name.
* **Breaking change.** `tidy(fit_tmb)` labels fixed-effect rows
  `component == "fixed"` instead of `"consumption"`, matching
  `tidy(fit_nlme)` and the `nlme` / `lme4` convention. Code filtering TMB
  `tidy()` output on `component == "consumption"` will return zero rows.
  Hurdle methods are unchanged. (`summary(fit_tmb)$coefficients` was
  harmonized to `"fixed"` separately in the TICKET-031 follow-up below.)
* **Behavior change.** `tidy(fit_tmb, effects = "ran_pars")` reports the
  random-effect variance components on the same scale as
  `summary(fit_tmb)$variance_components` -- Q0/alpha RE SDs on the log10
  scale, residual SD on the likelihood scale -- rather than the raw
  internal `logsigma` optimizer coefficients. `std.error` is `NA` for
  these rows, as it is for `tidy(fit_nlme)`.
* `tidy.beezdemand_tmb()` gains an `effects` argument
  (`c("fixed", "ran_pars")`, both by default) matching
  `tidy.beezdemand_nlme()`: `effects = "fixed"` returns the fixed-effect
  rows, `effects = "ran_pars"` returns the variance-component rows. An
  invalid value is rejected with a `match.arg()`-style error.
* `glance(fit_nlme)` gains an `n_random_effects` column (the count of
  random-effect terms), so the canonical `glance()` columns --
  `model_class`, `backend`, `equation_form`, `nobs`, `n_subjects`,
  `n_random_effects`, `converged`, `logLik`, `AIC`, `BIC` -- are now
  identical across both backends.
* **Breaking change (TICKET-030, TICKET-017 follow-up).**
  `tidy(fit_nlme, effects = "ran_pars")$estimate` now reports random-effect
  *standard deviations* (pulled from `nlme::VarCorr(model)[, "StdDev"]`),
  not variances. This matches the `tidy(fit_tmb)` sibling (post-TICKET-015)
  and the `broom.mixed::tidy.lme` upstream convention, closing the
  cross-backend divergence on `"ran_pars"` rows. Migration: callers that
  consumed the previous value as a variance should square the estimate
  (`estimate^2`) or read `nlme::VarCorr(fit$model)[, "Variance"]` directly.
  Hurdle and fixed tiers are unaffected.
* **Breaking change (TICKET-031, TICKET-017 follow-up).**
  `summary(fit_tmb)$coefficients$component` now also emits `"fixed"` for
  q0 / alpha / log_k rows, matching `tidy(fit_tmb)` (renamed in TICKET-017
  above) and `summary(fit_nlme)$coefficients`. Code filtering
  `summary(fit_tmb)$coefficients` on `component == "consumption"` will
  return zero rows. `summary(fit_tmb)$derived_metrics$component` is
  deliberately left as `"consumption"` -- those rows describe derived
  demand metrics (pmax, omax, q_at_pmax, elasticity_at_pmax), not fitted
  coefficients, and a future ticket may rename them to `"derived"` or
  `"metric"`. Hurdle methods are unchanged.

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
  `plot_demand_overlay`, and the cross-model forest plot
  `plot_model_comparison()`, which compares parameter estimates and CIs
  across any fitted demand models via their `tidy()` methods).

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
  `newdata`; an unknown subject id at `level = "subject"` is an error
  (use `level = "population"` for the random-effects-at-zero prediction).
  Previously the function silently used cached subject parameters for
  known subjects and the reference-level intercepts for unknown ones,
  producing systematically biased `.fitted` values.
  `augment.beezdemand_tmb()` inherits the fix. Predict now also errors
  clearly when `newdata` is missing a required modeling column or
  contains factor levels not seen in training.

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

* `summary.beezdemand_nlme()` / `tidy()` keep the Wald `statistic`/`p.value` on
  the estimation scale when `report_space != internal_space`, so natural-scale
  inference is nlme's native containment-t (DF-aware) test rather than a
  recomputed (and degenerate) natural-scale Wald test. Only `estimate`/`std.error`
  are back-transformed (broom convention; supersedes the earlier TICKET-006
  delta-method recompute).

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
