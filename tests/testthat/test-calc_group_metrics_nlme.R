# Tests for calc_group_metrics.beezdemand_nlme()
# Mirrors the TMB-side return contract (flat scalar list); see
# internal_docs/tickets/TICKET-025-REFINED-2026-05-23.md.

# Gender subsample (2 usable levels), mirroring helper_subsample_apt_full()
# in test-calc_group_metrics.R. Distinct name to avoid cross-file collision.
.cgm_nlme_subsample <- function(n_per_group = 25) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ig <- unique(d$id[d$gender == g])
    head(ig[order(ig)], n_per_group)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d$y_ll4 <- ll4(d$y, lambda = 4)
  d
}

# Shared zben + gender NLME fit, memoized so it is fit at most once per file run
# (testthat parallelizes files, not test_that() blocks, so a file-level cache is
# valid). Reused by the shape, self-consistency, at=, and invalid-at tests to
# bound peak memory on CI. Pattern mirrors test-boot-demand.R.
.cgm_cache <- new.env(parent = emptyenv())

.cgm_zben_gender_fit <- function() {
  if (is.null(.cgm_cache$zben_gender)) {
    .cgm_cache$zben_gender <- suppressMessages(fit_demand_mixed(
      .cgm_nlme_subsample(), equation_form = "zben", factors = "gender",
      y_var = "y_ll4", x_var = "x", id_var = "id"))
  }
  .cgm_cache$zben_gender
}

# Manual parameter-first marginalization reference: geometric mean of the
# per-cell natural-scale EMMs fed to the shared engine. Mirrors the method's
# internals but composed only from public pieces, so it is an independent check.
.cgm_nlme_reference <- function(fit, model_type, k = NULL, at = NULL) {
  eq <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "Q0", at = at, factors_in_emm = NULL, include_ev = FALSE)))
  ea <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "alpha", at = at, factors_in_emm = NULL, include_ev = FALSE)))
  gm <- function(v) {
    v <- v[is.finite(v) & v > 0]
    exp(mean(log(v)))
  }
  Q0 <- gm(eq$Q0_natural)
  al <- gm(ea$alpha_natural)
  if (identical(model_type, "hs")) {
    beezdemand_calc_pmax_omax(
      "hs", list(alpha = al, q0 = Q0, k = k),
      list(alpha = "natural", q0 = "natural", k = "natural"))
  } else {
    beezdemand_calc_pmax_omax(
      "snd", list(alpha = al, q0 = Q0),
      list(alpha = "natural", q0 = "natural"))
  }
}

# Simulated 3-level between-id factor dataset (zben-fittable) for the
# overlapping-label collapse test.
.cgm_nlme_collapse_data <- function() {
  set.seed(123)
  grp_levels <- c("A", "B", "C")
  prices <- c(0, 0.5, 1, 2, 4, 8, 16)
  rows <- list()
  uid <- 0
  for (g in grp_levels) {
    for (i in seq_len(12)) {
      uid <- uid + 1
      q0 <- 12 + rnorm(1, 0, 2) + switch(g, A = 0, B = 2, C = 4)
      a <- max(0.012 + rnorm(1, 0, 0.002) + switch(g, A = 0, B = 0.002, C = -0.002), 0.002)
      y <- q0 * exp(-a * q0 * prices) + rnorm(length(prices), 0, 0.3)
      y[y < 0] <- 0
      rows[[length(rows) + 1]] <- data.frame(id = uid, grp = g, x = prices, y = y)
    }
  }
  d <- do.call(rbind, rows)
  d$grp <- factor(d$grp, levels = grp_levels)
  d$id <- factor(d$id)
  d$y_ll4 <- ll4(d$y, lambda = 4)
  d
}

.fields <- c("Pmax", "Omax", "Qmax", "elasticity_at_pmax", "method", "conditioned_on")

# ---------------------------------------------------------------------------
# 1. Shape (snd path): flat scalar list, exact fields, no tibble, no EV.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme returns the flat scalar list", {
  skip_on_cran()
  fit <- .cgm_zben_gender_fit()

  expect_no_warning(cm <- calc_group_metrics(fit))
  expect_type(cm, "list")
  expect_false(inherits(cm, "tbl_df") || inherits(cm, "data.frame"))
  expect_identical(sort(names(cm)), sort(.fields))
  expect_false("EV" %in% names(cm))
  for (m in c("Pmax", "Omax", "Qmax", "elasticity_at_pmax")) {
    expect_length(cm[[m]], 1L)
    expect_true(is.finite(cm[[m]]))
  }
  expect_type(cm$method, "character")
})

# ---------------------------------------------------------------------------
# 2. Shape parity vs TMB (names + conditioned_on substructure; NOT values).
# ---------------------------------------------------------------------------
test_that("calc_group_metrics nlme and tmb share field + conditioned_on shape", {
  skip_on_cran()
  fit_nlme <- .cgm_zben_gender_fit()
  # Shape parity only checks field names / conditioned_on structure (not
  # values), so use the smallest adequate factored TMB fit to bound memory --
  # a large $sdr$cov is unnecessary here. (Also emits a benign NaN-SE warning
  # from sdreport on small data; suppress it.)
  fit_tmb <- suppressWarnings(fit_demand_tmb(
    .cgm_nlme_subsample(n_per_group = 8),
    equation = "exponential", factors = "gender", verbose = 0))

  cm_nlme <- calc_group_metrics(fit_nlme)
  cm_tmb <- calc_group_metrics(fit_tmb)
  # `pmax_at_bound` (GH #19, TMB tier) is TMB-only: it flags
  # when the zben numerical Pmax search hits its domain-expansion cap, which
  # has no NLME counterpart (NLME zben Pmax/Omax are out of scope for that
  # ticket). Checked for presence explicitly instead of folding it into the
  # blanket name-set comparison, so the rest of the shape parity stays a
  # real (not weakened) equality check.
  expect_true("pmax_at_bound" %in% names(cm_tmb))
  expect_false("pmax_at_bound" %in% names(cm_nlme))
  expect_equal(
    sort(setdiff(names(cm_nlme), "pmax_at_bound")),
    sort(setdiff(names(cm_tmb), "pmax_at_bound"))
  )
  expect_equal(sort(names(cm_nlme$conditioned_on)),
               sort(names(cm_tmb$conditioned_on)))
  expect_equal(sort(names(cm_nlme$conditioned_on$factors)),
               sort(names(cm_tmb$conditioned_on$factors)))
})

# ---------------------------------------------------------------------------
# 3. Self-consistency, snd path (zben): metrics == manual marginalization.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme is self-consistent (snd)", {
  skip_on_cran()
  fit <- .cgm_zben_gender_fit()

  cm <- calc_group_metrics(fit)
  ref <- .cgm_nlme_reference(fit, "snd")
  expect_equal(cm$Pmax, ref$pmax_model, tolerance = 1e-6)
  expect_equal(cm$Omax, ref$omax_model, tolerance = 1e-6)
  expect_equal(cm$Qmax, ref$q_at_pmax_model, tolerance = 1e-6)
  expect_equal(cm$elasticity_at_pmax, ref$elasticity_at_pmax_model, tolerance = 1e-6)
  expect_equal(cm$method, ref$method_model)
})

# ---------------------------------------------------------------------------
# 4. Self-consistency, hs/k path (exponentiated): exercises has_k + param_info$k.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme is self-consistent (hs/k)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_mixed(
    apt, equation_form = "exponentiated",
    y_var = "y", x_var = "x", id_var = "id")
  expect_false(is.null(fit$param_info$k))

  cm <- calc_group_metrics(fit)
  ref <- .cgm_nlme_reference(fit, "hs", k = fit$param_info$k)
  expect_equal(cm$Pmax, ref$pmax_model, tolerance = 1e-6)
  expect_equal(cm$Omax, ref$omax_model, tolerance = 1e-6)
  expect_equal(cm$method, ref$method_model)
  expect_match(cm$method, "lambert_w")
})

# ---------------------------------------------------------------------------
# 5. Intercept-only -> conditioned_on NULL.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme: intercept-only conditioned_on NULL", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)
  fit <- fit_demand_mixed(
    apt, equation_form = "zben", y_var = "y_ll4", x_var = "x", id_var = "id")

  cm <- calc_group_metrics(fit)
  expect_type(cm, "list")
  expect_null(cm$conditioned_on)
  expect_length(cm$Pmax, 1L)
  expect_true(is.finite(cm$Pmax))
})

# ---------------------------------------------------------------------------
# 6. at = list(gender = level): records the level; metric self-consistent.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme honors at = factor level", {
  skip_on_cran()
  fit <- .cgm_zben_gender_fit()

  cm <- calc_group_metrics(fit, at = list(gender = "Male"))
  expect_equal(cm$conditioned_on$factors$gender, "Male")
  ref <- .cgm_nlme_reference(fit, "snd", at = list(gender = "Male"))
  expect_equal(cm$Pmax, ref$pmax_model, tolerance = 1e-6)
  expect_equal(cm$Omax, ref$omax_model, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 7. Invalid at name errors helpfully.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme errors on invalid at name", {
  skip_on_cran()
  fit <- .cgm_zben_gender_fit()

  expect_error(
    calc_group_metrics(fit, at = list(nonexistent = "value")),
    "nonexistent")
})

# ---------------------------------------------------------------------------
# 8. collapse_levels (overlapping labels): NA join rows filtered -> finite metrics.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme filters NA rows under overlapping-label collapse", {
  skip_on_cran()
  d <- .cgm_nlme_collapse_data()
  # Shared collapsed label "x" maps to different originals for Q0 (A+B) vs
  # alpha (A only), so get_demand_param_emms()'s internal join yields NA rows.
  cl <- list(
    Q0 = list(grp = list(x = c("A", "B"))),
    alpha = list(grp = list(x = "A", y = c("B", "C"))))
  fit <- fit_demand_mixed(
    d, equation_form = "zben", factors = "grp",
    y_var = "y_ll4", x_var = "x", id_var = "id", collapse_levels = cl)

  # Precondition: the per-param EMM table actually carries NA rows (else the
  # finite-positive filter in calc_group_metrics is not exercised).
  emm_q0 <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "Q0", factors_in_emm = NULL, include_ev = FALSE)))
  expect_true(any(is.na(emm_q0$Q0_natural)))

  cm <- calc_group_metrics(fit)
  expect_identical(sort(names(cm)), sort(.fields))
  for (m in c("Pmax", "Omax", "Qmax", "elasticity_at_pmax")) {
    expect_true(is.finite(cm[[m]]))
  }
})

# ---------------------------------------------------------------------------
# 9. Covariate at + default training-mean conditioning.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme honors at = covariate value", {
  skip_on_cran()
  d <- .cgm_nlme_subsample()
  fit <- fit_demand_mixed(
    d, equation_form = "zben", continuous_covariates = "age",
    y_var = "y_ll4", x_var = "x", id_var = "id")

  cm_default <- calc_group_metrics(fit)
  expect_equal(cm_default$conditioned_on$covariates[["age"]],
               mean(d$age, na.rm = TRUE), tolerance = 1e-6)

  cm_at <- calc_group_metrics(fit, at = list(age = 30))
  expect_equal(cm_at$conditioned_on$covariates[["age"]], 30)
})

# ---------------------------------------------------------------------------
# 10. TICKET-064 (F12): targeted muffling, not blanket suppression.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme propagates a real emms warning (estimate-column fallback)", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  d <- .cgm_nlme_subsample()
  fit <- fit_demand_mixed(
    d, equation_form = "zben",
    y_var = "y_ll4", x_var = "x", id_var = "id")

  # Directly injecting the "Could not reliably identify estimate column"
  # warning is impractical to trigger organically (it needs a malformed
  # emmeans summary shape); instead verify the muffle helper's contract at
  # the unit level: a non-benign, non-gate warning raised inside the wrapped
  # expression is NOT muffled.
  warn_fn <- function() {
    warning("Could not reliably identify estimate column for log10 scale of Q0. Using 'x'.")
    1
  }
  warns <- testthat::capture_warnings(
    beezdemand:::.nlme_muffle_group_metrics_emms_noise(warn_fn())
  )
  expect_true(any(grepl("Could not reliably identify estimate column", warns)))
})

test_that(".nlme_muffle_group_metrics_emms_noise mutes only the benign message + the convergence-gate duplicate", {
  msg_fn <- function() {
    message("No factors specified or found in model. Reporting global parameter estimates.")
    1
  }
  expect_no_message(beezdemand:::.nlme_muffle_group_metrics_emms_noise(msg_fn()))

  other_msg_fn <- function() {
    message("some other message")
    1
  }
  expect_message(
    beezdemand:::.nlme_muffle_group_metrics_emms_noise(other_msg_fn()),
    "some other message"
  )

  gate_warn_fn <- function() {
    warning(structure(
      class = c("beezdemand_nlme_convergence_warning", "beezdemand_warning", "warning", "condition"),
      list(message = "gate warning", call = NULL)
    ))
    1
  }
  expect_no_warning(beezdemand:::.nlme_muffle_group_metrics_emms_noise(gate_warn_fn()))
})

# ---------------------------------------------------------------------------
# F-BD6-3 (audit 2026-09-06): a multi-value continuous `at` used to be
# forwarded whole to emmeans (the grid expanded over every value and the
# geometric mean averaged across them) while `conditioned_on` recorded only
# the first value. The NLME method now normalises to the first value with
# the same warning the TMB method emits, so the recorded conditioning is the
# conditioning that was applied.
# ---------------------------------------------------------------------------
test_that("calc_group_metrics.beezdemand_nlme: multi-value continuous `at` warns and uses the first value (F-BD6-3)", {
  skip_on_cran()
  d <- .cgm_nlme_subsample()
  fit <- fit_demand_mixed(
    d, equation_form = "zben", continuous_covariates = "age",
    y_var = "y_ll4", x_var = "x", id_var = "id")

  cm_one <- calc_group_metrics(fit, at = list(age = 30))
  expect_warning(
    cm_two <- calc_group_metrics(fit, at = list(age = c(30, 60))),
    "using first value"
  )
  expect_equal(cm_two$conditioned_on$covariates[["age"]], 30)
  expect_equal(cm_two$Pmax, cm_one$Pmax, tolerance = 1e-10)
  expect_equal(cm_two$Omax, cm_one$Omax, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Batch 3 (2026-09-12), F-BD6-3: marginalise over OBSERVED factor cells with
# equal weight (geometric mean), matching the TMB backend; F-BD6-5: `at` on a
# collapse_levels fit was silently ignored.
# ---------------------------------------------------------------------------

# apt_full subsample with a second between-subject factor `site` whose
# (Female, B) cell has no subjects (probe: scratchpad bd3/probe-fixtures.R).
.cgm_nlme_incomplete_data <- function() {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(factor(d$gender))
  ids <- unique(d[c("id", "gender")])
  set.seed(1)
  ids$site <- ifelse(ids$gender == "Female", "A", sample(c("A", "B"), nrow(ids), TRUE))
  keep <- unlist(lapply(split(ids$id, paste(ids$gender, ids$site)), head, 20))
  d <- d[d$id %in% keep, ]
  d$site <- factor(ids$site[match(d$id, ids$id)])
  d$id <- droplevels(factor(d$id))
  d$y_ll4 <- ll4(d$y, lambda = 4)
  d
}

.cgm_geom_mean <- function(v) {
  v <- v[is.finite(v) & v > 0]
  exp(mean(log(v)))
}

test_that("calc_group_metrics.beezdemand_nlme averages over observed cells only (F-BD6-3)", {
  skip_on_cran()
  d <- .cgm_nlme_incomplete_data()
  cells <- unique(d[c("gender", "site")])
  expect_identical(nrow(cells), 3L)  # (Female, B) unobserved

  fit <- suppressMessages(fit_demand_mixed(
    d, equation_form = "zben", factors = c("gender", "site"),
    factor_interaction = FALSE, y_var = "y_ll4", x_var = "x", id_var = "id"))
  skip_if(is.null(fit$model), "incomplete-design NLME fit failed")

  eq <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "Q0", factors_in_emm = NULL, include_ev = FALSE)))
  ea <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "alpha", factors_in_emm = NULL, include_ev = FALSE)))
  expect_identical(nrow(eq), 4L)  # the full factorial grid has the empty cell
  ref_obs <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = .cgm_geom_mean(merge(ea, cells)$alpha_natural),
                  q0 = .cgm_geom_mean(merge(eq, cells)$Q0_natural)),
    param_scales = list(alpha = "natural", q0 = "natural"))
  ref_full <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = .cgm_geom_mean(ea$alpha_natural),
                  q0 = .cgm_geom_mean(eq$Q0_natural)),
    param_scales = list(alpha = "natural", q0 = "natural"))

  cm <- calc_group_metrics(fit)
  expect_true(is.numeric(cm$Pmax) && is.finite(cm$Pmax))
  expect_equal(cm$Pmax, ref_obs$pmax_model, tolerance = 1e-6)
  expect_equal(cm$Omax, ref_obs$omax_model, tolerance = 1e-6)
  # Negative control: the full-grid estimand is a different number.
  expect_false(isTRUE(all.equal(cm$Pmax, ref_full$pmax_model, tolerance = 1e-4)))

  # `at` on one factor restricts the observed cells of that factor.
  cm_f <- calc_group_metrics(fit, at = list(gender = "Female"))
  eq_f <- merge(eq, cells[cells$gender == "Female", ])
  ea_f <- merge(ea, cells[cells$gender == "Female", ])
  expect_identical(nrow(eq_f), 1L)
  ref_f <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = .cgm_geom_mean(ea_f$alpha_natural),
                  q0 = .cgm_geom_mean(eq_f$Q0_natural)),
    param_scales = list(alpha = "natural", q0 = "natural"))
  expect_equal(cm_f$Pmax, ref_f$pmax_model, tolerance = 1e-6)
  # A requested cell with no data errors instead of extrapolating.
  expect_error(calc_group_metrics(fit, at = list(gender = "Female", site = "B")),
               "No usable")
})

test_that("calc_group_metrics.beezdemand_nlme honours `at` under partial collapse_levels (F-BD6-5)", {
  skip_on_cran()
  d <- .cgm_nlme_collapse_data()
  # Collapse only Q0's factor; alpha keeps the original `grp` column.
  cl <- list(Q0 = list(grp = list(x = c("A", "B"))))
  fit <- suppressMessages(fit_demand_mixed(
    d, equation_form = "zben", factors = "grp",
    y_var = "y_ll4", x_var = "x", id_var = "id", collapse_levels = cl))
  skip_if(is.null(fit$model), "partial-collapse NLME fit failed")
  expect_true("grp_Q0" %in% names(fit$data))
  expect_identical(fit$param_info$factors_alpha, "grp")

  cm_all <- calc_group_metrics(fit)
  cm_a <- calc_group_metrics(fit, at = list(grp = "A"))
  cm_c <- calc_group_metrics(fit, at = list(grp = "C"))
  for (cm in list(cm_all, cm_a, cm_c)) expect_true(is.finite(cm$Pmax))
  expect_identical(cm_a$conditioned_on$factors$grp, "A")
  # Before batch 3 the restriction was silently dropped (cm_a == cm_all).
  expect_false(isTRUE(all.equal(cm_a$Pmax, cm_all$Pmax, tolerance = 1e-4)))
  expect_false(isTRUE(all.equal(cm_c$Pmax, cm_all$Pmax, tolerance = 1e-4)))
  expect_false(isTRUE(all.equal(cm_a$Pmax, cm_c$Pmax, tolerance = 1e-4)))

  # Reference for at = "C": Q0 cell = collapsed label "C" (unchanged level),
  # alpha cell = "C".
  eq <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "Q0", factors_in_emm = NULL, include_ev = FALSE)))
  ea <- suppressWarnings(suppressMessages(get_demand_param_emms(
    fit, param = "alpha", factors_in_emm = NULL, include_ev = FALSE)))
  ref_c <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = ea$alpha_natural[ea$grp == "C"],
                  q0 = eq$Q0_natural[eq$grp == "C"]),
    param_scales = list(alpha = "natural", q0 = "natural"))
  expect_equal(cm_c$Pmax, ref_c$pmax_model, tolerance = 1e-6)
})

test_that("calc_group_metrics.beezdemand_nlme runs when a parameter collapses to one level", {
  skip_on_cran()
  d <- .cgm_nlme_collapse_data()
  cl <- list(alpha = list(grp = list(all = c("A", "B", "C"))))
  fit <- suppressMessages(suppressWarnings(fit_demand_mixed(
    d, equation_form = "zben", factors = "grp",
    y_var = "y_ll4", x_var = "x", id_var = "id", collapse_levels = cl)))
  skip_if(is.null(fit$model), "single-level-collapse NLME fit failed")
  cm <- calc_group_metrics(fit)
  expect_true(is.finite(cm$Pmax))
  cm_b <- calc_group_metrics(fit, at = list(grp = "B"))
  expect_true(is.finite(cm_b$Pmax))
  expect_false(isTRUE(all.equal(cm_b$Pmax, cm$Pmax, tolerance = 1e-4)))
})
