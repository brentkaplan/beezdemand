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
  # `pmax_at_bound` (Codex review of GH #19, TMB tier) is TMB-only: it flags
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
