# TICKET-047: multi-start default protocol for fit_demand_fixed()
#
# Shipped spec (release-train plan section 1, decision D3): FitCurves() is
# always run exactly as before (the "production start"). A subject whose
# production fit is strict-converged is accepted immediately and NEVER
# refit -- its row/fit/prediction/data stay byte-identical to the S = 1
# protocol by construction. Only subjects that are NOT strict-converged get
# S - 1 additional sampled starts; the minimum-AbsSS strict-converged
# sampled start wins. FitCurves() itself and equation = "linear" are
# untouched.

# small helper: treat NA as FALSE for converged_strict comparisons
isTRUE_vec <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

test_that("(a) multistart = FALSE / S = 1 reproduce current output exactly on apt", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  legacy <- suppressWarnings(suppressMessages(FitCurves(
    apt, equation = "hs", k = 2, detailed = TRUE
  )))
  modern_off <- suppressWarnings(fit_demand_fixed(
    apt, equation = "hs", k = 2, multistart = FALSE
  ))
  modern_s1 <- suppressWarnings(fit_demand_fixed(
    apt, equation = "hs", k = 2, S = 1
  ))

  new_cols <- c("n_starts_tried", "n_starts_converged", "start_source")
  expect_identical(
    modern_off$results[, setdiff(names(modern_off$results), c(new_cols, "converged"))],
    legacy$dfres[, setdiff(names(legacy$dfres), "converged")]
  )
  expect_identical(
    modern_s1$results[, setdiff(names(modern_s1$results), c(new_cols, "converged"))],
    legacy$dfres[, setdiff(names(legacy$dfres), "converged")]
  )

  # fits coefficients identical too
  for (id in names(legacy$fits)) {
    if (!inherits(legacy$fits[[id]], "try-error")) {
      expect_identical(
        coef(modern_off$fits[[id]]),
        coef(legacy$fits[[id]])
      )
    }
  }

  expect_true(all(modern_off$results$n_starts_tried == 1L))
  expect_true(all(modern_s1$results$n_starts_tried == 1L))
})

test_that("(b) default budget leaves strict-converged subjects byte-identical (apt, hs)", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  base <- suppressWarnings(fit_demand_fixed(apt, equation = "hs", k = 2, multistart = FALSE))
  def <- suppressWarnings(fit_demand_fixed(apt, equation = "hs", k = 2))

  strict_ids <- base$results$id[isTRUE_vec(base$results$converged_strict)]
  expect_true(length(strict_ids) > 0)

  cmp_cols <- setdiff(names(base$results), c("n_starts_tried", "n_starts_converged", "start_source"))
  b <- base$results[base$results$id %in% strict_ids, cmp_cols]
  d <- def$results[def$results$id %in% strict_ids, cmp_cols]
  b <- b[order(b$id), ]
  d <- d[order(d$id), ]
  rownames(b) <- NULL
  rownames(d) <- NULL
  expect_identical(b, d)

  expect_true(all(def$results$start_source[def$results$id %in% strict_ids] == "production"))
})

test_that("(b) default budget leaves strict-converged subjects byte-identical (apt_full, koff)", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")

  base <- suppressWarnings(fit_demand_fixed(apt_full, equation = "koff", k = 2, multistart = FALSE))
  def <- suppressWarnings(fit_demand_fixed(apt_full, equation = "koff", k = 2))

  strict_ids <- base$results$id[isTRUE_vec(base$results$converged_strict)]
  expect_true(length(strict_ids) > 0)

  cmp_cols <- setdiff(names(base$results), c("n_starts_tried", "n_starts_converged", "start_source"))
  b <- base$results[base$results$id %in% strict_ids, cmp_cols]
  d <- def$results[def$results$id %in% strict_ids, cmp_cols]
  b <- b[order(b$id), ]
  d <- d[order(d$id), ]
  rownames(b) <- NULL
  rownames(d) <- NULL
  expect_identical(b, d)

  expect_true(all(def$results$start_source[def$results$id %in% strict_ids] == "production"))
})

test_that("(c) a production-failure fixture is rescued under the default budget", {
  skip_on_cran()
  # Deterministic simplified-equation data with a deliberately terrible
  # production start (passed through `...`) that makes the single-start
  # protocol fail entirely.
  q0_true <- 10
  alpha_true <- 0.05
  x <- c(0, 0.5, 1, 2, 4, 8, 16)
  y <- round(q0_true * exp(-alpha_true * q0_true * x), 4)
  d <- data.frame(id = "s1", x = x, y = y)

  off <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", startq0 = 1e-8, startalpha = 500, multistart = FALSE
  ))
  expect_false(isTRUE(off$results$converged_strict))

  set.seed(1)
  rescued <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", startq0 = 1e-8, startalpha = 500
  ))
  expect_true(isTRUE(rescued$results$converged_strict))
  expect_identical(rescued$results$start_source, "sampled")
  expect_equal(rescued$results$Q0d, q0_true, tolerance = 1e-2)
  expect_equal(rescued$results$Alpha, alpha_true, tolerance = 1e-2)
  expect_true(rescued$results$n_starts_tried > 1L)
})

test_that("(d) at-bound fits are rejected as rescue candidates (bound still binds every sample)", {
  skip_on_cran()
  q0_true <- 10
  alpha_true <- 0.05
  x <- c(0, 0.5, 1, 2, 4, 8, 16)
  y <- round(q0_true * exp(-alpha_true * q0_true * x), 4)
  d <- data.frame(id = "s1", x = x, y = y)

  # true alpha (0.05) is above the supplied upper bound (0.03); the
  # production fit converges numerically AT the bound (converged_strict
  # FALSE). No sampled start can escape the same bound.
  set.seed(1)
  bounded <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified",
    lobound = c(q0 = -Inf, alpha = -Inf),
    hibound = c(q0 = Inf, alpha = 0.03)
  ))
  expect_false(isTRUE(bounded$results$converged_strict))
  expect_identical(bounded$results$start_source, "none")
  expect_equal(bounded$results$n_starts_converged, 0L)
  expect_equal(bounded$results$Alpha, 0.03, tolerance = 1e-8)
})

test_that("(e) seed determinism: identical seeds give identical results, different seeds differ only for rescued subjects", {
  skip_on_cran()
  q0_true <- 10
  alpha_true <- 0.05
  x <- c(0, 0.5, 1, 2, 4, 8, 16)
  y <- round(q0_true * exp(-alpha_true * q0_true * x), 4)
  hard <- data.frame(id = "hard", x = x, y = y)
  easy <- data.frame(
    id = "easy", x = x,
    y = round(15 * exp(-0.02 * 15 * x), 4)
  )
  d <- rbind(hard, easy)

  set.seed(1)
  r1 <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", startq0 = 1e-8, startalpha = 500
  ))
  set.seed(1)
  r2 <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", startq0 = 1e-8, startalpha = 500
  ))
  expect_identical(r1$results, r2$results)

  set.seed(2)
  r3 <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", startq0 = 1e-8, startalpha = 500
  ))
  expect_false(identical(r1$results, r3$results))

  # "easy" subject's production start is the SAME terrible start as "hard"
  # -- both are expected to need rescue here since startq0/startalpha are
  # shared across all subjects in a single FitCurves() call. Just confirm
  # any differences between seeds are confined to rows that were actually
  # rescued (start_source == "sampled" in at least one of the two runs).
  diff_rows <- which(!mapply(identical, split(r1$results, seq_len(nrow(r1$results))),
                              split(r3$results, seq_len(nrow(r3$results)))))
  if (length(diff_rows) > 0) {
    expect_true(all(
      r1$results$start_source[diff_rows] == "sampled" |
        r3$results$start_source[diff_rows] == "sampled"
    ))
  }
})

test_that("(f) equation = 'linear' never multistarts", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  sub <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- suppressWarnings(fit_demand_fixed(sub, equation = "linear"))
  expect_true(all(fit$results$n_starts_tried == 1L))
  expect_false(fit$multistart$eligible)
  expect_identical(fit$multistart$S, 1L)

  # multistart = TRUE / large S must not change anything for linear
  fit2 <- suppressWarnings(fit_demand_fixed(sub, equation = "linear", multistart = TRUE, S = 32))
  expect_true(all(fit2$results$n_starts_tried == 1L))
  expect_identical(
    fit$results[, setdiff(names(fit$results), c("n_starts_tried", "n_starts_converged", "start_source"))],
    fit2$results[, setdiff(names(fit2$results), c("n_starts_tried", "n_starts_converged", "start_source"))]
  )
})

test_that("(g) mapping round-trip: sampled (Q0, Pmax) recovers Pmax via beezdemand_calc_pmax_omax()", {
  skip_on_cran()
  set.seed(123)
  q0 <- exp(runif(20, log(1), log(80)))
  pmax <- exp(runif(20, log(0.05), log(40)))

  # hs and koff share the Lambert-W closed form (verified against the
  # engine's .pmax_analytic_hs()); k must clear the existence threshold.
  k_nat <- 2.5
  for (eq in c("hs", "koff")) {
    alpha <- beezdemand:::.fixed_multistart_qp_to_alpha(eq, k_nat, q0, pmax)
    for (i in seq_along(q0)) {
      r <- beezdemand_calc_pmax_omax(
        model_type = eq,
        params = list(alpha = alpha[i], q0 = q0[i], k = k_nat)
      )
      expect_equal(r$pmax_model, pmax[i], tolerance = 1e-8)
    }
  }

  # simplified/SND
  alpha_snd <- beezdemand:::.fixed_multistart_qp_to_alpha("simplified", NA_real_, q0, pmax)
  for (i in seq_along(q0)) {
    r <- beezdemand_calc_pmax_omax(
      model_type = "snd",
      params = list(alpha = alpha_snd[i], q0 = q0[i])
    )
    expect_equal(r$pmax_model, pmax[i], tolerance = 1e-8)
  }
})

test_that("default budget is tiered by parameter count", {
  skip_on_cran()
  expect_identical(beezdemand:::.fixed_multistart_default_S("hs", 2), 8L)
  expect_identical(beezdemand:::.fixed_multistart_default_S("koff", 2), 8L)
  expect_identical(beezdemand:::.fixed_multistart_default_S("simplified", NA_real_), 8L)
  expect_identical(beezdemand:::.fixed_multistart_default_S("hs", "fit"), 32L)
  expect_identical(beezdemand:::.fixed_multistart_default_S("koff", "fit"), 32L)
  expect_identical(beezdemand:::.fixed_multistart_default_S("linear", 2), 1L)
})

test_that("FitCurves() is untouched by the multi-start protocol", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  legacy <- suppressWarnings(suppressMessages(FitCurves(apt, equation = "hs", k = 2)))
  expect_false(any(c("n_starts_tried", "n_starts_converged", "start_source") %in% names(legacy)))
})

# =============================================================================
# Codex 2F review fold (TICKET-047): blocking + recommended + optional items
# =============================================================================

test_that("(h) item 1: positional calls with an arg after `by` are unaffected", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  # Pre-fold, `multistart`/`S` were inserted BEFORE `by`, so this 9th
  # positional argument ("gender") would have bound to `multistart` instead
  # of `by`, silently making the fit ungrouped.
  fit <- suppressWarnings(fit_demand_fixed(
    apt_full, "hs", 2, NULL, "x", "y", "id", "natural", "gender"
  ))
  expect_s3_class(fit, "beezdemand_fixed_grouped")
  expect_identical(fit$by_var, "gender")
  expect_true(length(fit$groups) >= 2)
})

test_that("(i) item 3: k = 'range' rescue reuses the production dataset-wide K (no per-subject recompute)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  sub <- apt[apt$id %in% c(19, 30, 38), ]

  base <- suppressWarnings(fit_demand_fixed(sub, equation = "koff", k = "range", multistart = FALSE))
  k_production <- unique(round(base$results$K, 8))
  expect_length(k_production, 1)

  set.seed(1)
  rescued <- suppressWarnings(fit_demand_fixed(
    sub, equation = "koff", k = "range", startq0 = 1e-8, startalpha = 500
  ))
  expect_true(any(rescued$results$start_source == "sampled"))
  # every row (production or rescued) must share the SAME dataset-wide K --
  # a per-subject GetK() recompute would give a different value.
  expect_equal(unique(round(rescued$results$K, 8)), k_production)
})

test_that("(j) item 3: k = 'share' rescue reuses the production shared K (no crash)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  sub <- apt[apt$id %in% c(19, 30, 38), ]

  base <- suppressWarnings(fit_demand_fixed(sub, equation = "koff", k = "share", multistart = FALSE))
  k_production <- unique(round(base$results$K[is.finite(base$results$K)], 8))
  expect_length(k_production, 1)

  set.seed(1)
  rescued <- suppressWarnings(fit_demand_fixed(
    sub, equation = "koff", k = "share", startq0 = 1e-8, startalpha = 500
  ))
  # GetSharedK() hard-stops on a single-subject dataset ("Cannot find a
  # shared k value with only one dataset!"); pre-fold this made every
  # rescue attempt error out (silently, via try()), so nothing ever got
  # rescued even though `k` was perfectly well-defined at the dataset
  # level. Post-fold, the rescue reuses the resolved production K.
  expect_true(any(rescued$results$start_source == "sampled"))
  rescued_k <- unique(round(rescued$results$K[is.finite(rescued$results$K)], 8))
  expect_equal(rescued_k, k_production)
})

test_that("(k) item 3: agg = 'pooled' rescue does not crash (GetEmpirical duplicate-id guard)", {
  skip_on_cran()
  q0_true <- 10
  alpha_true <- 0.05
  x <- c(0, 0.5, 1, 2, 4, 8, 16)
  y <- round(q0_true * exp(-alpha_true * q0_true * x), 4)
  d <- rbind(
    data.frame(id = "s1", x = x, y = y),
    data.frame(id = "s2", x = x, y = y)
  )

  set.seed(1)
  rescued <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", agg = "pooled", startq0 = 1e-8, startalpha = 500
  ))
  expect_identical(nrow(rescued$results), 1L)
  expect_true(isTRUE(rescued$results$converged_strict))
  expect_identical(rescued$results$start_source, "sampled")
  expect_equal(rescued$results$Q0d, q0_true, tolerance = 1e-2)
})

test_that("(l) item 4: rescue candidates must be domain-valid (Q0 > 0, Alpha > 0)", {
  skip_on_cran()
  # The TICKET-069 pathological fixture (26 orders of magnitude, wildly
  # non-monotonic): pre-fold, the default multistart budget could "rescue"
  # this to a numerically strict-converged but domain-invalid (negative
  # alpha) fit. Post-fold, domain-invalid candidates are never accepted, so
  # this subject stays non-converged under the DEFAULT protocol (no
  # multistart = FALSE workaround needed).
  d <- data.frame(
    id = rep(c("s1", "s2"), each = 6),
    x  = rep(c(0, 0.5, 1, 2, 4, 8), 2),
    y  = c(10, 8, 6, 4, 2, 1, 4e8, 1e7, 5e5, 1e13, 2e27, 60)
  )
  f <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", x_var = "x", y_var = "y", id_var = "id"
  ))
  s2 <- f$results[f$results$id == "s2", ]
  expect_false(isTRUE(s2$converged))
  expect_false(isTRUE(s2$converged_strict))
})

test_that("(m) item 6: S is validated (single finite integer >= 1)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  sub <- apt[apt$id %in% c(19, 30), ]

  expect_error(
    fit_demand_fixed(sub, equation = "hs", k = 2, S = NA),
    "S"
  )
  expect_error(
    fit_demand_fixed(sub, equation = "hs", k = 2, S = 1.9),
    "S"
  )
  expect_error(
    fit_demand_fixed(sub, equation = "hs", k = 2, S = c(2, 3)),
    "S"
  )
  expect_error(
    fit_demand_fixed(sub, equation = "hs", k = 2, S = 0),
    "S"
  )
  # valid values do not error
  expect_no_error(fit_demand_fixed(sub, equation = "hs", k = 2, S = 4))
})

test_that("(n) item 5: low-k mapping is an explicit stochastic sampler, not a silent SND reuse", {
  skip_on_cran()
  q0 <- c(5, 10, 20)
  pmax <- c(1, 2, 4)
  k_low <- 1  # below exp(1)/log(10) ~= 1.18 -- no real interior Pmax for hs/koff

  set.seed(11)
  a1 <- beezdemand:::.fixed_multistart_qp_to_alpha("hs", k_low, q0, pmax)
  expect_true(all(is.finite(a1)))
  expect_true(all(a1 > 0))

  # Deterministic SND-style reuse would give IDENTICAL output regardless of
  # RNG state; the fix draws alpha independently, so a different seed must
  # give a DIFFERENT result.
  set.seed(22)
  a2 <- beezdemand:::.fixed_multistart_qp_to_alpha("hs", k_low, q0, pmax)
  expect_false(isTRUE(all.equal(a1, a2)))

  # And it must not silently equal the plain SND point formula either.
  snd_formula <- 1 / (pmax * q0)
  expect_false(isTRUE(all.equal(a1, snd_formula)))
})
