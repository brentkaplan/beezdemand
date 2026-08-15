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
