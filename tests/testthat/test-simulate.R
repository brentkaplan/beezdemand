# TICKET-060: GetValsForSim() misassigns/crashes on non-canonical price grids
# (positional residual assignment) and SimulateDemand() errors in a fresh
# session (.Random.seed). Neither function previously had test coverage.

mk_sim_subj <- function(id, q0 = 10, alpha = 0.01, k = 2,
                         x = c(0, 0.5, 1, 2, 4, 8)) {
  y <- q0 * 10^(k * (exp(-alpha * q0 * x) - 1))
  data.frame(id = id, x = x, y = round(y, 3))
}

# =============================================================================
# Positional residual assignment (batch fatality + silent misalignment)
# =============================================================================

test_that("TICKET-060: GetValsForSim tolerates a subject missing a price row", {
  skip_on_cran()
  full1 <- mk_sim_subj("v1")
  miss2 <- mk_sim_subj("v2")[-3, ]  # v2 lacks the x = 1 row

  result <- suppressWarnings(GetValsForSim(rbind(full1, miss2)))
  expect_type(result, "list")
  expect_equal(length(result$sdindex), length(result$x))
  # the missing price's sd is computed from only 1 subject's residual (v1);
  # sd() of a single value is NA, but it must not error and must not
  # silently misplace v1's residuals under the wrong price column.
  expect_true(is.numeric(result$sdindex))
})

test_that("Codex 2B fold: GetValsForSim places residuals in the exact price column (not just type/length)", {
  skip_on_cran()
  full1 <- mk_sim_subj("v1")
  miss2 <- mk_sim_subj("v2")[-3, ]  # v2 lacks the x = 1 row
  d <- rbind(full1, miss2)

  # Recompute the same per-subject fits independently (mirroring
  # GetValsForSim()'s own formula/start/k) to get an external, known-correct
  # residual-by-price mapping, then assert sdindex matches it exactly --
  # including NA at exactly the missing price's column and nowhere else.
  k <- log10(max(d[d$y > 0, "y"])) - log10(min(d[d$y > 0, "y"]))
  adf1 <- full1; adf1$k <- k
  adf2 <- miss2; adf2$k <- k
  fo <- y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1))
  fit1 <- nlsr::wrapnlsr(data = adf1, fo, start = list(q0 = 10, alpha = 0.01))
  fit2 <- nlsr::wrapnlsr(data = adf2, fo, start = list(q0 = 10, alpha = 0.01))
  res1 <- resid(fit1); names(res1) <- as.character(adf1$x)
  res2 <- resid(fit2); names(res2) <- as.character(adf2$x)

  prices <- unique(d$x)
  expected_sdindex <- vapply(prices, function(p) {
    p_chr <- as.character(p)
    vals <- c(
      if (p_chr %in% names(res1)) unname(res1[p_chr]) else NA_real_,
      if (p_chr %in% names(res2)) unname(res2[p_chr]) else NA_real_
    )
    sd(vals, na.rm = TRUE)
  }, numeric(1))

  result <- suppressWarnings(GetValsForSim(d))
  expect_equal(result$x, prices)
  expect_equal(result$sdindex, expected_sdindex, tolerance = 1e-6)
  # x = 1 is exactly the price v2 is missing -- and only that column is NA
  missing_idx <- which(prices == 1)
  expect_true(is.na(result$sdindex[missing_idx]))
  expect_true(all(!is.na(result$sdindex[-missing_idx])))
})

test_that("Codex 2B fold: GetValsForSim rejects duplicated within-subject prices instead of silently corrupting one residual cell", {
  skip_on_cran()
  full1 <- mk_sim_subj("v1")
  dup2 <- mk_sim_subj("v2")
  dup2$x[2] <- dup2$x[1]  # duplicate the first price (x = 0) onto row 2
  err <- tryCatch(
    GetValsForSim(rbind(full1, dup2)),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_true(grepl("v2", conditionMessage(err)))
  expect_true(grepl("0", conditionMessage(err)))
})

test_that("TICKET-060: GetValsForSim is invariant to within-subject row order", {
  skip_on_cran()
  d <- rbind(mk_sim_subj("v1"), mk_sim_subj("v2", q0 = 12))
  d_shuffled <- d
  idx_v2 <- which(d_shuffled$id == "v2")
  d_shuffled[idx_v2, ] <- d_shuffled[rev(idx_v2), ]

  r1 <- GetValsForSim(d)
  r2 <- GetValsForSim(d_shuffled)

  expect_equal(r1$setparams, r2$setparams, tolerance = 1e-8)
  expect_equal(r1$sdindex, r2$sdindex, tolerance = 1e-8)
})

test_that("TICKET-060: GetValsForSim warns (not silently) on a subject whose fit fails", {
  skip_on_cran()
  full1 <- mk_sim_subj("v1")
  onerow <- data.frame(id = "toofew", x = 0, y = 10)  # 1 row, 2 free params
  expect_warning(
    result <- GetValsForSim(rbind(full1, onerow)),
    "toofew"
  )
  expect_type(result, "list")
})

# =============================================================================
# Fresh-session .Random.seed fatality
# =============================================================================

test_that("TICKET-060: SimulateDemand works when .Random.seed does not yet exist", {
  skip_on_cran()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  if (had_seed) rm(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  setparams <- c(
    alphalm = -2.5547, alphalsd = .702521, q0lm = 1.239893,
    q0lsd = .320221, k = 3.096, yvalssd = 1.438231
  )
  sdindex <- c(2.1978, 1.9243, 1.5804, 1.2465, 0.8104, 0.1751, 0.0380, 0.0270)
  x <- c(.1, 1, 3, 10, 30, 100, 300, 1000)

  expect_no_error(
    sim <- SimulateDemand(nruns = 1, setparams = setparams, sdindex = sdindex, x = x)
  )
})
