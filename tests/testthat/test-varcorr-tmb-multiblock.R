# Regression tests for the v0.3.0 release audit (Codex finding C3).
#
# VarCorr.beezdemand_tmb() placed each random-effect correlation on the row of
# its higher-indexed RE using indices LOCAL to its pdBlocked block, ignoring the
# block's global row offset. For the canonical M1 spec (pdSymm block FIRST) the
# offset is 0 so the result was already correct; the bug bit only when a
# correlated (pdSymm) block was NOT the first block. The fix derives positions
# from the same block map summary() uses, adding each block's global offset.
#
# The placement math is pinned with fast, deterministic unit tests on the pure
# helper .tmb_varcorr_corr_positions(); an integration test guards the common
# single-block 2-RE pdSymm path end-to-end.

# --- Unit tests: global correlation positions from a block map ----------------

test_that(".tmb_varcorr_corr_positions places a single 2-RE pdSymm block at (2,1)", {
  bmap <- list(n_blocks = 1L, block_q0_dim = 1L, block_alpha_dim = 1L,
               block_types = 1L)
  expect_equal(beezdemand:::.tmb_varcorr_corr_positions(bmap), list(c(2L, 1L)))
})

test_that(".tmb_varcorr_corr_positions: M1 (pdSymm first, pdDiag second) -> (2,1) only", {
  # Block 1 pdSymm (Q0+alpha intercepts), block 2 pdDiag (no off-diagonals).
  bmap <- list(n_blocks = 2L, block_q0_dim = c(1L, 1L),
               block_alpha_dim = c(1L, 1L), block_types = c(1L, 2L))
  expect_equal(beezdemand:::.tmb_varcorr_corr_positions(bmap), list(c(2L, 1L)))
})

test_that(".tmb_varcorr_corr_positions: pdSymm as the SECOND block gets the global offset (BUG C3)", {
  # Block 1 pdDiag (2 REs, no corr) -> offset 2; block 2 pdSymm (2 REs).
  # Correct global placement is (4, 3); the old local-index code gave (2, 1).
  bmap <- list(n_blocks = 2L, block_q0_dim = c(1L, 1L),
               block_alpha_dim = c(1L, 1L), block_types = c(2L, 1L))
  expect_equal(beezdemand:::.tmb_varcorr_corr_positions(bmap), list(c(4L, 3L)))
})

test_that(".tmb_varcorr_corr_positions: d=3 pdSymm block emits all 3 off-diagonals in order", {
  bmap <- list(n_blocks = 1L, block_q0_dim = 2L, block_alpha_dim = 1L,
               block_types = 1L)
  expect_equal(
    beezdemand:::.tmb_varcorr_corr_positions(bmap),
    list(c(2L, 1L), c(3L, 1L), c(3L, 2L))
  )
})

test_that(".tmb_varcorr_corr_positions: all-pdDiag structure yields no positions", {
  bmap <- list(n_blocks = 2L, block_q0_dim = c(1L, 1L),
               block_alpha_dim = c(1L, 1L), block_types = c(2L, 2L))
  expect_equal(beezdemand:::.tmb_varcorr_corr_positions(bmap), list())
})

# --- Integration: single-block 2-RE pdSymm path unchanged (regression guard) --

test_that("VarCorr() Corr column matches summary()$correlations on the alpha row (2-RE pdSymm)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    covariance_structure = "pdSymm", verbose = 0
  )

  vc <- VarCorr(fit)
  expect_true("Corr" %in% colnames(vc))

  alpha_rows <- grep("^alpha", rownames(vc))
  expect_length(alpha_rows, 1L)
  corr_val <- suppressWarnings(as.numeric(vc[alpha_rows[1], "Corr"]))

  s_corr <- summary(fit)$correlations
  skip_if(is.null(s_corr) || nrow(s_corr) == 0L, "fit produced no correlations")
  expect_equal(corr_val, signif(s_corr$Estimate[1], 3), tolerance = 1e-6)
})
