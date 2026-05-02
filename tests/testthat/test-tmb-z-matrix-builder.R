# =============================================================================
# TICKET-011 Phase 2.3: R-side Z-matrix and block-map builders for the
# generalized TMB template (Phase 2.4). These helpers consume the canonical
# block representation produced by .normalize_re_input() (Phase 1) and
# emit the design matrices and metadata vectors the new
# src/MixedDemand.h DATA interface will consume.
#
# Conventions locked in by these tests:
#   * Z_q0 has one column per term in the (concatenated) terms_q0 across
#     all blocks; same for Z_alpha. When a block contributes 0 q0 terms,
#     it adds 0 columns to Z_q0; likewise for alpha.
#   * block_types: 0 = pdDiag, 1 = pdSymm.
#   * Per-block "dim" for the within-block covariance is
#     block_q0_dim + block_alpha_dim (q0 and alpha REs share the block's
#     covariance).
#   * n_logsigma = sum of within-block dims (one logsigma per RE column).
#   * n_rho = sum over pdSymm blocks of dim*(dim-1)/2; pdDiag blocks
#     contribute 0.
# =============================================================================

helper_tiny_data <- function() {
  data.frame(
    id = factor(rep(1:4, each = 3)),
    condition = factor(rep(c("A", "B", "C"), 4),
                       levels = c("A", "B", "C")),
    x = rep(c(0.1, 1, 10), 4),
    y = runif(12, 0.1, 50)
  )
}

# -----------------------------------------------------------------------------
# .tmb_build_z_matrices(re_parsed, data, id_var)
# -----------------------------------------------------------------------------

test_that("Z matrices are columns of 1s for intercept-only c('q0','alpha')", {
  d <- helper_tiny_data()
  re_parsed <- .normalize_re_input(c("q0", "alpha"), covariance_structure = "pdSymm")
  z <- .tmb_build_z_matrices(re_parsed, d, id_var = "id")

  expect_equal(dim(z$Z_q0), c(nrow(d), 1L))
  expect_equal(dim(z$Z_alpha), c(nrow(d), 1L))
  expect_true(all(z$Z_q0 == 1))
  expect_true(all(z$Z_alpha == 1))
  expect_equal(z$re_dim_q0, 1L)
  expect_equal(z$re_dim_alpha, 1L)
})

test_that("Z_alpha has 0 columns when only q0 is RE", {
  d <- helper_tiny_data()
  re_parsed <- .normalize_re_input(c("q0"), covariance_structure = "pdSymm")
  z <- .tmb_build_z_matrices(re_parsed, d, id_var = "id")

  expect_equal(dim(z$Z_q0), c(nrow(d), 1L))
  expect_equal(dim(z$Z_alpha), c(nrow(d), 0L))
  expect_equal(z$re_dim_q0, 1L)
  expect_equal(z$re_dim_alpha, 0L)
})

test_that("pdDiag(Q0+alpha~condition) Z matrices match model.matrix(~condition)", {
  d <- helper_tiny_data()
  re_parsed <- .normalize_re_input(
    Q0 + alpha ~ condition,
    covariance_structure = "pdDiag",
    data = d
  )
  z <- .tmb_build_z_matrices(re_parsed, d, id_var = "id")

  # Treatment contrasts on 3 levels -> 3 columns each.
  expect_equal(dim(z$Z_q0), c(nrow(d), 3L))
  expect_equal(dim(z$Z_alpha), c(nrow(d), 3L))
  expect_equal(z$re_dim_q0, 3L)
  expect_equal(z$re_dim_alpha, 3L)

  # Z columns must reproduce the contrast pattern from `condition`:
  # row 1 -> A (intercept only), row 2 -> B, row 3 -> C, etc.
  expected_X <- stats::model.matrix(~ condition, data = d)
  expect_true(all(z$Z_q0 == expected_X))
  expect_true(all(z$Z_alpha == expected_X))
})

test_that("`~ condition - 1` produces one indicator column per level", {
  d <- helper_tiny_data()
  re_parsed <- .normalize_re_input(
    Q0 + alpha ~ condition - 1,
    covariance_structure = "pdDiag",
    data = d
  )
  z <- .tmb_build_z_matrices(re_parsed, d, id_var = "id")

  expect_equal(dim(z$Z_q0), c(nrow(d), 3L))
  expected_X <- stats::model.matrix(~ condition - 1, data = d)
  expect_true(all(z$Z_q0 == expected_X))
  # Each row sums to 1 across the three indicator columns.
  expect_true(all(rowSums(z$Z_q0) == 1))
})

# -----------------------------------------------------------------------------
# .tmb_build_block_map(re_parsed)
# -----------------------------------------------------------------------------

test_that("block map for intercept-only pdSymm(Q0+alpha~1)", {
  re_parsed <- .normalize_re_input(c("q0", "alpha"), covariance_structure = "pdSymm")
  m <- .tmb_build_block_map(re_parsed)

  expect_equal(m$n_blocks, 1L)
  expect_equal(m$block_q0_dim, 1L)
  expect_equal(m$block_alpha_dim, 1L)
  expect_equal(m$block_types, 1L)  # 1 = pdSymm
  expect_equal(m$block_q0_offset, 0L)
  expect_equal(m$block_alpha_offset, 0L)
  expect_equal(m$n_logsigma, 2L)   # one per RE column (q0 + alpha)
  expect_equal(m$n_rho, 1L)        # 2x2 symmetric -> 1 free correlation
})

test_that("block map for intercept-only pdDiag(Q0+alpha~1)", {
  re_parsed <- .normalize_re_input(Q0 + alpha ~ 1, covariance_structure = "pdDiag")
  m <- .tmb_build_block_map(re_parsed)

  expect_equal(m$n_blocks, 1L)
  expect_equal(m$block_q0_dim, 1L)
  expect_equal(m$block_alpha_dim, 1L)
  expect_equal(m$block_types, 0L)  # 0 = pdDiag
  expect_equal(m$n_logsigma, 2L)
  expect_equal(m$n_rho, 0L)        # diagonal -> no rhos
})

test_that("block map for q0-only c('q0')", {
  re_parsed <- .normalize_re_input(c("q0"), covariance_structure = "pdSymm")
  m <- .tmb_build_block_map(re_parsed)

  expect_equal(m$n_blocks, 1L)
  expect_equal(m$block_q0_dim, 1L)
  expect_equal(m$block_alpha_dim, 0L)
  # Single-RE block has dim 1 -> pdSymm = pdDiag (no off-diagonals); n_rho = 0.
  expect_equal(m$n_logsigma, 1L)
  expect_equal(m$n_rho, 0L)
})

test_that("block map for pdDiag(Q0+alpha~condition) with 3 levels", {
  d <- helper_tiny_data()
  re_parsed <- .normalize_re_input(
    Q0 + alpha ~ condition,
    covariance_structure = "pdDiag",
    data = d
  )
  m <- .tmb_build_block_map(re_parsed)

  expect_equal(m$n_blocks, 1L)
  expect_equal(m$block_q0_dim, 3L)
  expect_equal(m$block_alpha_dim, 3L)
  expect_equal(m$block_types, 0L)
  expect_equal(m$n_logsigma, 6L)
  expect_equal(m$n_rho, 0L)        # diagonal -> still 0
})

test_that("block map for pdSymm(Q0+alpha~condition) with 3 levels", {
  d <- helper_tiny_data()
  re_parsed <- .normalize_re_input(
    Q0 + alpha ~ condition,
    covariance_structure = "pdSymm",
    data = d
  )
  m <- .tmb_build_block_map(re_parsed)

  expect_equal(m$n_blocks, 1L)
  expect_equal(m$block_q0_dim, 3L)
  expect_equal(m$block_alpha_dim, 3L)
  expect_equal(m$block_types, 1L)
  expect_equal(m$n_logsigma, 6L)
  # 6x6 symmetric covariance -> 6*5/2 = 15 unique off-diagonals.
  expect_equal(m$n_rho, 15L)
})
