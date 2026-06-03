# Independent verification that the 3-RE hurdle correlation parameterization
# yields a valid (positive-definite) 3x3 correlation matrix for all real-valued
# raw parameters. The construction mirrors R/hurdle-demand.R:1003-1013 and
# src/HurdleDemand3RE.h (confirmed bit-identical during the audit):
#
#   rho_ab = tanh(rho_ab_raw); rho_ac = tanh(rho_ac_raw)
#   rho_bc = rho_ab*rho_ac + tanh(rho_bc_raw)*sqrt((1-rho_ab^2)(1-rho_ac^2))
#
# Positive-definiteness is checked INDEPENDENTLY via eigen() (never assumed).
# This is the partial-correlation construction that guarantees a PD matrix for
# any finite raws; it only approaches the PSD boundary as |raw| -> Inf.

.audit_build_3re_corr <- function(rab_raw, rac_raw, rbc_raw) {
  rho_ab <- tanh(rab_raw)
  rho_ac <- tanh(rac_raw)
  rho_bc <- rho_ab * rho_ac +
    tanh(rbc_raw) * sqrt((1 - rho_ab^2) * (1 - rho_ac^2))
  matrix(
    c(
      1, rho_ab, rho_ac,
      rho_ab, 1, rho_bc,
      rho_ac, rho_bc, 1
    ),
    nrow = 3, byrow = TRUE
  )
}

test_that("3-RE correlation construction is never indefinite over a random grid", {
  set.seed(20260603)
  raws <- matrix(stats::runif(3 * 4000, min = -5, max = 5), ncol = 3)
  min_eig <- apply(raws, 1, function(r) {
    R <- .audit_build_3re_corr(r[1], r[2], r[3])
    min(eigen(R, symmetric = TRUE, only.values = TRUE)$values)
  })
  # The parameterization guarantees a valid (>= PSD) correlation matrix for all
  # real raws, approaching singular only as |raw| -> Inf. It must NEVER be
  # indefinite (a wrong construction would give negative eigenvalues here).
  expect_true(all(min_eig > -1e-8))
})

test_that("3-RE correlation is strictly positive-definite in the typical region", {
  set.seed(11)
  raws <- matrix(stats::runif(3 * 2000, min = -2.5, max = 2.5), ncol = 3)
  min_eig <- apply(raws, 1, function(r) {
    R <- .audit_build_3re_corr(r[1], r[2], r[3])
    min(eigen(R, symmetric = TRUE, only.values = TRUE)$values)
  })
  expect_true(all(min_eig > 1e-8))
})

test_that("3-RE construction keeps rho_bc within [-1, 1] including at boundaries", {
  grid <- expand.grid(
    a = c(-5, -1, 0, 1, 5),
    b = c(-5, -1, 0, 1, 5),
    c = c(-5, -1, 0, 1, 5)
  )
  rho_bc <- mapply(
    function(a, b, cc) {
      rho_ab <- tanh(a)
      rho_ac <- tanh(b)
      rho_ab * rho_ac + tanh(cc) * sqrt((1 - rho_ab^2) * (1 - rho_ac^2))
    },
    grid$a, grid$b, grid$c
  )
  expect_true(all(rho_bc >= -1 - 1e-12 & rho_bc <= 1 + 1e-12))
})

test_that("when rho_ab = rho_ac = 0, rho_bc reduces to tanh(rho_bc_raw)", {
  R <- .audit_build_3re_corr(0, 0, 0.8)
  expect_equal(R[2, 3], tanh(0.8), tolerance = 1e-12)
})
