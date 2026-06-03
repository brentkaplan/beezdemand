# Independent verification of lambertW() (R/utils.R) against external invariants.
#
# lambertW() is Ben Bolker's GSL port (principal branch b = 0, Halley iteration).
# It underpins every analytic Pmax (HS plus the hurdle variants) in
# pmax-omax-engine.R, yet had no isolated test. These checks anchor to the
# DEFINING IDENTITY W(z) * exp(W(z)) == z and to published constants, so the
# expected values never come from lambertW() itself (non-circular).

test_that("lambertW matches known principal-branch constants", {
  expect_equal(lambertW(0), 0, tolerance = 1e-9)
  expect_equal(lambertW(exp(1)), 1, tolerance = 1e-9) # W(e) = 1
  expect_equal(lambertW(1), 0.5671432904097838, tolerance = 1e-9) # Omega
  expect_equal(lambertW(-exp(-1)), -1, tolerance = 1e-7) # branch point W(-1/e) = -1
})

test_that("lambertW satisfies the defining identity w*exp(w) == z on a grid", {
  # Positive z spanning the small-z and large-z asymptotic regimes.
  z_pos <- c(1e-3, 1e-2, 0.1, 0.5, 1, exp(1), 3, 5, 10, 25, 50)
  # The exact negative arguments the Pmax solver feeds in (all > -1/e):
  #   HS:     -1/(k*ln10) for k > e/ln10
  #   hurdle: -1/k        for k > e
  z_hs <- -1 / (c(1.2, 2, 3, 5) * log(10))
  z_hu <- -1 / c(2.8, 3, 5, 10)
  z <- c(z_pos, z_hs, z_hu)

  w <- vapply(z, lambertW, numeric(1))
  expect_true(all(is.finite(w)))
  expect_equal(w * exp(w), z, tolerance = 1e-8)
})

test_that("lambertW returns the principal branch W0 (not W_-1) on (-1/e, 0)", {
  # The identity alone cannot separate W0 from W_-1 on [-1/e, 0); pin the branch.
  # W0 maps (-1/e, 0) -> (-1, 0) monotonically increasing; W_-1 maps it to
  # (-Inf, -1). Selecting the wrong branch would silently break every Pmax.
  z <- seq(-exp(-1) + 1e-6, -1e-6, length.out = 60)
  w <- vapply(z, lambertW, numeric(1))
  expect_true(all(w >= -1 & w < 0))
  expect_true(all(diff(w) > 0))
})

test_that("lambertW does not hit the iteration limit for in-range arguments", {
  z <- c(
    1e-3, 0.5, 1, exp(1), 50,
    -1 / (c(1.2, 2, 3, 5) * log(10)),
    -1 / c(2.8, 3, 5, 10)
  )
  expect_no_warning(vapply(z, lambertW, numeric(1)))
})

test_that("lambertW is vectorized consistently with scalar calls", {
  z <- c(0.5, 1, exp(1), 5)
  expect_equal(lambertW(z), vapply(z, lambertW, numeric(1)), tolerance = 1e-10)
})
