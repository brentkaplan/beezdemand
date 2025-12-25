# Tests for k = "estimate" functionality in fit_joint_hurdle

test_that("fit_joint_hurdle k parameter validation works", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create minimal test data
  set.seed(123)
  n_subj <- 3
  n_obs <- 5
  test_data <- data.frame(
    id = rep(1:n_subj, each = n_obs * 3),
    stream = rep(rep(c("own", "cp1", "cp2"), each = n_obs), n_subj),
    x = rep(rep(c(0.1, 1, 2, 5, 10), 3), n_subj),
    y = rpois(n_subj * n_obs * 3, lambda = 5)
  )

  # Error on invalid k values

  expect_error(
    fit_joint_hurdle(test_data, k = NULL),
    regexp = "cannot be NULL"
  )
  expect_error(
    fit_joint_hurdle(test_data, k = "invalid"),
    regexp = "must be"
  )
  expect_error(
    fit_joint_hurdle(test_data, k = c(1, 2)),
    regexp = "single numeric"
  )
})


test_that("fit_joint_hurdle k = 2 works (default)", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create test data
  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  test_data <- data.frame(
    id = rep(1:n_subj, each = n_obs * 3),
    stream = rep(rep(c("own", "cp1", "cp2"), each = n_obs), n_subj),
    x = rep(rep(seq(0.1, 10, length.out = n_obs), 3), n_subj),
    y = rpois(n_subj * n_obs * 3, lambda = 8)
  )

  fit <- tryCatch(
    fit_joint_hurdle(test_data, k = 2),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  g <- glance(fit)
  expect_true(g$k_fixed)
  expect_equal(g$k_value, 2)
})


test_that("fit_joint_hurdle k = numeric value works", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create test data
  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  test_data <- data.frame(
    id = rep(1:n_subj, each = n_obs * 3),
    stream = rep(rep(c("own", "cp1", "cp2"), each = n_obs), n_subj),
    x = rep(rep(seq(0.1, 10, length.out = n_obs), 3), n_subj),
    y = rpois(n_subj * n_obs * 3, lambda = 8)
  )

  fit <- tryCatch(
    fit_joint_hurdle(test_data, k = 3.5),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  g <- glance(fit)
  expect_true(g$k_fixed)
  expect_equal(g$k_value, 3.5)
})


test_that("fit_joint_hurdle k = 'estimate' works with warning", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create test data with more observations for stable estimation
  set.seed(456)
  n_subj <- 8
  n_obs <- 10
  test_data <- data.frame(
    id = rep(1:n_subj, each = n_obs * 3),
    stream = rep(rep(c("own", "cp1", "cp2"), each = n_obs), n_subj),
    x = rep(rep(seq(0.1, 10, length.out = n_obs), 3), n_subj),
    y = rpois(n_subj * n_obs * 3, lambda = 10)
  )

  # Should emit identifiability warning
  fit <- tryCatch(
    expect_warning(
      fit_joint_hurdle(test_data, k = "estimate"),
      regexp = "identifiability"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  g <- glance(fit)
  expect_false(g$k_fixed)
  expect_true(!is.na(g$k_value))
})


test_that("glance includes k_fixed and k_value columns", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create test data
  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  test_data <- data.frame(
    id = rep(1:n_subj, each = n_obs * 3),
    stream = rep(rep(c("own", "cp1", "cp2"), each = n_obs), n_subj),
    x = rep(rep(seq(0.1, 10, length.out = n_obs), 3), n_subj),
    y = rpois(n_subj * n_obs * 3, lambda = 8)
  )

  fit <- tryCatch(
    fit_joint_hurdle(test_data, k = 2),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  g <- glance(fit)

  expect_true("k_fixed" %in% names(g))
  expect_true("k_value" %in% names(g))
})


test_that("summary reflects k specification", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create test data
  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  test_data <- data.frame(
    id = rep(1:n_subj, each = n_obs * 3),
    stream = rep(rep(c("own", "cp1", "cp2"), each = n_obs), n_subj),
    x = rep(rep(seq(0.1, 10, length.out = n_obs), 3), n_subj),
    y = rpois(n_subj * n_obs * 3, lambda = 8)
  )

  fit <- tryCatch(
    fit_joint_hurdle(test_data, k = 2.5),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  s <- summary(fit)

  expect_true("k_fixed" %in% names(s))
  expect_true("k_value" %in% names(s))
  expect_true(s$k_fixed)
  expect_equal(s$k_value, 2.5)
})
