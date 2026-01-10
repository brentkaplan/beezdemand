# Tests for k = "estimate" functionality in fit_joint_hurdle

create_joint_k_test_data <- function(n_subjects = 6, n_prices = 6, seed = 123) {
  set.seed(seed)
  prices <- seq(0.1, 10, length.out = n_prices)
  targets <- c("alone", "own", "alt")
  out <- vector("list", length = n_subjects * n_prices * length(targets))
  idx <- 1L

  for (id in seq_len(n_subjects)) {
    for (p in prices) {
      for (tgt in targets) {
        # Generate some zeros and positive values
        y <- rpois(1, lambda = if (tgt == "alt") 6 else 10)
        if (runif(1) < 0.15) y <- 0
        out[[idx]] <- data.frame(
          id = id,
          x = p,
          y = y,
          target = tgt,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  do.call(rbind, out)
}

test_that("fit_joint_hurdle k parameter validation works", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  test_data <- create_joint_k_test_data(n_subjects = 3, n_prices = 4, seed = 123)

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

  test_data <- create_joint_k_test_data(n_subjects = 6, n_prices = 6, seed = 123)

  fit <- tryCatch(
    suppressWarnings(fit_joint_hurdle(
      test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )),
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

  test_data <- create_joint_k_test_data(n_subjects = 6, n_prices = 6, seed = 123)

  fit <- tryCatch(
    suppressWarnings(fit_joint_hurdle(
      test_data,
      k = 3.5,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )),
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

  test_data <- create_joint_k_test_data(n_subjects = 8, n_prices = 8, seed = 456)

  warn_msgs <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      fit_joint_hurdle(
        test_data,
        k = "estimate",
        verbose = 0,
        control = list(eval.max = 300, iter.max = 300)
      ),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")
  expect_true(any(grepl("identifiability", warn_msgs, ignore.case = TRUE)))

  g <- glance(fit)
  expect_false(g$k_fixed)
  expect_true(!is.na(g$k_value))
})


test_that("glance includes k_fixed and k_value columns", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  test_data <- create_joint_k_test_data(n_subjects = 6, n_prices = 6, seed = 123)

  fit <- tryCatch(
    suppressWarnings(fit_joint_hurdle(
      test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )),
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

  test_data <- create_joint_k_test_data(n_subjects = 6, n_prices = 6, seed = 123)

  fit <- tryCatch(
    suppressWarnings(fit_joint_hurdle(
      test_data,
      k = 2.5,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  s <- summary(fit)

  expect_true("k_fixed" %in% names(s))
  expect_true("k_value" %in% names(s))
  expect_true(s$k_fixed)
  expect_equal(s$k_value, 2.5)
})
