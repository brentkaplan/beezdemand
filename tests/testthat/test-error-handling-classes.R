# TICKET-009: regression tests for the structured error class system. The
# package exposes three error helpers in R/utils.R that wrap cli::cli_abort
# with package-specific class tags, so callers can use
# `tryCatch(expr, beezdemand_validation_error = ...)` for programmatic
# handling. These tests pin the contract.

# --- validation_error class --------------------------------------------------

test_that("ll4() warns via rlang on negative values", {
  # ll4 emits a cli::cli_warn — the resulting warning carries rlang's class.
  expect_warning(ll4(c(-1, 1, 10)), "negative")
})

test_that("check_unsystematic_cp() rejects non-data.frame input via validation_error", {
  expect_error(
    check_unsystematic_cp(list(x = 1:5, y = 1:5)),
    class = "beezdemand_validation_error"
  )
})

test_that("check_unsystematic_cp() rejects too-few rows via validation_error", {
  expect_error(
    check_unsystematic_cp(data.frame(x = 1:2, y = 1:2)),
    class = "beezdemand_validation_error"
  )
})

test_that("get_empirical_measures() rejects non-data.frame via validation_error", {
  expect_error(
    get_empirical_measures(c(1, 2, 3)),
    class = "beezdemand_validation_error"
  )
})

test_that("get_empirical_measures() rejects duplicate prices via validation_error", {
  dup <- data.frame(id = rep("S1", 4), x = c(1, 2, 2, 3),
                    y = c(10, 8, 5, 2))
  expect_error(
    get_empirical_measures(dup),
    class = "beezdemand_validation_error"
  )
})

# --- missing_package_error class --------------------------------------------

test_that("missing_package_error returns the documented class hierarchy", {
  err <- tryCatch(
    missing_package_error("nonexistent_pkg", reason = "for testing"),
    error = function(e) e
  )
  expect_s3_class(err, "beezdemand_missing_package")
  expect_s3_class(err, "beezdemand_error")
})

# --- fitting_error class ----------------------------------------------------

test_that("fitting_error returns the documented class hierarchy", {
  err <- tryCatch(
    fitting_error("simulated convergence failure", model_type = "tmb"),
    error = function(e) e
  )
  expect_s3_class(err, "beezdemand_fitting_error")
  expect_s3_class(err, "beezdemand_error")
})

# --- cli interpolation through helpers --------------------------------------

test_that("validation_error renders cli inline markup from the caller's frame", {
  # This is the contract that broke once during the migration: cli::cli_abort
  # interpolates {} expressions in *its* caller's environment, so the helper
  # must thread the original call site's frame through .envir for messages
  # like "{x[i]}" to resolve.
  helper_with_local_var <- function() {
    target <- "specific_value"
    validation_error("Bad target: {.val {target}}.")
  }
  err <- tryCatch(helper_with_local_var(), error = function(e) e)
  expect_s3_class(err, "beezdemand_validation_error")
  expect_match(conditionMessage(err), "specific_value")
})
