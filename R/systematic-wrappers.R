#' @title Systematicity Check Wrappers
#' @description Modern wrappers for systematicity checks with unified output vocabulary.
#' @name systematic-wrappers
NULL

#' Create a beezdemand_systematicity Object
#'
#' @param results Tibble with standardized columns
#' @param type Character: "demand" or "cp"
#' @param call Original function call
#' @keywords internal
new_beezdemand_systematicity <- function(results, type, call) {
  structure(
    list(
      results = results,
      type = type,
      call = call,
      n_total = nrow(results),
      n_systematic = sum(results$systematic, na.rm = TRUE),
      n_unsystematic = sum(!results$systematic, na.rm = TRUE)
    ),
    class = c("beezdemand_systematicity", "list")
  )
}


#' Check Demand Data for Unsystematic Responding
#'
#' Modern interface for screening purchase task data using Stein et al. (2015)
#' criteria. Returns a structured object with standardized output vocabulary
#' that is consistent with `check_systematic_cp()`.
#'
#' @param data Data frame in long format with columns: `id`, `x` (price), `y` (consumption).
#' @param trend_threshold Numeric. Threshold for trend detection (log-log slope).
#'   Default `0.025`.
#' @param bounce_threshold Numeric. Threshold for bounce proportion. Default `0.10`.
#' @param max_reversals Integer. Maximum allowed reversals from zero. Default `0`.
#' @param consecutive_zeros Integer. Consecutive zeros required for reversal detection.
#'   Default `2` (per Stein et al. 2015).
#' @param x_var Character. Name of the price column. Default `"x"`.
#' @param y_var Character. Name of the consumption column. Default `"y"`.
#' @param id_var Character. Name of the subject identifier column. Default `"id"`.
#'
#' @return An object of class `beezdemand_systematicity` with components:
#'   \describe{
#'     \item{results}{Tibble with one row per subject containing systematicity metrics}
#'     \item{type}{"demand"}
#'     \item{call}{The original function call}
#'     \item{n_total}{Total number of subjects}
#'     \item{n_systematic}{Number of subjects passing all criteria}
#'     \item{n_unsystematic}{Number of subjects failing at least one criterion}
#'   }
#'
#' @details
#' The `results` tibble contains standardized columns for both demand and
#' cross-price systematicity checks:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{type}{"demand" for this function}
#'   \item{trend_stat}{DeltaQ statistic (log-log slope)}
#'   \item{trend_threshold}{Threshold used}
#'   \item{trend_direction}{"down", "up", or "none"}
#'   \item{trend_pass}{Logical: passed trend criterion}
#'   \item{bounce_stat}{Bounce proportion}
#'   \item{bounce_threshold}{Threshold used}
#'   \item{bounce_direction}{"significant" or "none"}
#'   \item{bounce_pass}{Logical: passed bounce criterion}
#'   \item{reversals}{Count of reversals from zero}
#'   \item{reversals_pass}{Logical: passed reversals criterion}
#'   \item{returns}{NA for demand (CP-specific)}
#'   \item{n_positive}{Count of positive values}
#'   \item{systematic}{Logical: passed all criteria}
#' }
#'
#' @examples
#' \dontrun{
#' data(apt)
#' check <- check_systematic_demand(apt)
#' print(check)
#' summary(check)
#' tidy(check)
#' }
#'
#' @export
check_systematic_demand <- function(data,
                                    trend_threshold = 0.025,
                                    bounce_threshold = 0.10,
                                    max_reversals = 0,
                                    consecutive_zeros = 2,
                                    x_var = "x",
                                    y_var = "y",
                                    id_var = "id") {
  call <- match.call()

  # Rename columns if needed
  if (x_var != "x" || y_var != "y" || id_var != "id") {
    data <- data.frame(
      id = data[[id_var]],
      x = data[[x_var]],
      y = data[[y_var]],
      stringsAsFactors = FALSE
    )
  }

  # Call legacy function
  legacy <- CheckUnsystematic(
    dat = data,
    deltaq = trend_threshold,
    bounce = bounce_threshold,
    reversals = max_reversals,
    ncons0 = consecutive_zeros
  )

  # Compute trend direction from sign of DeltaQ
  trend_direction <- dplyr::case_when(
    legacy$DeltaQ < -trend_threshold ~ "down",
    legacy$DeltaQ > trend_threshold ~ "up",
    TRUE ~ "none"
  )

  # Standardize output
  results <- tibble::tibble(
    id = as.character(legacy$id),
    type = "demand",
    trend_stat = legacy$DeltaQ,
    trend_threshold = trend_threshold,
    trend_direction = trend_direction,
    trend_pass = legacy$DeltaQPass == "Pass",
    bounce_stat = legacy$Bounce,
    bounce_threshold = bounce_threshold,
    bounce_direction = ifelse(legacy$Bounce > bounce_threshold, "significant", "none"),
    bounce_pass = legacy$BouncePass == "Pass",
    reversals = as.integer(legacy$Reversals),
    reversals_pass = legacy$ReversalsPass == "Pass",
    returns = NA_integer_,
    n_positive = as.integer(legacy$NumPosValues),
    systematic = legacy$TotalPass == 3
  )

  new_beezdemand_systematicity(results, type = "demand", call = call)
}


#' Check Cross-Price Data for Unsystematic Responding
#'
#' Modern interface for screening cross-price data with standardized output
#' vocabulary aligned with `check_systematic_demand()`.
#'
#' @param data Data frame with columns: `id` (optional), `x` (price), `y` (consumption).
#' @param trend_threshold Numeric. Threshold for trend detection. Default `0.025`.
#' @param bounce_threshold_down Numeric. Bounce threshold for upward trends. Default `0.1`.
#' @param bounce_threshold_up Numeric. Bounce threshold for downward trends. Default `0.1`.
#' @param bounce_threshold_none Numeric. Bounce threshold when no trend. Default `0.1`.
#' @param consecutive_zeros Integer. Zeros for reversal detection. Default `2`.
#' @param consecutive_nonzeros Integer. Non-zeros for return detection. Default `2`.
#' @param expected_down Logical. Suppress reversal detection if TRUE. Default `FALSE`.
#' @param x_var Character. Name of the price column. Default `"x"`.
#' @param y_var Character. Name of the consumption column. Default `"y"`.
#' @param id_var Character. Name of the subject identifier column. Default `"id"`.
#'
#' @return An object of class `beezdemand_systematicity` with the same structure
#'   as `check_systematic_demand()`, with `type = "cp"`.
#'
#' @details
#' If the data contains an `id` column (or column specified by `id_var`), each
#' unique ID is checked separately. Otherwise, the entire dataset is treated
#' as a single pattern.
#'
#' For cross-price data, the wrapper preserves the legacy meaning of
#' `check_unsystematic_cp()`:
#' - `trend_direction` and `bounce_direction` are taken directly from the legacy
#'   function outputs.
#' - `trend_pass` is set to `NA` because cross-price systematicity does not use a
#'   separate trend “pass/fail” criterion in the same way as purchase-task
#'   screening; instead, trend classification determines which bounce rule
#'   applies.
#' - `bounce_stat` is reported as the proportion relevant to the legacy bounce
#'   rule for the detected `trend_direction` (or `expected_down` case), computed
#'   from the legacy bounce counts and the number of price steps.
#'
#' @examples
#' \dontrun{
#' # Check cross-price data with id column
#' check <- check_systematic_cp(cp_data)
#'
#' # Check single pattern without id
#' check_single <- check_systematic_cp(data.frame(x = 1:10, y = runif(10)))
#' }
#'
#' @export
check_systematic_cp <- function(data,
                                trend_threshold = 0.025,
                                bounce_threshold_down = 0.1,
                                bounce_threshold_up = 0.1,
                                bounce_threshold_none = 0.1,
                                consecutive_zeros = 2,
                                consecutive_nonzeros = 2,
                                expected_down = FALSE,
                                x_var = "x",
                                y_var = "y",
                                id_var = "id") {
  call <- match.call()

  # Rename columns if needed
  if (x_var != "x" || y_var != "y") {
    data[[x_var]] -> data$x
    data[[y_var]] -> data$y
  }

  # Determine if data has id column
  has_id <- id_var %in% names(data)

  if (has_id) {
    # Process each id separately
    ids <- unique(data[[id_var]])
    results_list <- lapply(ids, function(id_val) {
      subset_data <- data[data[[id_var]] == id_val, ]

      legacy <- check_unsystematic_cp(
        data = subset_data,
        delta_threshold = trend_threshold,
        bounce_down_threshold = bounce_threshold_down,
        bounce_up_threshold = bounce_threshold_up,
        bounce_none_threshold = bounce_threshold_none,
        rev_zeroes = consecutive_zeros,
        ret_nums = consecutive_nonzeros,
        expected_down = expected_down,
        detailed = FALSE
      )

      # Compute bounce threshold used (consistent with legacy logic)
      bounce_threshold_used <- if (isTRUE(expected_down)) {
        bounce_threshold_up
      } else if (identical(legacy$delta_direction, "down")) {
        bounce_threshold_up
      } else if (identical(legacy$delta_direction, "up")) {
        bounce_threshold_down
      } else {
        bounce_threshold_none
      }

      # Compute bounce statistic (consistent with legacy logic)
      subset_complete <- subset_data[!is.na(subset_data$x) & !is.na(subset_data$y), ]
      denom <- nrow(subset_complete) - 1
      prop_above <- if (denom > 0) legacy$bounce_above / denom else NA_real_
      prop_below <- if (denom > 0) legacy$bounce_below / denom else NA_real_
      bounce_stat_used <- if (isTRUE(expected_down) || identical(legacy$delta_direction, "down")) {
        prop_above
      } else if (identical(legacy$delta_direction, "up")) {
        prop_below
      } else if (identical(legacy$delta_direction, "none")) {
        min(prop_above, prop_below, na.rm = TRUE)
      } else {
        NA_real_
      }

      tibble::tibble(
        id = as.character(id_val),
        type = "cp",
        trend_stat = NA_real_,
        trend_threshold = trend_threshold,
        trend_direction = legacy$delta_direction,
        trend_pass = NA,
        bounce_stat = bounce_stat_used,
        bounce_threshold = bounce_threshold_used,
        bounce_direction = legacy$bounce_direction,
        bounce_pass = !isTRUE(legacy$bounce_any),
        reversals = as.integer(legacy$reversals),
        reversals_pass = NA,
        returns = as.integer(legacy$returns),
        n_positive = NA_integer_,
        systematic = !isTRUE(legacy$bounce_any)
      )
    })

    results <- dplyr::bind_rows(results_list)
  } else {
    # Single pattern (no id column)
    legacy <- check_unsystematic_cp(
      data = data,
      delta_threshold = trend_threshold,
      bounce_down_threshold = bounce_threshold_down,
      bounce_up_threshold = bounce_threshold_up,
      bounce_none_threshold = bounce_threshold_none,
      rev_zeroes = consecutive_zeros,
      ret_nums = consecutive_nonzeros,
      expected_down = expected_down,
      detailed = TRUE
    )

    bounce_threshold_used <- if (isTRUE(expected_down)) {
      bounce_threshold_up
    } else if (identical(legacy$delta_direction, "down")) {
      bounce_threshold_up
    } else if (identical(legacy$delta_direction, "up")) {
      bounce_threshold_down
    } else {
      bounce_threshold_none
    }

    subset_complete <- data[!is.na(data$x) & !is.na(data$y), ]
    denom <- nrow(subset_complete) - 1
    prop_above <- if (denom > 0) legacy$bounce_above / denom else NA_real_
    prop_below <- if (denom > 0) legacy$bounce_below / denom else NA_real_
    bounce_stat_used <- if (isTRUE(expected_down) || identical(legacy$delta_direction, "down")) {
      prop_above
    } else if (identical(legacy$delta_direction, "up")) {
      prop_below
    } else if (identical(legacy$delta_direction, "none")) {
      min(prop_above, prop_below, na.rm = TRUE)
    } else {
      NA_real_
    }

    results <- tibble::tibble(
      id = "1",
      type = "cp",
      trend_stat = NA_real_,
      trend_threshold = trend_threshold,
      trend_direction = legacy$delta_direction,
      trend_pass = NA,
      bounce_stat = bounce_stat_used,
      bounce_threshold = bounce_threshold_used,
      bounce_direction = legacy$bounce_direction,
      bounce_pass = !isTRUE(legacy$bounce_any),
      reversals = as.integer(legacy$reversals),
      reversals_pass = NA,
      returns = as.integer(legacy$returns),
      n_positive = NA_integer_,
      systematic = !isTRUE(legacy$bounce_any)
    )
  }

  new_beezdemand_systematicity(results, type = "cp", call = call)
}


#' Print Method for beezdemand_systematicity
#'
#' @param x A beezdemand_systematicity object
#' @param ... Additional arguments (ignored)
#' @export
print.beezdemand_systematicity <- function(x, ...) {
  cat("\n")
  cat("Systematicity Check (", x$type, ")\n", sep = "")
  cat(strrep("-", 30), "\n")
  cat("Total patterns:", x$n_total, "\n")
  cat("Systematic:", x$n_systematic,
      "(", round(x$n_systematic / x$n_total * 100, 1), "%)\n")
  cat("Unsystematic:", x$n_unsystematic,
      "(", round(x$n_unsystematic / x$n_total * 100, 1), "%)\n")
  cat("\nUse summary() for details, tidy() for per-subject results.\n")

  invisible(x)
}


#' Summary Method for beezdemand_systematicity
#'
#' @param object A beezdemand_systematicity object
#' @param ... Additional arguments (ignored)
#' @return A summary.beezdemand_systematicity object
#' @export
summary.beezdemand_systematicity <- function(object, ...) {
  # Count by criteria
  trend_fail <- sum(!object$results$trend_pass, na.rm = TRUE)
  bounce_fail <- sum(!object$results$bounce_pass, na.rm = TRUE)

  if (object$type == "demand") {
    reversals_fail <- sum(!object$results$reversals_pass, na.rm = TRUE)
  } else {
    reversals_fail <- NA_integer_
  }

  # Problem IDs (unsystematic)
  problem_ids <- object$results$id[!object$results$systematic]

  # Counts tibble
  counts <- tibble::tibble(
    criterion = c("trend", "bounce", "reversals", "overall"),
    n_fail = c(trend_fail, bounce_fail, reversals_fail, object$n_unsystematic),
    pct_fail = c(
      trend_fail / object$n_total * 100,
      bounce_fail / object$n_total * 100,
      if (!is.na(reversals_fail)) reversals_fail / object$n_total * 100 else NA_real_,
      object$n_unsystematic / object$n_total * 100
    )
  )

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_systematicity",
      backend = NA_character_,
      type = object$type,
      nobs = NA_integer_,
      n_subjects = object$n_total,
      n_systematic = object$n_systematic,
      n_unsystematic = object$n_unsystematic,
      converged = NA,
      logLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      counts = counts,
      problem_ids = problem_ids,
      coefficients = tibble::tibble(
        term = character(),
        estimate = numeric(),
        std.error = numeric(),
        statistic = numeric(),
        p.value = numeric(),
        component = character()
      ),
      notes = character(0)
    ),
    class = c("summary.beezdemand_systematicity", "beezdemand_summary")
  )
}


#' Print Method for summary.beezdemand_systematicity
#'
#' @param x A summary.beezdemand_systematicity object
#' @param ... Additional arguments (ignored)
#' @export
print.summary.beezdemand_systematicity <- function(x, ...) {
  cat("\n")
  cat("Systematicity Check Summary (", x$type, ")\n", sep = "")
  cat(strrep("=", 50), "\n\n")

  cat("Total patterns:", x$n_subjects, "\n")
  cat("Systematic:", x$n_systematic,
      "(", round(x$n_systematic / x$n_subjects * 100, 1), "%)\n")
  cat("Unsystematic:", x$n_unsystematic,
      "(", round(x$n_unsystematic / x$n_subjects * 100, 1), "%)\n\n")

  cat("Failures by Criterion:\n")
  print(x$counts, n = Inf)
  cat("\n")

  if (length(x$problem_ids) > 0 && length(x$problem_ids) <= 20) {
    cat("Unsystematic IDs:", paste(x$problem_ids, collapse = ", "), "\n")
  } else if (length(x$problem_ids) > 20) {
    cat("Unsystematic IDs (first 20):",
        paste(utils::head(x$problem_ids, 20), collapse = ", "), "...\n")
  }

  invisible(x)
}


#' Tidy Method for beezdemand_systematicity
#'
#' @param x A beezdemand_systematicity object
#' @param ... Additional arguments (ignored)
#' @return The per-subject results tibble
#' @export
tidy.beezdemand_systematicity <- function(x, ...) {
  x$results
}


#' Glance Method for beezdemand_systematicity
#'
#' @param x A beezdemand_systematicity object
#' @param ... Additional arguments (ignored)
#' @return A one-row tibble of overall statistics
#' @export
glance.beezdemand_systematicity <- function(x, ...) {
  tibble::tibble(
    model_class = "beezdemand_systematicity",
    backend = NA_character_,
    type = x$type,
    nobs = NA_integer_,
    n_subjects = x$n_total,
    n_systematic = x$n_systematic,
    n_unsystematic = x$n_unsystematic,
    pct_systematic = x$n_systematic / x$n_total * 100,
    converged = NA,
    logLik = NA_real_,
    AIC = NA_real_,
    BIC = NA_real_
  )
}
