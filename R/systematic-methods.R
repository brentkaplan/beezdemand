#' Summarize Cross-Price Unsystematic Data Check Results
#'
#' @description
#' Summarizes systematic and unsystematic patterns from multiple calls to
#' `check_unsystematic_cp()`. This includes overall proportions, trend and bounce
#' direction counts, and optionally summaries by subject or group.
#'
#' @param object A data frame containing results from multiple `check_unsystematic_cp()` calls,
#'   with at minimum the columns 'delta_direction', 'bounce_direction', and 'bounce_any'.
#'   Columns 'id', 'group', 'reversals', and 'returns' are optional but allow extended summaries.
#' @param ... Additional arguments (currently unused)
#'
#' @return A list of class `summary.cp_unsystematic` with the following elements:
#'   \describe{
#'     \item{total_patterns}{Number of total patterns examined.}
#'     \item{systematic_count}{Count of systematic patterns (no bounce).}
#'     \item{unsystematic_count}{Count of unsystematic patterns (bounce detected).}
#'     \item{systematic_percent}{Proportion of systematic patterns.}
#'     \item{unsystematic_percent}{Proportion of unsystematic patterns.}
#'     \item{trend_counts}{Breakdown of trend directions.}
#'     \item{bounce_counts}{Breakdown of bounce directions.}
#'     \item{reversal_summary}{(Optional) Summary of zero-reversal patterns, if present in input.}
#'     \item{return_summary}{(Optional) Summary of zero-return patterns, if present in input.}
#'     \item{group_summary}{(Optional) Summary stats by 'group'.}
#'     \item{problem_ids}{(Optional) Top IDs with unsystematic patterns.}
#'   }
#'
#' @export
summary.cp_unsystematic <- function(object, ...) {
  required_cols <- c("bounce_any", "delta_direction", "bounce_direction")
  if (!all(required_cols %in% names(object))) {
    stop("Input must include columns: ", paste(required_cols, collapse = ", "))
  }

  total_patterns <- nrow(object)
  unsystematic_count <- sum(object$bounce_any, na.rm = TRUE)
  systematic_count <- total_patterns - unsystematic_count
  unsystematic_percent <- round(100 * unsystematic_count / total_patterns, 1)
  systematic_percent <- round(100 * systematic_count / total_patterns, 1)

  trend_counts <- as.data.frame(table(object$delta_direction, useNA = "ifany"))
  names(trend_counts) <- c("direction", "count")
  trend_counts$percent <- round(
    100 * trend_counts$count / sum(trend_counts$count),
    1
  )

  bounce_counts <- as.data.frame(table(
    object$bounce_direction,
    useNA = "ifany"
  ))
  names(bounce_counts) <- c("direction", "count")
  bounce_counts$percent <- round(
    100 * bounce_counts$count / sum(bounce_counts$count),
    1
  )

  # Initialize summaries for optional columns
  reversal_summary <- NULL
  return_summary <- NULL

  # Conditionally summarize reversals if column exists
  if ("reversals" %in% names(object)) {
    reversal_count <- sum(object$reversals > 0, na.rm = TRUE)
    reversal_summary <- list(
      count = reversal_count,
      percent = round(100 * reversal_count / total_patterns, 1)
    )
  }

  # Conditionally summarize returns if column exists
  if ("returns" %in% names(object)) {
    return_count <- sum(object$returns > 0, na.rm = TRUE)
    return_summary <- list(
      count = return_count,
      percent = round(100 * return_count / total_patterns, 1)
    )
  }

  group_summary <- NULL
  if ("group" %in% names(object)) {
    group_summary <- aggregate(bounce_any ~ group, object, function(x) {
      c(
        total = length(x),
        unsystematic = sum(x),
        unsystematic_percent = round(100 * mean(x), 1)
      )
    })
    group_summary <- do.call(data.frame, group_summary)
  }

  problem_ids <- NULL
  if (all(c("id", "group") %in% names(object))) {
    id_counts <- aggregate(bounce_any ~ id, object, sum)
    problem_ids <- id_counts[id_counts$bounce_any > 0, ]
    names(problem_ids) <- c("id", "unsystematic_count")
    problem_ids <- problem_ids[order(-problem_ids$unsystematic_count), ]
  }

  structure(
    list(
      total_patterns = total_patterns,
      systematic_count = systematic_count,
      unsystematic_count = unsystematic_count,
      systematic_percent = systematic_percent,
      unsystematic_percent = unsystematic_percent,
      trend_counts = trend_counts,
      bounce_counts = bounce_counts,
      reversal_summary = reversal_summary,
      return_summary = return_summary,
      group_summary = group_summary,
      problem_ids = problem_ids
    ),
    class = "summary.cp_unsystematic"
  )
}

#' @export
print.summary.cp_unsystematic <- function(x, ...) {
  cat("Cross-Price Demand Unsystematic Data Summary\n")
  cat("===========================================\n\n")
  cat(sprintf("Total patterns examined: %d\n", x$total_patterns))
  cat(sprintf(
    "Systematic patterns: %d (%.1f%%)\n",
    x$systematic_count,
    x$systematic_percent
  ))
  cat(sprintf(
    "Unsystematic patterns (bounces): %d (%.1f%%)\n",
    x$unsystematic_count,
    x$unsystematic_percent
  ))

  # Conditionally print reversal and return summaries
  if (!is.null(x$reversal_summary)) {
    cat(sprintf(
      "Patterns with Reversals: %d (%.1f%%)\n",
      x$reversal_summary$count,
      x$reversal_summary$percent
    ))
  }
  if (!is.null(x$return_summary)) {
    cat(sprintf(
      "Patterns with Returns: %d (%.1f%%)\n",
      x$return_summary$count,
      x$return_summary$percent
    ))
  }
  cat("\n")

  cat("Trend Direction Counts:\n")
  print(x$trend_counts)
  cat("\nBounce Pattern Counts:\n")
  print(x$bounce_counts)

  if (!is.null(x$group_summary)) {
    cat("\nSummary by Group:\n")
    print(x$group_summary)
  }
  if (!is.null(x$problem_ids) && nrow(x$problem_ids) > 0) {
    cat("\nProblematic IDs (most unsystematic patterns):\n")
    print(utils::head(x$problem_ids, 10))
    if (nrow(x$problem_ids) > 10) {
      cat(sprintf("... and %d more\n", nrow(x$problem_ids) - 10))
    }
  }
  invisible(x)
}

#' @export
summary.tbl_df <- function(object, ...) {
  required_cols <- c("bounce_any", "delta_direction", "bounce_direction")
  if (all(required_cols %in% names(object))) {
    summary.cp_unsystematic(object, ...)
  } else {
    NextMethod()
  }
}
