#' Summarize Cross-Price Unsystematic Data Check Results
#'
#' @description
#' Creates a summary of unsystematic data checks across multiple subjects and groups.
#' This function takes a dataframe containing results from multiple calls to
#' `check_unsystematic_cp()` and provides a comprehensive summary of patterns found.
#'
#' @param object A dataframe containing results from multiple `check_unsystematic_cp()` calls,
#'   typically with columns 'id', 'group', 'delta_direction', 'bounce_direction', 'bounce_any',
#'   'bounce_above', and 'bounce_below'.
#' @param ... Additional arguments (currently unused)
#'
#' @return A list with summary statistics including:
#'   \item{total_patterns}{Total number of patterns examined}
#'   \item{systematic_count}{Number of systematic patterns}
#'   \item{unsystematic_count}{Number of unsystematic patterns}
#'   \item{systematic_percent}{Percentage of systematic patterns}
#'   \item{unsystematic_percent}{Percentage of unsystematic patterns}
#'   \item{trend_counts}{Counts by trend direction}
#'   \item{bounce_counts}{Counts by bounce direction}
#'   \item{group_summary}{Summary statistics by group}
#'   \item{problem_ids}{IDs with the most unsystematic patterns}
#'
#' @examples
#' \dontrun{
#' # Run unsystematic checks on multiple patterns
#' unsys_all <- etm |>
#'   group_by(id, group) |>
#'   nest() |>
#'   mutate(unsys = map(data, check_unsystematic_cp)) |>
#'   unnest(unsys)
#'
#' # Create summary
#' summary(unsys_all)
#' }
#'
#' @export
summary.cp_unsystematic <- function(object, ...) {
  # Check if required columns exist
  required_cols <- c("bounce_any", "delta_direction", "bounce_direction")
  if (!all(required_cols %in% names(object))) {
    stop(
      "Input dataframe must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  # Calculate basic statistics
  total_patterns <- nrow(object)
  unsystematic_count <- sum(object$bounce_any, na.rm = TRUE)
  systematic_count <- total_patterns - unsystematic_count
  unsystematic_percent <- round(100 * unsystematic_count / total_patterns, 1)
  systematic_percent <- round(100 * systematic_count / total_patterns, 1)

  # Count by trend direction
  trend_counts <- table(object$delta_direction, useNA = "ifany")
  trend_percents <- round(100 * trend_counts / sum(trend_counts), 1)
  trend_summary <- data.frame(
    direction = names(trend_counts),
    count = as.numeric(trend_counts),
    percent = trend_percents
  )

  # Count by bounce direction
  bounce_counts <- table(object$bounce_direction, useNA = "ifany")
  bounce_percents <- round(100 * bounce_counts / sum(bounce_counts), 1)
  bounce_summary <- data.frame(
    direction = names(bounce_counts),
    count = as.numeric(bounce_counts),
    percent = bounce_percents
  )

  # If group column exists, summarize by group
  group_summary <- NULL
  if ("group" %in% names(object)) {
    group_summary <- tapply(object$bounce_any, object$group, function(x) {
      c(
        total = length(x),
        unsystematic = sum(x, na.rm = TRUE),
        unsystematic_percent = round(100 * sum(x, na.rm = TRUE) / length(x), 1)
      )
    })
    group_summary <- do.call(rbind, group_summary)
    group_summary <- as.data.frame(group_summary)
  }

  # If id column exists, find IDs with most unsystematic patterns
  problem_ids <- NULL
  if (all(c("id", "group") %in% names(object))) {
    id_unsys_counts <- tapply(object$bounce_any, object$id, sum, na.rm = TRUE)
    problem_ids <- sort(id_unsys_counts, decreasing = TRUE)
    problem_ids <- data.frame(
      id = names(problem_ids),
      unsystematic_count = as.numeric(problem_ids)
    )
    problem_ids <- problem_ids[problem_ids$unsystematic_count > 0, ]
  }

  # Create the summary output
  result <- list(
    total_patterns = total_patterns,
    systematic_count = systematic_count,
    unsystematic_count = unsystematic_count,
    systematic_percent = systematic_percent,
    unsystematic_percent = unsystematic_percent,
    trend_counts = trend_summary,
    bounce_counts = bounce_summary,
    group_summary = group_summary,
    problem_ids = problem_ids
  )

  class(result) <- "summary.cp_unsystematic"
  return(result)
}

#' Print method for summary.cp_unsystematic objects
#' @param x A summary.cp_unsystematic object
#' @param ... Additional arguments (currently unused)
#' @importFrom utils head
#' @export
print.summary.cp_unsystematic <- function(x, ...) {
  cat("Cross-Price Demand Unsystematic Data Summary\n")
  cat("===========================================\n\n")

  cat("Overall Statistics:\n")
  cat(sprintf("Total patterns examined: %d\n", x$total_patterns))
  cat(sprintf(
    "Systematic patterns: %d (%.1f%%)\n",
    x$systematic_count,
    x$systematic_percent
  ))
  cat(sprintf(
    "Unsystematic patterns: %d (%.1f%%)\n\n",
    x$unsystematic_count,
    x$unsystematic_percent
  ))

  cat("Trend Direction Counts:\n")
  print(x$trend_counts)
  cat("\n")

  cat("Bounce Pattern Counts:\n")
  print(x$bounce_counts)
  cat("\n")

  if (!is.null(x$group_summary)) {
    cat("Summary by Group:\n")
    print(x$group_summary)
    cat("\n")
  }

  if (!is.null(x$problem_ids) && nrow(x$problem_ids) > 0) {
    cat("Problematic IDs (with unsystematic patterns):\n")
    print(head(x$problem_ids, 10)) # Show top 10 problematic IDs
    if (nrow(x$problem_ids) > 10) {
      cat(sprintf(
        "... and %d more IDs with unsystematic patterns\n",
        nrow(x$problem_ids) - 10
      ))
    }
  }

  invisible(x)
}

#' @export
summary.tbl_df <- function(object, ...) {
  # Check if this looks like the result of check_unsystematic_cp
  required_cols <- c("bounce_any", "delta_direction", "bounce_direction")

  if (all(required_cols %in% names(object))) {
    # This looks like unsystematic check results, use our specialized summary
    summary.cp_unsystematic(object, ...)
  } else {
    # Fall back to default tibble summary
    NextMethod()
  }
}
