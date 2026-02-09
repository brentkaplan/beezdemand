#' S3 Methods for beezdemand_empirical Objects
#'
#' @description
#' Methods for printing, summarizing, and visualizing objects of class
#' `beezdemand_empirical` created by [get_empirical_measures()].
#'
#' @param x,object A `beezdemand_empirical` object
#' @param ... Additional arguments passed to plotting functions
#' @param type Character string specifying plot type. Options:
#'   \itemize{
#'     \item "histogram" (default) - Faceted histograms showing distribution of each measure
#'     \item "matrix" - Scatterplot matrix showing pairwise relationships between measures
#'   }
#'
#' @return
#' \itemize{
#'   \item `print()` - Returns the object invisibly (called for side effects)
#'   \item `summary()` - Returns a list with extended summary information
#'   \item `plot()` - Returns a ggplot2 object
#' }
#'
#' @details
#' ## Print Method
#' Displays a compact summary showing the number of subjects analyzed and a
#' preview of the empirical measures table.
#'
#' ## Summary Method
#' Provides extended information including:
#' \itemize{
#'   \item Data summary (subjects, zero consumption patterns, completeness)
#'   \item Descriptive statistics for each empirical measure (min, median, mean, max, SD)
#'   \item Missing data patterns
#' }
#'
#' ## Plot Method
#' Creates visualizations of empirical measures across subjects.
#'
#' **Histogram type** (default):
#' \itemize{
#'   \item Six-panel faceted plot showing distribution of each measure
#'   \item Helps identify central tendencies and outliers
#'   \item Uses modern beezdemand styling
#' }
#'
#' **Matrix type**:
#' \itemize{
#'   \item Scatterplot matrix (pairs plot) showing relationships between measures
#'   \item Useful for identifying correlated demand metrics
#'   \item Lower triangle: scatterplots with smoothed trend lines
#'   \item Diagonal: density plots
#'   \item Upper triangle: correlation coefficients
#' }
#'
#' @seealso [get_empirical_measures()]
#'
#' @examples
#' \dontrun{
#' data(apt, package = "beezdemand")
#' emp <- get_empirical_measures(apt)
#'
#' # Print compact summary
#' print(emp)
#'
#' # Extended summary
#' summary(emp)
#'
#' # Histogram of measure distributions
#' plot(emp)
#'
#' # Scatterplot matrix
#' plot(emp, type = "matrix")
#' }
#'
#' @name beezdemand_empirical_methods
NULL

#' @rdname beezdemand_empirical_methods
#' @export
print.beezdemand_empirical <- function(x, ...) {
  cat("Empirical Demand Measures\n")
  cat("=========================\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Data Summary:\n")
  cat(sprintf("  Subjects: %d\n", x$data_summary$n_subjects))
  cat(sprintf("  Subjects with zero consumption: %s\n",
              ifelse(x$data_summary$has_zeros, "Yes", "No")))
  cat(sprintf("  Complete cases (no NAs): %d\n", x$data_summary$complete_cases))
  cat("\n")

  cat("Empirical Measures:\n")
  print(x$measures, row.names = FALSE)

  invisible(x)
}

#' @rdname beezdemand_empirical_methods
#' @export
summary.beezdemand_empirical <- function(object, ...) {
  cat("Extended Summary of Empirical Demand Measures\n")
  cat("=============================================\n\n")

  cat("Data Overview:\n")
  cat(sprintf("  Number of subjects: %d\n", object$data_summary$n_subjects))
  cat(sprintf("  Complete cases: %d (%.1f%%)\n",
              object$data_summary$complete_cases,
              100 * object$data_summary$complete_cases / object$data_summary$n_subjects))
  cat("\n")

  # Calculate descriptive statistics for each measure (excluding id column)
  measure_cols <- c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")

  cat("Descriptive Statistics for Empirical Measures:\n")
  cat("-----------------------------------------------\n")

  for (measure in measure_cols) {
    vals <- object$measures[[measure]]
    n_missing <- sum(is.na(vals))

    cat(sprintf("\n%s:\n", measure))
    cat(sprintf("  Min: %.2f\n", min(vals, na.rm = TRUE)))
    cat(sprintf("  Median: %.2f\n", median(vals, na.rm = TRUE)))
    cat(sprintf("  Mean: %.2f\n", mean(vals, na.rm = TRUE)))
    cat(sprintf("  Max: %.2f\n", max(vals, na.rm = TRUE)))
    cat(sprintf("  SD: %.2f\n", sd(vals, na.rm = TRUE)))
    if (n_missing > 0) {
      cat(sprintf("  Missing: %d (%.1f%%)\n", n_missing,
                  100 * n_missing / length(vals)))
    }
  }

  # Return summary list invisibly
  stats_list <- lapply(measure_cols, function(m) {
    vals <- object$measures[[m]]
    c(min = min(vals, na.rm = TRUE),
      median = median(vals, na.rm = TRUE),
      mean = mean(vals, na.rm = TRUE),
      max = max(vals, na.rm = TRUE),
      sd = sd(vals, na.rm = TRUE),
      n_missing = sum(is.na(vals)))
  })
  names(stats_list) <- measure_cols

  invisible(list(
    data_summary = object$data_summary,
    measure_statistics = stats_list
  ))
}

#' @rdname beezdemand_empirical_methods
#' @export
plot.beezdemand_empirical <- function(x, type = "histogram", ...) {
  # Validate type
  type <- match.arg(type, c("histogram", "matrix"))

  # Extract measures (exclude id column)
  measures_df <- x$measures[, c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")]

  if (type == "histogram") {
    # Reshape to long format for faceting
    measures_long <- data.frame(
      id = rep(x$measures$id, 5),
      measure = rep(c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe"),
                    each = nrow(x$measures)),
      value = c(measures_df$Intensity, measures_df$BP0, measures_df$BP1,
                measures_df$Omaxe, measures_df$Pmaxe)
    )

    # Create faceted histogram
    p <- ggplot2::ggplot(measures_long, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      ggplot2::facet_wrap(~ measure, scales = "free", ncol = 2) +
      ggplot2::labs(x = "Value", y = "Frequency",
                   title = "Distribution of Empirical Demand Measures") +
      theme_apa()

  } else if (type == "matrix") {
    # Create scatterplot matrix using GGally if available, otherwise base pairs
    if (requireNamespace("GGally", quietly = TRUE)) {
      p <- GGally::ggpairs(
        measures_df,
        title = "Empirical Demand Measures: Scatterplot Matrix",
        lower = list(continuous = GGally::wrap("points", alpha = 0.5)),
        diag = list(continuous = GGally::wrap("densityDiag", fill = "steelblue")),
        upper = list(continuous = GGally::wrap("cor", size = 4))
      ) + theme_apa()
    } else {
      # Fallback to base graphics pairs plot
      message("Package 'GGally' not installed. Using base graphics pairs plot.")
      message("Install GGally with: install.packages('GGally') for better visualization.")
      pairs(measures_df,
            main = "Empirical Demand Measures: Scatterplot Matrix",
            pch = 19,
            col = rgb(0, 0, 1, 0.5))
      return(invisible(NULL))
    }
  }

  return(p)
}
