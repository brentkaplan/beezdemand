#' S3 Methods for beezdemand_descriptive Objects
#'
#' @description
#' Methods for printing, summarizing, and visualizing objects of class
#' `beezdemand_descriptive` created by [get_descriptive_summary()].
#'
#' @param x,object A `beezdemand_descriptive` object
#' @param ... Additional arguments (currently unused)
#' @param x_trans Character string specifying x-axis transformation. Options:
#'   "identity" (default), "log10", "log", "sqrt". See [scales::transform_log10()] etc.
#' @param y_trans Character string specifying y-axis transformation. Options:
#'   "identity" (default), "log10", "log", "sqrt", "pseudo_log" (signed log).
#' @param show_zeros Logical indicating whether to show proportion of zeros as labels
#'   on the boxplot (default: FALSE)
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
#' Displays a compact summary showing the number of subjects and prices analyzed,
#' plus a preview of the statistics table.
#'
#' ## Summary Method
#' Provides extended information including:
#' \itemize{
#'   \item Data summary (subjects, prices analyzed)
#'   \item Distribution of means across prices (min, median, max)
#'   \item Proportion of zeros by price (range)
#'   \item Missing data summary
#' }
#'
#' ## Plot Method
#' Creates a boxplot showing the distribution of consumption at each price point.
#' Features:
#' \itemize{
#'   \item Red cross markers indicate means
#'   \item Boxes show median and quartiles
#'   \item Whiskers extend to 1.5 * IQR
#'   \item Supports axis transformations (log, sqrt, etc.)
#'   \item Uses modern beezdemand styling via [theme_apa()]
#' }
#'
#' @seealso [get_descriptive_summary()]
#'
#' @examples
#' \dontrun{
#' data(apt, package = "beezdemand")
#' desc <- get_descriptive_summary(apt)
#'
#' # Print compact summary
#' print(desc)
#'
#' # Extended summary
#' summary(desc)
#'
#' # Default boxplot
#' plot(desc)
#'
#' # With log-transformed y-axis
#' plot(desc, y_trans = "log10")
#'
#' # With both axes transformed
#' plot(desc, x_trans = "log10", y_trans = "pseudo_log")
#' }
#'
#' @name beezdemand_descriptive_methods
NULL

#' @rdname beezdemand_descriptive_methods
#' @export
print.beezdemand_descriptive <- function(x, ...) {
  cat("Descriptive Summary of Demand Data\n")
  cat("===================================\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Data Summary:\n")
  cat(sprintf("  Subjects: %d\n", x$data_summary$n_subjects))
  cat(sprintf("  Prices analyzed: %d\n", x$data_summary$n_prices))
  cat("\n")

  cat("Statistics by Price:\n")
  print(x$statistics, row.names = FALSE)

  invisible(x)
}

#' @rdname beezdemand_descriptive_methods
#' @export
summary.beezdemand_descriptive <- function(object, ...) {
  cat("Extended Summary of Descriptive Statistics\n")
  cat("==========================================\n\n")

  cat("Data Overview:\n")
  cat(sprintf("  Number of subjects: %d\n", object$data_summary$n_subjects))
  cat(sprintf("  Number of prices: %d\n", object$data_summary$n_prices))
  cat(sprintf("  Price range: %s to %s\n",
              min(object$statistics$Price),
              max(object$statistics$Price)))
  cat("\n")

  cat("Distribution of Mean Consumption Across Prices:\n")
  cat(sprintf("  Minimum: %.2f\n", min(object$statistics$Mean, na.rm = TRUE)))
  cat(sprintf("  Median: %.2f\n", median(object$statistics$Mean, na.rm = TRUE)))
  cat(sprintf("  Maximum: %.2f\n", max(object$statistics$Mean, na.rm = TRUE)))
  cat("\n")

  cat("Proportion of Zeros by Price:\n")
  cat(sprintf("  Range: %.2f to %.2f\n",
              min(object$statistics$PropZeros),
              max(object$statistics$PropZeros)))
  n_full_zeros <- sum(object$statistics$PropZeros == 1)
  if (n_full_zeros > 0) {
    cat(sprintf("  Prices with all zeros: %d\n", n_full_zeros))
  }
  cat("\n")

  cat("Missing Data:\n")
  total_nas <- sum(object$statistics$NAs)
  if (total_nas == 0) {
    cat("  No missing values detected\n")
  } else {
    cat(sprintf("  Total NAs: %d\n", total_nas))
    cat(sprintf("  Prices with NAs: %d\n", sum(object$statistics$NAs > 0)))
  }

  # Return summary list invisibly
  invisible(list(
    data_summary = object$data_summary,
    mean_distribution = c(
      min = min(object$statistics$Mean, na.rm = TRUE),
      median = median(object$statistics$Mean, na.rm = TRUE),
      max = max(object$statistics$Mean, na.rm = TRUE)
    ),
    zero_proportion_range = c(
      min = min(object$statistics$PropZeros),
      max = max(object$statistics$PropZeros)
    ),
    missing_data = list(
      total_nas = total_nas,
      prices_with_nas = sum(object$statistics$NAs > 0)
    )
  ))
}

#' @rdname beezdemand_descriptive_methods
#' @export
plot.beezdemand_descriptive <- function(x,
                                        x_trans = "identity",
                                        y_trans = "identity",
                                        show_zeros = FALSE,
                                        ...) {
  # Extract data
  dat <- x$data
  if (is.null(dat)) {
    stop("Data not found in object. Cannot create plot.", call. = FALSE)
  }

  # Create base boxplot
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = as.factor(x), y = y)) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(fun = "mean", geom = "point", shape = 43,
                         size = 5, color = "red") +
    ggplot2::labs(x = "Price", y = "Reported Consumption") +
    theme_apa()

  # Apply transformations
  if (x_trans != "identity") {
    p <- p + ggplot2::scale_x_continuous(trans = x_trans)
  }

  if (y_trans != "identity") {
    if (y_trans == "pseudo_log") {
      p <- p + ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans())
    } else {
      p <- p + ggplot2::scale_y_continuous(trans = y_trans)
    }
  }

  # Optionally add zero proportion labels
  if (show_zeros) {
    # Create label data frame
    label_data <- x$statistics
    label_data$x <- as.factor(label_data$Price)
    label_data$label <- sprintf("%.0f%%", label_data$PropZeros * 100)

    # Add text labels at top of plot
    p <- p + ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = x, y = Inf, label = label),
      vjust = 1.5,
      size = 3,
      inherit.aes = FALSE
    )
  }

  return(p)
}
