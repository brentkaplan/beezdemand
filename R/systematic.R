#' Check for Unsystematic Patterns in Cross-Price Data
#'
#' @description
#' Analyzes data patterns to determine whether there's a systematic relationship
#' between two variables (typically price and consumption), and checks for
#' significant deviations or "bounces" that might indicate unsystematic patterns.
#' This function is particularly useful for analyzing demand elasticity and
#' cross-commodity relationships in behavioral economics.
#'
#' @param data A data frame with columns 'x' and 'y', where 'x' typically represents
#'   price and 'y' typically represents consumption or demand.
#' @param delta_threshold Numeric value (default 0.025) that determines what qualifies
#'   as a significant trend in the data.
#' @param bounce_down_threshold Numeric value (default 0.1) that determines what
#'   proportion of downward deviations qualifies as significant for upward trends.
#' @param bounce_up_threshold Numeric value (default 0.1) that determines what
#'   proportion of upward deviations qualifies as significant for downward trends.
#' @param bounce_none_threshold Numeric value (default 0.2) that determines what
#'   proportion of deviations qualifies as significant when there's no clear trend.
#' @param verbose Logical value (default FALSE) indicating whether to print
#'   intermediate calculations.
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{delta_down}{Logical. TRUE if there's a significant downward trend.}
#'     \item{delta_up}{Logical. TRUE if there's a significant upward trend.}
#'     \item{delta_none}{Logical. TRUE if there's no significant trend.}
#'     \item{bounce_up}{Logical. TRUE if there are significant upward bounces in a downward trend. NA if not applicable.}
#'     \item{bounce_down}{Logical. TRUE if there are significant downward bounces in an upward trend. NA if not applicable.}
#'     \item{bounce_none}{Logical. TRUE if there are significant bounces in no-trend data. NA if not applicable.}
#'     \item{bounce_any}{Logical. TRUE if any type of bounce was detected.}
#'     \item{bounce_above}{Integer. Count of upward deviations meeting the threshold criteria.}
#'     \item{bounce_below}{Integer. Count of downward deviations meeting the threshold criteria.}
#'   }
#'
#' @details
#' The function first determines if the data shows a significant upward trend,
#' downward trend, or no trend by calculating slopes in log-scale. It then
#' checks for "bounces" or significant deviations based on the identified trend type:
#'
#' - For downward trends: looks for significant upward jumps
#' - For upward trends: looks for significant downward drops
#' - For no trends: looks for deviations both above and below the average
#'
#' The function uses relative changes (percentage changes) rather than absolute
#' changes to detect bounces, making it suitable for data at different scales.
#'
#' @examples
#' # Create a sequence of x values for examples
#' x_seq <- 10^(seq(-2, 2, length.out = 10))
#'
#' # Example 1: Flat pattern with some fluctuations
#' pattern1 <- data.frame(
#'   x = x_seq,
#'   y = c(10, 5, 10, 9, 10, 13, 10, 10, 7, 9)
#' )
#' check_unsystematic_cp(pattern1)
#'
#' # Example 2: Upward trend with bounces
#' pattern2 <- data.frame(
#'   x = x_seq,
#'   y = 10 - c(10, 10, 7, 8, 2, 6, 3, 4, 10, 0) # increasing pattern
#' )
#' check_unsystematic_cp(pattern2)
#'
#' # Example 3: Downward trend
#' pattern3 <- data.frame(
#'   x = x_seq,
#'   y = c(10, 10, 10, 9, 8, 5, 3, 2, 0, 0)
#' )
#' check_unsystematic_cp(pattern3)
#'
#' # Example 4: No clear trend with significant deviations
#' pattern4 <- data.frame(
#'   x = x_seq,
#'   y = c(10, 10, 10, 9, 8, 5, 3, 2, 0, 10)
#' )
#' check_unsystematic_cp(pattern4)
#'
#' # Example 5: Perfectly flat line
#' pattern8 <- data.frame(
#'   x = x_seq,
#'   y = rep(10, 10)
#' )
#' check_unsystematic_cp(pattern8)
#'
#' # Analyzing multiple patterns at once
#' # First combine patterns into one dataframe with IDs
#' combined_df <- rbind(
#'   cbind(id = "pattern1", pattern1),
#'   cbind(id = "pattern2", pattern2),
#'   cbind(id = "pattern3", pattern3),
#'   cbind(id = "pattern4", pattern4),
#'   cbind(id = "pattern8", pattern8)
#' )
#'
#' # Then analyze all patterns and combine results
#' results_list <- lapply(unique(combined_df$id), function(pattern_id) {
#'   pattern_data <- combined_df[combined_df$id == pattern_id, c("x", "y")]
#'   result <- check_unsystematic_cp(pattern_data)
#'   result$id <- pattern_id
#'   return(result)
#' })
#'
#' # Combine all results
#' all_results <- do.call(rbind, results_list)
#' print(all_results[, c(
#'   "id",
#'   "delta_direction",
#'   "bounce_direction",
#'   "bounce_any"
#' )])
#'
#' @export
check_unsystematic_cp <- function(
  data,
  delta_threshold = 0.025,
  bounce_down_threshold = 0.1,
  bounce_up_threshold = 0.1,
  bounce_none_threshold = 0.2,
  verbose = FALSE
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  if (!all(c("x", "y") %in% colnames(data))) {
    stop("Input data frame must contain 'x' and 'y' columns")
  }
  if (any(is.na(data$x)) || any(is.na(data$y))) {
    warning("Input data contains NA values which will be removed")
    data <- data[!is.na(data$x) & !is.na(data$y), ]
  }
  if (nrow(data) < 3) {
    stop("Input data must contain at least 3 rows for meaningful analysis")
  }

  # Order data by x values
  data_ordered <- data[order(data$x), ]

  # Add small value to x and y to avoid log(0) issues
  data_ordered$x <- data_ordered$x + 0.01
  data_ordered$y <- data_ordered$y + 0.01

  # Get first, last, and length values for clearer code
  first_x <- data_ordered$x[1]
  last_x <- data_ordered$x[nrow(data_ordered)]
  first_y <- data_ordered$y[1]
  last_y <- data_ordered$y[nrow(data_ordered)]
  data_length <- nrow(data_ordered)

  # Calculate x denominator once to avoid repetition
  x_log_diff <- log10(last_x) - log10(first_x)

  # Check for trends
  delta_down <- round((log10(first_y) - log10(last_y)) / x_log_diff, 4) >
    delta_threshold
  delta_up <- round((log10(last_y) - log10(first_y)) / x_log_diff, 4) >
    delta_threshold
  delta_none <- !delta_down & !delta_up

  # Initialize bounce variables
  bounce_up <- NA
  bounce_down <- NA
  bounce_none <- NA
  bounce_above <- NA
  bounce_below <- NA

  # Calculate bounce metrics based on trend type
  if (delta_down) {
    # For downward trend, check for upward bounces
    bounce_threshold <- first_y * 0.25
    bounce_above <- sum(diff(data_ordered$y) > bounce_threshold, na.rm = TRUE)
    bounce_proportion <- bounce_above / (data_length - 1)
    bounce_up <- bounce_proportion > bounce_up_threshold
    bounce_below <- NA

    if (verbose) {
      print(paste("Bounce up proportion:", round(bounce_proportion, 4)))
    }
  }

  if (delta_up) {
    # For upward trend, check for downward bounces
    bounce_threshold <- last_y * 0.25
    bounce_below <- sum(-diff(data_ordered$y) > bounce_threshold, na.rm = TRUE)
    bounce_proportion <- bounce_below / (data_length - 1)
    bounce_down <- bounce_proportion > bounce_down_threshold
    bounce_above <- NA

    if (verbose) {
      print(paste("Bounce down proportion:", round(bounce_proportion, 4)))
    }
  }

  if (delta_none) {
    # For no trend, check for both upward and downward bounces
    mean_y <- mean(c(first_y, last_y))
    high_threshold <- mean_y * 1.25
    low_threshold <- mean_y * 0.75

    y_above_mean <- data_ordered$y[data_ordered$y > mean_y]
    y_below_mean <- data_ordered$y[data_ordered$y < mean_y]

    bounce_above <- sum(y_above_mean > high_threshold, na.rm = TRUE)
    bounce_below <- sum(y_below_mean < low_threshold, na.rm = TRUE)

    above_proportion <- bounce_above / (data_length - 1)
    below_proportion <- bounce_below / (data_length - 1)
    combined_proportion <- (above_proportion + below_proportion) / 2

    bounce_none <- combined_proportion > bounce_none_threshold

    if (verbose) {
      print(paste("Bounce none proportion:", round(combined_proportion, 4)))
    }
  }

  # Check if any bounce type is detected
  bounce_any <- any(bounce_up, bounce_down, bounce_none, na.rm = TRUE)

  # Determine direction as a single categorical variable
  delta_direction <- ifelse(delta_down, "down", ifelse(delta_up, "up", "none"))

  # Determine bounce direction as a single categorical variable
  bounce_direction <- NA
  if (delta_down && !is.na(bounce_up)) {
    bounce_direction <- ifelse(bounce_up, "up", "none_detected")
  } else if (delta_up && !is.na(bounce_down)) {
    bounce_direction <- ifelse(bounce_down, "down", "none_detected")
  } else if (delta_none && !is.na(bounce_none)) {
    bounce_direction <- ifelse(bounce_none, "significant", "none_detected")
  }

  # Return results
  return(data.frame(
    delta_direction = delta_direction,
    bounce_direction = bounce_direction,
    bounce_any = unname(bounce_any),
    bounce_above = unname(bounce_above),
    bounce_below = unname(bounce_below)
  ))
}
