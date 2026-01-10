#' Check for Unsystematic Patterns in Cross-Price Data
#'
#' @description
#' Analyzes whether consumption data shows systematic trends or unsystematic patterns ("bounces")
#' with respect to price. Includes detection of zero-value reversal/return sequences and allows
#' flexible output based on the level of detail requested. See Rzeszutek et al. (in press) for
#' more details.
#'
#' @param data A data frame with columns 'x' and 'y', where 'x' is price and 'y' is consumption.
#' @param delta_threshold Numeric. Threshold for detecting log-scale trends (default 0.025).
#' @param bounce_down_threshold Numeric. Minimum downward bounce proportion to count as significant in upward trends.
#' @param bounce_up_threshold Numeric. Minimum upward bounce proportion to count as significant in downward trends.
#' @param bounce_none_threshold Numeric. Minimum bounce proportion to count as significant in no-trend cases.
#' @param rev_zeroes Integer. Length of zero sequences to detect reversals (default 2).
#' @param ret_nums Integer. Length of non-zero sequences to detect returns (default 2).
#' @param expected_down Logical. If TRUE, suppress reversal detection.
#' @param verbose Logical. If TRUE, print intermediate values (default FALSE).
#' @param detailed Logical. If TRUE, return additional columns including all trend/bounce flags.
#'
#' @return A data frame of class `cp_unsystematic` with core results:
#'   \describe{
#'     \item{delta_direction}{Character: 'down', 'up', or 'none'.}
#'     \item{bounce_direction}{Character: 'up', 'down', 'significant', or 'none'.}
#'     \item{bounce_any}{Logical. TRUE if any bounce pattern detected.}
#'     \item{bounce_above}{Integer. Number of upward changes meeting threshold.}
#'     \item{bounce_below}{Integer. Number of downward changes meeting threshold.}
#'     \item{reversals}{Integer. Detected reversals from 0 to non-0.}
#'     \item{returns}{Integer. Detected returns from non-0 to 0.}
#'   }
#' If `detailed = TRUE`, returns additional columns:
#'   \describe{
#'     \item{delta_down}{Logical. Significant downward trend.}
#'     \item{delta_up}{Logical. Significant upward trend.}
#'     \item{delta_none}{Logical. No significant trend.}
#'     \item{bounce_up}{Logical. Significant bounce up in a downward trend.}
#'     \item{bounce_down}{Logical. Significant bounce down in an upward trend.}
#'     \item{bounce_none}{Logical. Significant bounces in no-trend data.}
#'   }
#'
#' @examples
#' x_seq <- 10^(seq(-2, 2, length.out = 10))
#' pattern <- data.frame(x = x_seq, y = c(10, 5, 10, 9, 10, 13, 10, 10, 7, 9))
#' check_unsystematic_cp(pattern)
#'
#' @export
check_unsystematic_cp <- function(
  data,
  delta_threshold = 0.025,
  bounce_down_threshold = 0.1,
  bounce_up_threshold = 0.1,
  bounce_none_threshold = 0.1,
  rev_zeroes = 2,
  ret_nums = 2,
  expected_down = FALSE,
  verbose = FALSE,
  detailed = FALSE
) {
  # Helper function to check for zero patterns
    # Helper function to check for zero patterns
  check_zero_patterns <- function(y, rev_zeroes, ret_nums) {
    revs <- NA
    rets <- NA

    # --- Reversal Check ---
    if (any(y == 0)) {
      search_vec_rev <- y
      # If data starts at zero, only search after the first non-zero value
      if (y[1] == 0 && !all(y == 0)) {
        search_vec_rev <- y[min(which(y != 0)):length(y)]
      }

      pattern_str_rev <- paste0(as.numeric(search_vec_rev != 0), collapse = "")

      # gregexpr returns c(-1) on no match, which has a length of 1.
      # Must check the value inside the vector to correctly identify "no match".
      rev_positions <- gregexpr(
        paste0(c(rep(0, rev_zeroes), 1), collapse = ""),
        pattern_str_rev
      )[[1]]

      revs <- if (rev_positions[1] < 0) 0 else length(rev_positions)
    } else {
      revs <- 0 # If no zeroes, no reversals are possible
    }

    # --- Returns Check ---
    if (any(y == 0)) {
      # For returns, always start searching from the first occurrence of zero
      search_vec_ret <- y[min(which(y == 0)):length(y)]
      pattern_str_ret <- paste0(as.numeric(search_vec_ret != 0), collapse = "")

      ret_positions <- gregexpr(
        paste0(c(rep(1, ret_nums), 0), collapse = ""),
        pattern_str_ret
      )[[1]]

      rets <- if (ret_positions[1] < 0) 0 else length(ret_positions)
    } else {
      rets <- 0 # If no zeroes, no returns are possible
    }

    list(revs = revs, rets = rets)
  }

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

  rev_ret <- check_zero_patterns(data$y, rev_zeroes, ret_nums)
  revs <- if (delta_up) NA else rev_ret$revs
  rets <- if (expected_down || delta_down) NA else rev_ret$rets

  # Initialize bounce variables
  bounce_up <- NA
  bounce_down <- NA
  bounce_none <- NA
  bounce_above <- 0
  bounce_below <- 0

  if (expected_down) {
  bounce_above <- sum(diff(data_ordered$y) > (first_y * 0.25), na.rm = TRUE)
  bounce_up <- (bounce_above / (data_length - 1)) > bounce_up_threshold

  bounce_down <- NA
  bounce_none <- NA

  if (verbose) {
    bounce_proportion <- bounce_above / (data_length - 1)
    print(paste("Bounce up proportion (expected_down=TRUE):", round(bounce_proportion, 4)))
  }

} else {
  if (delta_down) {
    bounce_above <- sum(diff(data_ordered$y) > first_y * 0.25, na.rm = TRUE)
    bounce_up <- (bounce_above / (data_length - 1)) > bounce_up_threshold

    if (verbose) {
      bounce_proportion <- bounce_above / (data_length - 1)
      print(paste("Bounce up proportion:", round(bounce_proportion, 4)))
    }
  }

  if (delta_up) {
    bounce_below <- sum(-diff(data_ordered$y) > last_y * 0.25, na.rm = TRUE)
    bounce_down <- (bounce_below / (data_length - 1)) > bounce_down_threshold

    if (verbose) {
      bounce_proportion <- bounce_below / (data_length - 1)
      print(paste("Bounce down proportion:", round(bounce_proportion, 4)))
    }
  }

  if (delta_none) {
    max_endpoint_y <- max(c(first_y, last_y))
    bounce_threshold_value <- max_endpoint_y * 0.25

    # Count upward and downward bounces exceeding the threshold
    bounce_above <- sum(diff(data_ordered$y) > bounce_threshold_value, na.rm = TRUE)
    bounce_below <- sum(-diff(data_ordered$y) > bounce_threshold_value, na.rm = TRUE)

    # Calculate proportions for each direction
    proportion_above <- bounce_above / (data_length - 1)
    proportion_below <- bounce_below / (data_length - 1)

    bounce_none <- min(proportion_above, proportion_below) > bounce_none_threshold

    if (verbose) {
      print(paste("Bounce above proportion (no trend):", round(proportion_above, 4)))
      print(paste("Bounce below proportion (no trend):", round(proportion_below, 4)))
    }
  }
}

  # Check if any bounce type is detected
  bounce_any <- any(c(bounce_up, bounce_down, bounce_none), na.rm = TRUE)

  # Determine direction as a single categorical variable
  delta_direction <- ifelse(delta_down, "down", ifelse(delta_up, "up", "none"))

  # Determine bounce direction as a single categorical variable
  bounce_direction <- NA
  if (delta_down && !is.na(bounce_up)) {
    bounce_direction <- ifelse(bounce_up, "up", "none")
  } else if (delta_up && !is.na(bounce_down)) {
    bounce_direction <- ifelse(bounce_down, "down", "none")
  } else if (delta_none && !is.na(bounce_none)) {
    bounce_direction <- ifelse(bounce_none, "significant", "none")
  }

  # Return results
  result <- data.frame(
    delta_direction = delta_direction,
    bounce_direction = bounce_direction,
    bounce_any = unname(bounce_any),
    bounce_above = unname(bounce_above),
    bounce_below = unname(bounce_below),
    reversals = revs,
    returns = rets
  )

  if (detailed) {
    result <- cbind(
      result,
      data.frame(
        delta_down = delta_down,
        delta_up = delta_up,
        delta_none = delta_none,
        bounce_up = bounce_up,
        bounce_down = bounce_down,
        bounce_none = bounce_none
      )
    )
  }

  # Add class for proper S3 dispatch

  class(result) <- c("cp_unsystematic", class(result))
  return(result)
}
