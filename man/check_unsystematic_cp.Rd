% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/systematic.R
\name{check_unsystematic_cp}
\alias{check_unsystematic_cp}
\title{Check for Unsystematic Patterns in Cross-Price Data}
\usage{
check_unsystematic_cp(
  data,
  delta_threshold = 0.025,
  bounce_down_threshold = 0.1,
  bounce_up_threshold = 0.1,
  bounce_none_threshold = 0.2,
  verbose = FALSE
)
}
\arguments{
\item{data}{A data frame with columns 'x' and 'y', where 'x' typically represents
price and 'y' typically represents consumption or demand.}

\item{delta_threshold}{Numeric value (default 0.025) that determines what qualifies
as a significant trend in the data.}

\item{bounce_down_threshold}{Numeric value (default 0.1) that determines what
proportion of downward deviations qualifies as significant for upward trends.}

\item{bounce_up_threshold}{Numeric value (default 0.1) that determines what
proportion of upward deviations qualifies as significant for downward trends.}

\item{bounce_none_threshold}{Numeric value (default 0.2) that determines what
proportion of deviations qualifies as significant when there's no clear trend.}

\item{verbose}{Logical value (default FALSE) indicating whether to print
intermediate calculations.}
}
\value{
A data frame with the following columns:
  \describe{
    \item{delta_down}{Logical. TRUE if there's a significant downward trend.}
    \item{delta_up}{Logical. TRUE if there's a significant upward trend.}
    \item{delta_none}{Logical. TRUE if there's no significant trend.}
    \item{bounce_up}{Logical. TRUE if there are significant upward bounces in a downward trend. NA if not applicable.}
    \item{bounce_down}{Logical. TRUE if there are significant downward bounces in an upward trend. NA if not applicable.}
    \item{bounce_none}{Logical. TRUE if there are significant bounces in no-trend data. NA if not applicable.}
    \item{bounce_any}{Logical. TRUE if any type of bounce was detected.}
    \item{bounce_above}{Integer. Count of upward deviations meeting the threshold criteria.}
    \item{bounce_below}{Integer. Count of downward deviations meeting the threshold criteria.}
  }
}
\description{
Analyzes data patterns to determine whether there's a systematic relationship
between two variables (typically price and consumption), and checks for
significant deviations or "bounces" that might indicate unsystematic patterns.
This function is particularly useful for analyzing demand elasticity and
cross-commodity relationships in behavioral economics.
}
\details{
The function first determines if the data shows a significant upward trend,
downward trend, or no trend by calculating slopes in log-scale. It then
checks for "bounces" or significant deviations based on the identified trend type:

- For downward trends: looks for significant upward jumps
- For upward trends: looks for significant downward drops
- For no trends: looks for deviations both above and below the average

The function uses relative changes (percentage changes) rather than absolute
changes to detect bounces, making it suitable for data at different scales.
}
\examples{
# Create a sequence of x values for examples
x_seq <- 10^(seq(-2, 2, length.out = 10))

# Example 1: Flat pattern with some fluctuations
pattern1 <- data.frame(
  x = x_seq,
  y = c(10, 5, 10, 9, 10, 13, 10, 10, 7, 9)
)
check_unsystematic_cp(pattern1)

# Example 2: Upward trend with bounces
pattern2 <- data.frame(
  x = x_seq,
  y = 10 - c(10, 10, 7, 8, 2, 6, 3, 4, 10, 0) # increasing pattern
)
check_unsystematic_cp(pattern2)

# Example 3: Downward trend
pattern3 <- data.frame(
  x = x_seq,
  y = c(10, 10, 10, 9, 8, 5, 3, 2, 0, 0)
)
check_unsystematic_cp(pattern3)

# Example 4: No clear trend with significant deviations
pattern4 <- data.frame(
  x = x_seq,
  y = c(10, 10, 10, 9, 8, 5, 3, 2, 0, 10)
)
check_unsystematic_cp(pattern4)

# Example 5: Perfectly flat line
pattern8 <- data.frame(
  x = x_seq,
  y = rep(10, 10)
)
check_unsystematic_cp(pattern8)

# Analyzing multiple patterns at once
# First combine patterns into one dataframe with IDs
combined_df <- rbind(
  cbind(id = "pattern1", pattern1),
  cbind(id = "pattern2", pattern2),
  cbind(id = "pattern3", pattern3),
  cbind(id = "pattern4", pattern4),
  cbind(id = "pattern8", pattern8)
)

# Then analyze all patterns and combine results
results_list <- lapply(unique(combined_df$id), function(pattern_id) {
  pattern_data <- combined_df[combined_df$id == pattern_id, c("x", "y")]
  result <- check_unsystematic_cp(pattern_data)
  result$id <- pattern_id
  return(result)
})

# Combine all results
all_results <- do.call(rbind, results_list)
print(all_results[, c(
  "id",
  "delta_direction",
  "bounce_direction",
  "bounce_any"
)])

}
