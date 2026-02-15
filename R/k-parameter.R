#' Calculate K Scaling Parameter for Demand Curve Fitting
#'
#' @description
#' Calculates the k scaling parameter used in demand curve equations to normalize
#' consumption across different units or ranges. The k value is derived from the
#' logarithmic range of consumption values.
#'
#' This is the modern replacement for [GetK()], with explicit parameters for
#' the adjustment value and optional verbose output.
#'
#' @param data A data frame in long format with columns for price and consumption
#' @param use_means Logical indicating whether to calculate k from mean consumption
#'   by price (TRUE, default) or from individual consumption values (FALSE)
#' @param adjustment Numeric adjustment added to the log range (default: 0.5).
#'   This value ensures k is slightly larger than the observed range.
#' @param x_var Character string specifying the column name for price (default: "x")
#' @param y_var Character string specifying the column name for consumption (default: "y")
#' @param verbose Logical indicating whether to print calculation details (default: FALSE)
#'
#' @return A single numeric value representing the k scaling parameter
#'
#' @details
#' The k parameter is calculated as:
#'
#' \deqn{k = \log_{10}(\text{max}) - \log_{10}(\text{min}) + \text{adjustment}}{
#'       k = log10(max) - log10(min) + adjustment}
#'
#' where max and min are the maximum and minimum non-zero consumption values.
#'
#' ## Use in Demand Equations
#'
#' The k parameter appears in several demand curve equations:
#' \itemize{
#'   \item \strong{Hursh & Silberberg (2008)}: Scales the exponential term
#'   \item \strong{Koffarnus et al. (2015)}: Normalizes the exponentiated model
#'   \item Ensures numerical stability during model fitting
#' }
#'
#' ## Calculation Modes
#'
#' \itemize{
#'   \item \strong{use_means = TRUE} (default): Calculates k from mean consumption
#'     at each price point. Recommended when data has multiple subjects, as it
#'     reduces influence of individual outliers.
#'
#'   \item \strong{use_means = FALSE}: Calculates k from the full range of individual
#'     consumption values. May be preferable for single-subject data or when
#'     individual variability is theoretically important.
#' }
#'
#' @note
#' \itemize{
#'   \item Only non-zero consumption values are used (zero values are excluded)
#'   \item Missing values (NA) are automatically removed via `na.rm = TRUE`
#'   \item The default adjustment of 0.5 is conventional but can be modified
#' }
#'
#' @seealso
#' \itemize{
#'   \item [GetK()] - Legacy function (superseded)
#'   \item [FitCurves()] - Uses k parameter in demand curve fitting
#' }
#'
#' @examples
#' \donttest{
#' data(apt, package = "beezdemand")
#'
#' # Calculate k using default settings (mean range + 0.5)
#' k_val <- get_k(apt)
#'
#' # Calculate k from individual values
#' k_ind <- get_k(apt, use_means = FALSE)
#'
#' # Calculate with custom adjustment
#' k_custom <- get_k(apt, adjustment = 1.0)
#'
#' # Show calculation details
#' k_verbose <- get_k(apt, verbose = TRUE)
#' }
#'
#' @export
get_k <- function(data,
                  use_means = TRUE,
                  adjustment = 0.5,
                  x_var = "x",
                  y_var = "y",
                  verbose = FALSE) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }

  required_cols <- c(x_var, y_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Rename columns for internal processing
  df <- data
  names(df)[names(df) == x_var] <- "x"
  names(df)[names(df) == y_var] <- "y"

  # Calculate k
  if (use_means) {
    # Calculate mean consumption by price
    dat1 <- aggregate(y ~ x, df, mean)
    y_vals <- dat1$y[dat1$y > 0]

    if (length(y_vals) == 0) {
      stop("No positive consumption values found after aggregating by price",
           call. = FALSE)
    }

    max_val <- max(y_vals, na.rm = TRUE)
    min_val <- min(y_vals, na.rm = TRUE)

    if (verbose) {
      cat("Calculating k from mean consumption values:\n")
      cat(sprintf("  Max mean: %.2f\n", max_val))
      cat(sprintf("  Min mean: %.2f\n", min_val))
      cat(sprintf("  log10(%.2f) - log10(%.2f) + %.2f = %.3f\n",
                  max_val, min_val, adjustment,
                  log10(max_val) - log10(min_val) + adjustment))
    }

    k <- log10(max_val) - log10(min_val) + adjustment

  } else {
    # Use individual consumption values
    y_vals <- df$y[df$y > 0]

    if (length(y_vals) == 0) {
      stop("No positive consumption values found in data", call. = FALSE)
    }

    max_val <- max(y_vals, na.rm = TRUE)
    min_val <- min(y_vals, na.rm = TRUE)

    if (verbose) {
      cat("Calculating k from individual consumption values:\n")
      cat(sprintf("  Max: %.2f\n", max_val))
      cat(sprintf("  Min: %.2f\n", min_val))
      cat(sprintf("  log10(%.2f) - log10(%.2f) + %.2f = %.3f\n",
                  max_val, min_val, adjustment,
                  log10(max_val) - log10(min_val) + adjustment))
    }

    k <- log10(max_val) - log10(min_val) + adjustment
  }

  return(k)
}
