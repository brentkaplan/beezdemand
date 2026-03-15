#' Calculate Descriptive Statistics by Price
#'
#' @description
#' Calculates summary statistics for consumption data at each price point,
#' including measures of central tendency (mean, median), variability (SD),
#' range (min, max), and data quality (proportion of zeros, missing values).
#'
#' This is the modern replacement for [GetDescriptives()], returning a structured
#' S3 object with dedicated methods for printing, summarizing, and visualizing.
#'
#' @param data A data frame in long format with columns for subject ID, price, and consumption
#' @param x_var Character string specifying the column name for price (default: "x")
#' @param y_var Character string specifying the column name for consumption (default: "y")
#' @param id_var Character string specifying the column name for subject ID (default: "id")
#' @param by Optional character vector of column names to group by.
#'   When supplied, statistics are computed separately within each unique
#'   combination of the `by` columns. Group columns are prepended to
#'   `$statistics` and `$data`. Default `NULL` (no grouping).
#'
#' @return An S3 object of class `beezdemand_descriptive` containing:
#' \itemize{
#'   \item \strong{statistics} - Data frame with 8 columns (Price, Mean, Median, SD,
#'     PropZeros, NAs, Min, Max) and one row per unique price
#'   \item \strong{call} - The matched call
#'   \item \strong{data_summary} - List with n_subjects, n_prices, and prices vector
#' }
#'
#' @details
#' For each unique price in the dataset, the function calculates:
#' \itemize{
#'   \item \strong{Mean} - Average consumption across subjects (rounded to 2 decimals)
#'   \item \strong{Median} - Median consumption (rounded to 2 decimals)
#'   \item \strong{SD} - Standard deviation (rounded to 2 decimals)
#'   \item \strong{PropZeros} - Proportion of subjects with zero consumption (0-1)
#'   \item \strong{NAs} - Count of missing values
#'   \item \strong{Min} - Minimum consumption value (rounded to 2 decimals)
#'   \item \strong{Max} - Maximum consumption value (rounded to 2 decimals)
#' }
#'
#' @seealso
#' \itemize{
#'   \item [GetDescriptives()] - Legacy function (superseded)
#'   \item [plot.beezdemand_descriptive()] - Visualization method
#'   \item [summary.beezdemand_descriptive()] - Extended summary
#' }
#'
#' @examples
#' \donttest{
#' data(apt, package = "beezdemand")
#'
#' # Calculate descriptive statistics
#' desc <- get_descriptive_summary(apt)
#' print(desc)
#'
#' # View statistics table
#' desc$statistics
#'
#' # Create visualization
#' plot(desc)
#'
#' # Extended summary with distribution info
#' summary(desc)
#'
#' # Grouped summary — statistics and plots faceted by group
#' data(apt_full)
#' desc_g <- get_descriptive_summary(apt_full, by = "gender")
#' desc_g$statistics
#' plot(desc_g)
#' }
#'
#' @export
get_descriptive_summary <- function(data,
                                    x_var = "x",
                                    y_var = "y",
                                    id_var = "id",
                                    by = NULL) {
  call <- match.call()

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }

  required_cols <- c(x_var, y_var, id_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # --- grouped dispatch ---
  if (!is.null(by)) {
    split_out <- beezdemand_split_by(data, by, function(slice, key_row) {
      obj <- get_descriptive_summary(
        data = slice,
        x_var = x_var,
        y_var = y_var,
        id_var = id_var,
        by = NULL
      )
      # Prepend group columns to statistics
      for (col in rev(by)) {
        obj$statistics <- tibble::add_column(
          tibble::as_tibble(obj$statistics), !!col := key_row[[col]], .before = 1
        )
      }
      # For data: group cols may already exist (from the slice); add only if missing
      obj$data <- tibble::as_tibble(obj$data)
      for (col in rev(by)) {
        if (!col %in% names(obj$data)) {
          obj$data <- tibble::add_column(obj$data, !!col := key_row[[col]], .before = 1)
        }
      }
      obj
    })

    # Combine statistics and data across groups
    combined_stats <- dplyr::bind_rows(
      lapply(split_out$results, function(obj) obj$statistics)
    )
    combined_data <- dplyr::bind_rows(
      lapply(split_out$results, function(obj) obj$data)
    )
    total_subjects <- sum(vapply(
      split_out$results,
      function(obj) obj$data_summary$n_subjects,
      integer(1)
    ))

    result <- structure(
      list(
        statistics = combined_stats,
        call = call,
        data_summary = list(
          n_subjects = total_subjects,
          n_prices = length(unique(combined_stats$Price)),
          prices = unique(combined_data$x)
        ),
        data = combined_data,
        by_var = by
      ),
      class = "beezdemand_descriptive"
    )
    return(result)
  }

  # --- ungrouped (original logic) ---

  # Rename columns for internal processing
  df <- data
  names(df)[names(df) == x_var] <- "x"
  names(df)[names(df) == y_var] <- "y"
  names(df)[names(df) == id_var] <- "id"

  # Get unique prices
  prices <- unique(df$x)
  np <- length(prices)

  # Initialize results data frame
  cnames <- c("Price", "Mean", "Median", "SD", "PropZeros", "NAs", "Min", "Max")
  dfres <- data.frame(
    matrix(vector(), np, length(cnames), dimnames = list(c(), cnames)),
    stringsAsFactors = FALSE
  )

  # Clean price labels (remove "X" prefix if present)
  dfres$Price <- gsub("X", "", as.character(prices))

  # Calculate statistics
  dfres[, "Mean"] <- aggregate(y ~ x, df, function(x) round(mean(x, na.rm = TRUE), 2))$y
  dfres[, "Median"] <- aggregate(y ~ x, df, function(x) round(median(x, na.rm = TRUE), 2))$y
  dfres[, "SD"] <- aggregate(y ~ x, df, function(x) round(sd(x, na.rm = TRUE), 2))$y
  dfres[, "PropZeros"] <- aggregate(y ~ x, df, function(x) round(sum(x == 0, na.rm = TRUE) / length(x), 2))$y
  dfres[, "NAs"] <- aggregate(y ~ x, df, function(x) sum(is.na(x)))$y
  dfres[, "Min"] <- aggregate(y ~ x, df, function(x) round(min(x, na.rm = TRUE), 2))$y
  dfres[, "Max"] <- aggregate(y ~ x, df, function(x) round(max(x, na.rm = TRUE), 2))$y

  # Count subjects
  n_subjects <- length(unique(df$id))

  # Build S3 object (store processed data for plotting)
  result <- structure(
    list(
      statistics = dfres,
      call = call,
      data_summary = list(
        n_subjects = n_subjects,
        n_prices = np,
        prices = prices
      ),
      data = tibble::as_tibble(df)  # Store for plot method
    ),
    class = "beezdemand_descriptive"
  )

  return(result)
}
