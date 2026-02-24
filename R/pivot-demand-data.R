#' @title Reshape Demand Data Between Wide and Long Formats
#'
#' @description Converts demand data between wide format (one row per subject,
#'   prices as columns) and long format (one row per observation with `id`, `x`,
#'   `y` columns). This is a convenience wrapper around [tidyr::pivot_longer()]
#'   and [tidyr::pivot_wider()] tailored for behavioral economic purchase task
#'   data.
#'
#' @param data A data frame or tibble to reshape.
#' @param format Character. Direction of reshaping: `"long"` (wide to long) or
#'   `"wide"` (long to wide). Default `"long"`.
#' @param id_var Character. Name of the subject/series identifier column.
#'   Default `"id"`.
#' @param x_var Character. Name of the price column in long-format data. Used
#'   when `format = "wide"`. Default `"x"`.
#' @param y_var Character. Name of the consumption column in long-format data.
#'   Used when `format = "wide"`. Default `"y"`.
#' @param price_cols Character vector of column names in wide-format data that
#'   represent prices. Used when `format = "long"`. Default `NULL`, which
#'   auto-detects: columns whose names parse as numeric become price columns,
#'   and remaining columns (besides `id_var`) are preserved as identifiers.
#'   If no column names parse as numeric, all non-`id_var` columns are treated
#'   as price columns (and `x_values` must be supplied).
#' @param x_values Numeric vector of actual price values corresponding to each
#'   price column in wide-format data. Used when `format = "long"`. Must be the
#'   same length as the number of price columns. Default `NULL`, which parses
#'   prices from column names.
#' @param drop_na Logical. When `format = "long"`, drop rows where consumption
#'   (`y`) is `NA` after pivoting? Default `TRUE`. A warning is issued when rows
#'   are dropped.
#'
#' @return A tibble. When `format = "long"`: columns `id`, any extra identifier
#'   columns, `x` (numeric price), and `y` (numeric consumption). When
#'   `format = "wide"`: one row per subject with prices as column names.
#'
#' @details
#' ## Wide to Long (`format = "long"`)
#'
#' The function determines which columns are price columns by:
#' 1. If `price_cols` is provided, those columns are used directly.
#' 2. If `price_cols` is `NULL`, column names are tested with `as.numeric()`.
#'    Columns whose names successfully parse as numbers (e.g., `"0"`, `"0.5"`,
#'    `"10"`) are treated as price columns. Remaining non-`id_var` columns are
#'    preserved as extra identifiers.
#' 3. If no column names parse as numeric, all non-`id_var` columns become price
#'    columns and `x_values` must be supplied.
#'
#' Actual numeric price values come from `x_values` if supplied, or from parsing
#' column names. If column names cannot be parsed and `x_values` is not supplied,
#' an error is raised.
#'
#' ## Long to Wide (`format = "wide"`)
#'
#' Pivots long data so that each unique value in `x_var` becomes a column, with
#' values from `y_var`. All columns except `x_var` and `y_var` are used as
#' identifiers.
#'
#' @examples
#' # --- Wide to long ---
#' # Columns named as prices (auto-parsed)
#' wide_num <- data.frame(
#'   id = 1:3,
#'   "0" = c(10, 8, 12), "0.5" = c(9, 7, 11), "1" = c(8, 6, 9),
#'   check.names = FALSE
#' )
#' pivot_demand_data(wide_num, format = "long")
#'
#' # Columns with non-numeric names require x_values
#' wide_named <- data.frame(id = 1:2, price_1 = c(10, 8), price_2 = c(5, 4))
#' pivot_demand_data(wide_named, format = "long",
#'                   x_values = c(0, 0.5))
#'
#' # --- Long to wide ---
#' data(apt, package = "beezdemand")
#' wide_apt <- pivot_demand_data(apt, format = "wide")
#' head(wide_apt)
#'
#' @export
#' @importFrom tidyr pivot_wider
pivot_demand_data <- function(
    data,
    format = c("long", "wide"),
    id_var = "id",
    x_var = "x",
    y_var = "y",
    price_cols = NULL,
    x_values = NULL,
    drop_na = TRUE
) {
  format <- match.arg(format)
  data <- as.data.frame(data)

  if (format == "long") {
    pivot_to_long(data, id_var = id_var, price_cols = price_cols,
                  x_values = x_values, drop_na = drop_na)
  } else {
    pivot_to_wide(data, id_var = id_var, x_var = x_var, y_var = y_var)
  }
}


# --- Internal: wide to long ---------------------------------------------------

#' @noRd
pivot_to_long <- function(data, id_var, price_cols, x_values, drop_na) {
  # Validate id_var

if (!(id_var %in% names(data))) {
    validation_error(
      paste0("Column '", id_var, "' not found in data."),
      arg = "id_var"
    )
  }

  # Determine price columns and extra id columns
  all_cols <- names(data)
  non_id <- setdiff(all_cols, id_var)

  if (!is.null(price_cols)) {
    # User specified price columns — validate they exist
    missing <- setdiff(price_cols, all_cols)
    if (length(missing) > 0) {
      validation_error(
        paste0("Column(s) not found in data: ",
               paste(missing, collapse = ", ")),
        arg = "price_cols"
      )
    }
    extra_id_cols <- setdiff(non_id, price_cols)
  } else {
    # Auto-detect: try as.numeric() on each column name
    parsed <- suppressWarnings(as.numeric(non_id))
    numeric_mask <- !is.na(parsed)

    if (any(numeric_mask)) {
      # Some names parse as numeric — those are price cols, rest are extra ids
      price_cols <- non_id[numeric_mask]
      extra_id_cols <- non_id[!numeric_mask]
    } else {
      # No names parse as numeric — all are price cols, x_values required
      price_cols <- non_id
      extra_id_cols <- character(0)
    }
  }

  # Determine x values
  if (!is.null(x_values)) {
    if (!is.numeric(x_values)) {
      validation_error("x_values must be a numeric vector.", arg = "x_values")
    }
    if (length(x_values) != length(price_cols)) {
      validation_error(
        paste0("Length of x_values (", length(x_values),
               ") must match number of price columns (",
               length(price_cols), ")."),
        arg = "x_values"
      )
    }
    price_map <- stats::setNames(x_values, price_cols)
  } else {
    price_map <- parse_prices_from_names(price_cols)
  }

  # Pivot
  id_cols_all <- c(id_var, extra_id_cols)
  long <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(price_cols),
    names_to = ".price_name",
    values_to = "y"
  )

  # Map column names to numeric prices
  long$x <- unname(price_map[long$.price_name])
  long$.price_name <- NULL

  # Rename id_var to "id" (if not already)
  if (id_var != "id") {
    names(long)[names(long) == id_var] <- "id"
  }

  # Coerce x and y to numeric (defensive)
  long$x <- coerce_numeric_warn(long$x, "x")
  long$y <- coerce_numeric_warn(long$y, "y")

  # Handle NAs in y
  na_count <- sum(is.na(long$y))
  if (na_count > 0) {
    if (drop_na) {
      warning("Dropped ", na_count, " rows with NA values in y.", call. = FALSE)
      long <- long[!is.na(long$y), ]
    } else {
      warning(na_count, " rows have NA values in y.", call. = FALSE)
    }
  }

  # Reorder columns: id, [extra cols], x, y
  id_out <- if (id_var != "id") "id" else id_var
  col_order <- c(id_out, extra_id_cols, "x", "y")
  long <- long[, col_order, drop = FALSE]

  tibble::as_tibble(long)
}


# --- Internal: long to wide ---------------------------------------------------

#' @noRd
pivot_to_wide <- function(data, id_var, x_var, y_var) {
  # Validate columns exist
  required <- c(id_var, x_var, y_var)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    validation_error(
      paste0("Column(s) not found in data: ",
             paste(missing, collapse = ", "))
    )
  }

  # id_cols = everything except x_var and y_var
  id_cols <- setdiff(names(data), c(x_var, y_var))

  tidyr::pivot_wider(
    data,
    id_cols = dplyr::all_of(id_cols),
    names_from = dplyr::all_of(x_var),
    values_from = dplyr::all_of(y_var)
  )
}


# --- Internal helpers ----------------------------------------------------------

#' Parse numeric prices from column name strings
#'
#' Wraps as.numeric() with a clear error if any names fail to parse.
#' @param nms Character vector of column names to parse as prices.
#' @return Named numeric vector (names = original column names, values = prices).
#' @noRd
parse_prices_from_names <- function(nms) {
  parsed <- suppressWarnings(as.numeric(nms))
  failed <- nms[is.na(parsed)]
  if (length(failed) > 0) {
    validation_error(
      paste0(
        "Cannot parse prices from column names: ",
        paste(failed, collapse = ", "),
        ". Please provide x_values."
      ),
      arg = "x_values"
    )
  }
  stats::setNames(parsed, nms)
}


#' Coerce a vector to numeric with a warning if NAs are introduced
#'
#' @param x Vector to coerce.
#' @param col_name Name of the column (for warning message).
#' @return Numeric vector.
#' @noRd
coerce_numeric_warn <- function(x, col_name) {
  if (is.numeric(x)) return(x)
  na_before <- sum(is.na(x))
  result <- suppressWarnings(as.numeric(x))
  na_after <- sum(is.na(result))
  new_na <- na_after - na_before
  if (new_na > 0) {
    warning(
      "Coercing '", col_name, "' to numeric introduced ", new_na, " NA(s).",
      call. = FALSE
    )
  }
  result
}
