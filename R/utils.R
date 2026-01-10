##
## Copyright 2016 Brent Kaplan
##
## This file is part of beezdemand.
##
## beezdemand is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## beezdemand is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with beezdemand.  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.
##
## summary
## R script for utility functions
##

utils::globalVariables(c(
  "target"
))

#-------------------------------------------------------------------------------
# Standardized error helpers for consistent error messaging across the package
#-------------------------------------------------------------------------------

#' Signal a validation error
#'
#' Internal helper for signaling validation errors with consistent formatting.
#' Uses rlang::abort for structured error handling.
#'
#' @param message Error message describing what went wrong.
#' @param arg Optional name of the argument that caused the error.
#' @param call The calling environment for error reporting.
#' @param class Additional error classes to add (will be prefixed with "beezdemand_").
#' @noRd
validation_error <- function(message, arg = NULL, call = rlang::caller_env(), class = NULL) {
  if (!is.null(arg)) {
    message <- paste0("Problem with argument `", arg, "`: ", message)
  }
  error_class <- c(
    if (!is.null(class)) paste0("beezdemand_", class),
    "beezdemand_validation_error"
  )
  rlang::abort(message, class = error_class, call = call)
}

#' Signal a model fitting error
#'
#' Internal helper for signaling model fitting errors.
#'
#' @param message Error message.
#' @param model_type Type of model that failed (e.g., "nls", "nlme", "tmb").
#' @param call The calling environment.
#' @noRd
fitting_error <- function(message, model_type = NULL, call = rlang::caller_env()) {
  if (!is.null(model_type)) {
    message <- paste0("[", model_type, "] ", message)
  }
  rlang::abort(message, class = c("beezdemand_fitting_error", "beezdemand_error"), call = call)
}

#' Signal a missing package error
#'
#' Internal helper for signaling that a required package is missing.
#'
#' @param pkg Package name.
#' @param reason Why the package is needed.
#' @param call The calling environment.
#' @noRd
missing_package_error <- function(pkg, reason = NULL, call = rlang::caller_env()) {
  message <- paste0("Package '", pkg, "' is required")
  if (!is.null(reason)) {
    message <- paste0(message, " ", reason)
  }
  message <- paste0(message, ". Please install it with: install.packages('", pkg, "')")
  rlang::abort(message, class = c("beezdemand_missing_package", "beezdemand_error"), call = call)
}

##' Pull vector from data frame
##'
##' @description
##' `r lifecycle::badge("deprecated")`
##'
##' Pulls a single vector from a data frame. Good to use with dplyr.
##' From http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
##' @title Pull
##' @param x A data frame
##' @param y Name of column
##' @return Vector
##' @importFrom lifecycle deprecate_warn
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
pull <- function(x, y) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "beezdemand::pull()",
    "dplyr::pull()",
    details = "Please use dplyr::pull() instead, which provides similar functionality."
  )

  if (ncol(x) == 1) {
    y <- 1
  } else {
    y
  }
  x[, if (is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[
    1
  ]]
}

# Trim Leading Characters
# x A string
##' @noRd
trim.leading <- function(x) sub("^\\s+", "", x)

##' Checks to ensure column names are specified
##'
##' Check column names
##' @title Check Column Names
##' @param dat Dataframe
##' @param xcol Name of x column
##' @param ycol Name of y column
##' @param idcol Name of id column
##' @param groupcol Name of group column
##' @return Dataframe
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
CheckCols <- function(dat, xcol, ycol, idcol, groupcol = NULL) {
  dat <- if (dplyr::is.tbl(dat)) {
    print("Data casted as data.frame")
    dat <- as.data.frame(dat)
  } else {
    dat
  }

  dat[, xcol] <- if (!is.numeric(dat[, xcol])) {
    as.numeric(dat[, xcol])
  } else {
    dat[, xcol]
  }
  dat[, ycol] <- if (!is.numeric(dat[, ycol])) {
    as.numeric(dat[, ycol])
  } else {
    dat[, ycol]
  }

  if (any(is.na(dat[, ycol]))) {
    warning("NA values found in ", ycol, " column. Dropping NAs and continuing")
    dat <- dat[!is.na(dat[, ycol]), ]
  }

  if (
    any(colnames(dat) %in% "x") &&
      any(colnames(dat) %in% "y") &&
      any(colnames(dat) %in% "id")
  ) {} else if (
    any(colnames(dat) %in% xcol) &&
      any(colnames(dat) %in% ycol) &&
      any(colnames(dat) %in% idcol)
  ) {
    if (!any(colnames(dat) %in% "x") && any(colnames(dat) %in% xcol)) {
      colnames(dat) <- gsub(xcol, "x", colnames(dat))
    }
    if (!any(colnames(dat) %in% "y") && any(colnames(dat) %in% ycol)) {
      colnames(dat) <- gsub(ycol, "y", colnames(dat))
    }
    if (!any(colnames(dat) %in% "id") && any(colnames(dat) %in% idcol)) {
      colnames(dat) <- gsub(idcol, "id", colnames(dat))
    }
  } else {
    stop("Can't find x, y, and id column names in data!", call. = FALSE)
  }

  if (!is.null(groupcol) && any(colnames(dat) %in% groupcol)) {
    colnames(dat) <- gsub(groupcol, "group", colnames(dat))
  } else if (!is.null(groupcol) && !any(colnames(dat) %in% "group")) {
    stop("Can't find groupcol column name in data!", call. = FALSE)
  } else if (
    !is.null(groupcol) &&
      any(colnames(dat) %in% "group") &&
      !any(colnames(dat) %in% groupcol)
  ) {
    stop(
      "Groupcol does not match column names. Column name 'group' was found and will be used.",
      call. = FALSE
    )
  }

  return(dat)
}

##' Ben Bolker's port of Lambert W from GNU Scientific Library (GPLV3)
##'
##' Ben Bolker's port of Lambert W from GNU Scientific Library
##' @title Lambert W
##' @param z input value
##' @param b branch, set to principal by default
##' @param maxiter Halley iteration count
##' @param eps error precision
##' @param min.imag minimum for imaginary solution
##' @return numeric
##' @author Benjamin Bolker (port)
##' @export
lambertW = function(
  z,
  b = 0,
  maxiter = 10,
  eps = .Machine$double.eps,
  min.imag = 1e-9
) {
  if (any(round(Re(b)) != b)) {
    stop("branch number for W must be an integer")
  }
  if (!is.complex(z) && any(z < 0)) {
    z = as.complex(z)
  }
  ## series expansion about -1/e
  ##
  ## p = (1 - 2*abs(b)).*sqrt(2*e*z + 2);
  ## w = (11/72)*p;
  ## w = (w - 1/3).*p;
  ## w = (w + 1).*p - 1
  ##
  ## first-order version suffices:
  ##
  w = (1 - 2 * abs(b)) * sqrt(2 * exp(1) * z + 2) - 1
  ## asymptotic expansion at 0 and Inf
  ##
  v = log(z + as.numeric(z == 0 & b == 0)) + 2 * pi * b * 1i
  v = v - log(v + as.numeric(v == 0))
  ## choose strategy for initial guess
  ##
  c = abs(z + exp(-1))
  c = (c > 1.45 - 1.1 * abs(b))
  c = c | (b * Im(z) > 0) | (!Im(z) & (b == 1))
  w = (1 - c) * w + c * v
  ## Halley iteration
  ##
  for (n in 1:maxiter) {
    p = exp(w)
    t = w * p - z
    f = (w != -1)
    t = f * t / (p * (w + f) - 0.5 * (w + 2.0) * t / (w + f))
    w = w - t
    if (
      abs(Re(t)) < (2.48 * eps) * (1.0 + abs(Re(w))) &&
        abs(Im(t)) < (2.48 * eps) * (1.0 + abs(Im(w)))
    ) {
      break
    }
  }
  if (n == maxiter) {
    warning(paste(
      "iteration limit (",
      maxiter,
      ") reached, result of W may be inaccurate",
      sep = ""
    ))
  }
  if (all(Im(w) < min.imag)) {
    w = as.numeric(w)
  }
  return(w)
}

#-------------------------------------------------------------------------------
#' Validate and Filter Cross-Price Demand Data
#'
#' This function performs validation checks on cross-price demand data and applies
#' filtering if specified. It ensures the data meets the requirements for analysis
#' by checking for required columns, filtering by target type if needed, and
#' confirming ID column presence when required.
#'
#' @param data A data frame containing cross-price demand data.
#' @param required_cols Character vector of required column names.
#'   Default is c("x", "y").
#' @param filter_target Logical; if TRUE and data contains a "target" column,
#'   filters to keep only rows where target == "alt". Default is TRUE.
#' @param require_id Logical; if TRUE, validates that an "id" column exists
#'   in the data. Default is FALSE.
#'
#' @return A validated (and potentially filtered) data frame.
#'
#' @examples
#' \dontrun{
#' # Basic validation requiring x and y columns
#' validated_data <- validate_cp_data(my_data)
#'
#' # Require additional columns
#' validated_data <- validate_cp_data(my_data,
#'                                   required_cols = c("x", "y", "price"))
#'
#' # Require ID column and don't filter by target
#' validated_data <- validate_cp_data(my_data,
#'                                   filter_target = FALSE,
#'                                   require_id = TRUE)
#' }
#'
#' @keywords internal
validate_cp_data <- function(
  data,
  required_cols = c("x", "y"),
  filter_target = TRUE,
  require_id = FALSE
) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (filter_target && "target" %in% names(data)) {
    data <- dplyr::filter(data, target == "alt")
    if (nrow(data) == 0) {
      stop("No data with target = 'alt' found in the provided data.")
    }
  }

  if (require_id && !("id" %in% names(data))) {
    stop("Data must contain an 'id' column for this operation.")
  }

  return(data)
}


# #' @keywords internal
# validate_demand_data <- function(
#   data,
#   required_cols = c("x", "y"),
#   require_id = FALSE,
#   drop_unused_factors = TRUE
# ) {
#   if (!is.data.frame(data)) {
#     stop("Data must be a data frame.")
#   }

#   missing_cols <- setdiff(required_cols, names(data))
#   if (length(missing_cols) > 0) {
#     stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
#   }

#   if (require_id && !("id" %in% names(data))) {
#     stop("Data must contain an 'id' column for this operation.")
#   }

#   # Drop unused factor levels if requested
#   if (drop_unused_factors) {
#     factor_cols <- which(vapply(data, is.factor, logical(1)))
#     if (length(factor_cols) > 0) {
#       data[factor_cols] <- lapply(data[factor_cols], droplevels)
#     }
#   }

#   return(data)
# }

#' Validate and Prepare Demand Data
#'
#' Internal helper function to validate required columns in demand data and
#' ensure that specified factor columns are correctly formatted and have unused
#' levels dropped.
#'
#' @param data A data frame.
#' @param y_var Character string, the name of the dependent variable column.
#' @param x_var Character string, the name of the independent variable column.
#' @param id_var Character string, the name of the subject/group identifier column.
#' @param factors Character vector of factor names (can be NULL).
#'
#' @return The validated and prepared data frame.
#' @keywords internal
validate_demand_data <- function(
  data,
  y_var,
  x_var,
  id_var,
  factors = NULL
) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # Construct required columns list
  required_cols <- c(y_var, x_var, id_var)
  if (!is.null(factors)) {
    required_cols <- c(required_cols, factors)
  }

  # Check for missing columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in data: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Ensure id_var is a factor and drop unused levels
  data[[id_var]] <- droplevels(as.factor(data[[id_var]]))

  # Ensure all specified factors are factors and drop unused levels
  if (!is.null(factors)) {
    for (f_col in factors) {
      if (!is.factor(data[[f_col]])) {
        # Only convert if not already a factor
        data[[f_col]] <- as.factor(data[[f_col]])
      }
      data[[f_col]] <- droplevels(data[[f_col]])
    }
  }

  return(data)
}

#' @keywords internal
#' small infix helper (define once in your utilities)
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Collapse Factor Levels for a Specific Parameter
#'
#' Internal helper to apply level collapsing for a single parameter (Q0 or alpha).
#' Creates new columns with suffix to avoid modifying original factor columns.
#'
#' @param data A data frame.
#' @param collapse_spec Named list of factor collapse specifications.
#'   Structure: `list(factor_name = list(new_level = c(old_levels), ...))`.
#' @param factors Character vector of factor names in the model.
#' @param suffix Character suffix for new column names (e.g., "Q0" or "alpha").
#'
#' @return A list with:
#'   - `data`: Modified data frame with new collapsed factor columns
#'   - `new_factor_names`: Character vector of new factor column names to use
#'   - `info`: List with original and new levels for each collapsed factor
#'
#' @keywords internal
collapse_factor_levels <- function(data, collapse_spec, factors, suffix) {
  if (!is.list(collapse_spec) || is.null(names(collapse_spec))) {
    stop(
      "Collapse specification for '",
      suffix,
      "' must be a named list of factor mappings."
    )
  }

  new_factor_names <- factors
  info <- list()

  for (factor_col in names(collapse_spec)) {
    # Validate factor is in the model
    if (!factor_col %in% factors) {
      warning(
        "Factor '",
        factor_col,
        "' in collapse_levels$",
        suffix,
        " is not in the 'factors' list. Skipping."
      )
      next
    }

    # Validate factor is in the data
    if (!factor_col %in% names(data)) {
      warning(
        "Factor '",
        factor_col,
        "' not found in data. Skipping."
      )
      next
    }

    level_map <- collapse_spec[[factor_col]]

    if (!is.list(level_map) || is.null(names(level_map))) {
      stop(
        "Collapse mapping for factor '",
        factor_col,
        "' must be a named list (new_level = c(old_levels))."
      )
    }

    # Check for overlapping old levels
    all_old_levels <- unlist(level_map, use.names = FALSE)
    if (length(all_old_levels) != length(unique(all_old_levels))) {
      duplicates <- all_old_levels[duplicated(all_old_levels)]
      stop(
        "Overlapping old levels detected in collapse mapping for '",
        factor_col,
        "': ",
        paste(unique(duplicates), collapse = ", "),
        ". Each old level can only map to one new level."
      )
    }

    # Store original levels
    original_levels <- levels(data[[factor_col]])
    info[[factor_col]] <- list(original_levels = original_levels)

    # Create new column name

    new_col_name <- paste0(factor_col, "_", suffix)

    # Apply level mapping
    new_factor_values <- as.character(data[[factor_col]])
    for (new_level_name in names(level_map)) {
      old_levels_to_map <- level_map[[new_level_name]]
      new_factor_values[
        new_factor_values %in% old_levels_to_map
      ] <- new_level_name
    }

    # Create new column with collapsed levels

    data[[new_col_name]] <- droplevels(factor(new_factor_values))
    info[[factor_col]]$new_levels <- levels(data[[new_col_name]])
    info[[factor_col]]$new_col_name <- new_col_name

    # Update factor names to use the new column
    new_factor_names[new_factor_names == factor_col] <- new_col_name
  }

  list(
    data = data,
    new_factor_names = new_factor_names,
    info = info
  )
}


#' Build Fixed-Effects RHS Formula String
#'
#' Internal helper to construct the right-hand side of a fixed-effects formula
#' from factors, interaction flag, and continuous covariates.
#'
#' @param factors Character vector of factor names (can be NULL).
#' @param factor_interaction Logical. If TRUE and two factors provided,
#'   include their interaction.
#' @param continuous_covariates Character vector of continuous covariate names.
#'
#' @return A character string representing the RHS (e.g., "~ 1 + dose + drug").
#'
#' @keywords internal
build_fixed_rhs <- function(
  factors = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL,
  data = NULL
) {
  rhs_parts <- c()

  # Filter out factors with only 1 level (they don't contribute to contrasts)
  valid_factors <- factors
  if (!is.null(factors) && !is.null(data)) {
    valid_factors <- vapply(
      factors,
      function(f) {
        if (f %in% names(data) && is.factor(data[[f]])) {
          nlevels(data[[f]]) >= 2
        } else {
          TRUE # Keep non-factor or missing columns (will error elsewhere)
        }
      },
      logical(1)
    )
    valid_factors <- factors[valid_factors]

    # Warn about dropped single-level factors
    dropped <- setdiff(factors, valid_factors)
    if (length(dropped) > 0) {
      message(
        "Note: Factor(s) with only 1 level removed from formula: ",
        paste(dropped, collapse = ", ")
      )
    }
  }

  if (!is.null(valid_factors) && length(valid_factors) > 0) {
    if (length(valid_factors) == 1) {
      rhs_parts <- c(rhs_parts, valid_factors[1])
    } else if (length(valid_factors) >= 2) {
      if (isTRUE(factor_interaction)) {
        rhs_parts <- c(
          rhs_parts,
          paste0(valid_factors[1], "*", valid_factors[2])
        )
      } else {
        rhs_parts <- c(rhs_parts, valid_factors[1], valid_factors[2])
      }
    }
  }

  if (!is.null(continuous_covariates) && length(continuous_covariates) > 0) {
    rhs_parts <- c(rhs_parts, continuous_covariates)
  }

  if (length(rhs_parts) > 0) {
    paste("~", paste(rhs_parts, collapse = " + "))
  } else {
    "~ 1"
  }
}


#' Validate Collapse Levels Structure
#'
#' Internal helper to validate the structure of collapse_levels argument.
#'
#' @param collapse_levels The collapse_levels argument from fit_demand_mixed.
#'
#' @return TRUE if valid, otherwise stops with an error message.
#'
#' @keywords internal
validate_collapse_levels <- function(collapse_levels) {
  if (is.null(collapse_levels)) {
    return(TRUE)
  }

  if (!is.list(collapse_levels)) {
    stop(
      "'collapse_levels' must be a named list with keys 'Q0' and/or
'alpha'."
    )
  }

  valid_keys <- c("Q0", "alpha")
  provided_keys <- names(collapse_levels)

  if (is.null(provided_keys) || length(provided_keys) == 0) {
    stop(
      "'collapse_levels' must have named elements. ",
      "Expected keys: 'Q0' and/or 'alpha'."
    )
  }

  invalid_keys <- setdiff(provided_keys, valid_keys)
  if (length(invalid_keys) > 0) {
    stop(
      "Invalid keys in 'collapse_levels': ",
      paste(invalid_keys, collapse = ", "),
      ". ",
      "Only 'Q0' and 'alpha' are allowed."
    )
  }

  TRUE
}


#-------------------------------------------------------------------------------
# Base class for summary objects
#-------------------------------------------------------------------------------

beezdemand_empty_coefficients <- function() {
  tibble::tibble(
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    statistic = numeric(),
    p.value = numeric(),
    component = character()
  )
}

beezdemand_empty_derived_metrics <- function() {
  tibble::tibble(
    metric = character(),
    estimate = numeric(),
    std.error = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    method = character(),
    component = character(),
    level = character(),
    id = character()
  )
}

#' Print Method for beezdemand Summary Objects
#'
#' Fallback print method for summary objects inheriting from `beezdemand_summary`.
#' Specific summary classes should implement their own `print.summary.*` methods
#' for detailed output; this provides a minimal fallback.
#'
#' @param x A summary object with class including `beezdemand_summary`.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.beezdemand_summary <- function(x, ...) {
  cat("\n")
  cat("Summary of", x$model_class %||% "beezdemand", "model\n")
  cat(strrep("-", 40), "\n")

  if (!is.null(x$backend) && !is.na(x$backend)) {
    cat("Backend:", x$backend, "\n")
  }

  if (!is.null(x$nobs) && !is.na(x$nobs)) {
    cat("Observations:", x$nobs)
    if (!is.null(x$n_subjects) && !is.na(x$n_subjects)) {
      cat(" | Subjects:", x$n_subjects)
    }
    cat("\n")
  }

  if (!is.null(x$converged) && !is.na(x$converged)) {
    cat("Converged:", x$converged, "\n")
  }

  if (!is.null(x$logLik) && !is.na(x$logLik)) {
    cat("Log-Lik:", round(x$logLik, 2))
    if (!is.null(x$AIC) && !is.na(x$AIC)) {
      cat(" | AIC:", round(x$AIC, 2))
    }
    if (!is.null(x$BIC) && !is.na(x$BIC)) {
      cat(" | BIC:", round(x$BIC, 2))
    }
    cat("\n")
  }

  if (!is.null(x$notes) && length(x$notes) > 0) {
    cat("\nNotes:\n")
    cat(paste(" -", x$notes), sep = "\n")
  }

  invisible(x)
}
