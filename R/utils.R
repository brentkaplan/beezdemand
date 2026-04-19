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
## along with beezdemand.  If not, see <https://www.gnu.org/licenses/gpl-2.0.html>.
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
validation_error <- function(message, arg = NULL, call = rlang::caller_env(),
                             class = NULL, .envir = parent.frame()) {
  # Use cli::cli_abort so callers can pass inline markup ({.arg}, {.field},
  # {.val}, {.fn}, etc.) and per-bullet messages. .envir threads the original
  # caller's frame through so glue can resolve interpolations like {x[i]}.
  if (!is.null(arg)) {
    message <- c(
      paste0("Problem with argument {.arg ", arg, "}:"),
      message
    )
  }
  error_class <- c(
    if (!is.null(class)) paste0("beezdemand_", class),
    "beezdemand_validation_error"
  )
  cli::cli_abort(message, class = error_class, call = call, .envir = .envir)
}

#' Signal a model fitting error
#'
#' Internal helper for signaling model fitting errors.
#'
#' @param message Error message.
#' @param model_type Type of model that failed (e.g., "nls", "nlme", "tmb").
#' @param call The calling environment.
#' @noRd
fitting_error <- function(message, model_type = NULL, call = rlang::caller_env(),
                          .envir = parent.frame()) {
  if (!is.null(model_type)) {
    message <- c(
      paste0("[", model_type, "] ", if (length(message) > 0) message[[1]] else ""),
      if (length(message) > 1) message[-1]
    )
  }
  cli::cli_abort(message,
                 class = c("beezdemand_fitting_error", "beezdemand_error"),
                 call = call, .envir = .envir)
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
  msg_main <- paste0("Package {.pkg ", pkg, "} is required",
                     if (!is.null(reason)) paste0(" ", reason),
                     ".")
  cli::cli_abort(
    c(msg_main,
      "i" = paste0("Install it with {.code install.packages(\"", pkg, "\")}.")),
    class = c("beezdemand_missing_package", "beezdemand_error"),
    call = call
  )
}

#' Normalize Equation Name to Legacy Convention
#'
#' Normalize equation name across modeling tiers
#'
#' Maps equation aliases to canonical names for a given tier. Accepts
#' cross-tier aliases (e.g., "hs" in the TMB tier maps to "exponential")
#' and emits a deprecation-style message for non-canonical names.
#'
#' @param equation Character scalar. The equation name to normalize.
#' @param tier Character scalar. One of "fixed", "tmb", "nlme", "hurdle".
#'   Defaults to "fixed" for backwards compatibility.
#' @return Character scalar (canonical name for the tier).
#' @noRd
normalize_equation <- function(equation, tier = "fixed") {
  # Pass through if equation is still a multi-value default (pre-match.arg)
  if (length(equation) != 1) return(equation)

  # Canonical names per tier
  canonical <- list(
    fixed  = c("hs", "koff", "simplified", "linear"),
    tmb    = c("exponentiated", "exponential", "simplified", "zben"),
    nlme   = c("exponentiated", "zben", "simplified"),
    hurdle = c("zhao_exponential", "exponential", "simplified_exponential")
  )

  # Cross-tier alias map: alias -> list(tier = canonical_name)
  aliases <- list(
    # Legacy -> modern
    hs   = list(tmb = "exponential", nlme = NULL, hurdle = "exponential"),
    koff = list(tmb = "exponentiated", nlme = "exponentiated", hurdle = NULL),
    # Modern -> legacy
    exponential   = list(fixed = "hs"),
    exponentiated = list(fixed = "koff")
  )

  tier_canon <- canonical[[tier]]
  if (is.null(tier_canon)) {
    return(equation)
  }

  # Already canonical for this tier
 if (equation %in% tier_canon) {
    return(equation)
  }

  # Check aliases
  alias_entry <- aliases[[equation]]
  if (!is.null(alias_entry) && !is.null(alias_entry[[tier]])) {
    mapped <- alias_entry[[tier]]
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = I(paste0("equation = \"", equation, "\"")),
      with = I(paste0("equation = \"", mapped, "\"")),
      details = paste0("Alias accepted for the ", tier, " tier.")
    )
    return(mapped)
  }

  # Pass through (let match.arg in calling function handle invalid names)
  equation
}

##' Pull vector from data frame
##'
##' @description
##' `r lifecycle::badge("deprecated")`
##'
##' Pulls a single vector from a data frame. Good to use with dplyr.
##' From https://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
##' @title Pull
##' @param x A data frame
##' @param y Name of column
##' @return Vector
##' @importFrom lifecycle deprecate_warn
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @keywords internal
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
##' @examples
##' dat <- data.frame(price = 1:5, quantity = c(10, 8, 5, 2, 0), subj = rep(1, 5))
##' CheckCols(dat, xcol = "price", ycol = "quantity", idcol = "subj")
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
CheckCols <- function(dat, xcol, ycol, idcol, groupcol = NULL) {
  dat <- if (dplyr::is.tbl(dat)) {
    cli::cli_inform("Data casted as data.frame")
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
    cli::cli_warn("{.code NA} values found in {.field {ycol}} column. Dropping NAs and continuing.")
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
    cli::cli_abort("Can't find x, y, and id column names in data!")
  }

  if (!is.null(groupcol) && any(colnames(dat) %in% groupcol)) {
    colnames(dat) <- gsub(groupcol, "group", colnames(dat))
  } else if (!is.null(groupcol) && !any(colnames(dat) %in% "group")) {
    cli::cli_abort("Can't find groupcol column name in data!")
  } else if (
    !is.null(groupcol) &&
      any(colnames(dat) %in% "group") &&
      !any(colnames(dat) %in% groupcol)
  ) {
    cli::cli_abort(
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
##' @examples
##' ## Principal branch: W(1) ~ 0.5671
##' lambertW(1)
##'
##' ## Verify: W(z) * exp(W(z)) == z
##' w <- lambertW(2)
##' w * exp(w)
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
    cli::cli_abort("branch number for W must be an integer")
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
    cli::cli_warn(paste(
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
#' by checking for required columns (after optional column renaming), filtering
#' by target type if needed, and confirming ID column presence when required.
#'
#' @param data A data frame containing cross-price demand data.
#' @param x_var Character string; name of the price column. Default is `"x"`.
#'   If non-default, the column is renamed to `"x"` internally.
#' @param y_var Character string; name of the consumption column. Default is `"y"`.
#'   If non-default, the column is renamed to `"y"` internally.
#' @param id_var Character string; name of the subject identifier column.
#'   Default is `"id"`. If non-default, the column is renamed to `"id"` internally.
#' @param group_var Character string; name of the group column. Default is
#'   `"group"`. If non-default, the column is renamed to `"group"` internally.
#' @param target_var Character string; name of the target indicator column.
#'   Default is `"target"`. If non-default, the column is renamed to `"target"`
#'   internally.
#' @param required_cols Character vector of canonical column names to check after
#'   renaming. Default is `c("x", "y")`.
#' @param filter_target Logical; if TRUE and data contains a `"target"` column,
#'   filters to keep only rows where `target == target_level`. Default is TRUE.
#' @param target_level Character string; the value of the `target` column to
#'   retain when `filter_target = TRUE`. Default is `"alt"`.
#' @param require_id Logical; if TRUE, validates that an `"id"` column exists
#'   in the data (after renaming). Default is FALSE.
#'
#' @details
#' Column renaming uses a collision-safe approach: if a non-default `*_var`
#' mapping is requested but the canonical target name already exists as a
#' different column in `data`, the function stops with an informative error
#' rather than silently overwriting data. After normalization, `$data` on
#' returned objects always uses canonical names (`x`, `y`, `id`, `group`,
#' `target`), which is required for S3 methods to work correctly.
#'
#' @return A validated (and potentially filtered) data frame with canonical
#'   column names.
#'
#' @keywords internal
validate_cp_data <- function(
  data,
  x_var = "x",
  y_var = "y",
  id_var = "id",
  group_var = "group",
  target_var = "target",
  required_cols = c("x", "y"),
  filter_target = TRUE,
  target_level = "alt",
  require_id = FALSE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("Data must be a data frame.")
  }

  # Build mapping of non-default *_var -> canonical name
  var_map <- list(
    x = x_var,
    y = y_var,
    id = id_var,
    group = group_var,
    target = target_var
  )

  for (canonical in names(var_map)) {
    user_name <- var_map[[canonical]]
    if (user_name != canonical) {
      # Check collision: canonical name already exists as a different column
      if (canonical %in% names(data)) {
        cli::cli_abort(c(
          sprintf(
            "%s_var = %s but data already contains a column named %s.",
            canonical, shQuote(user_name), shQuote(canonical)
          ),
          "i" = sprintf(
            "Rename or drop the existing %s column before calling the fitting function.",
            shQuote(canonical)
          )
        ))
      }
      # Rename if the user-specified column exists
      if (user_name %in% names(data)) {
        names(data)[names(data) == user_name] <- canonical
      }
    }
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {.field {missing_cols}}.")
  }

  if (filter_target && "target" %in% names(data)) {
    data <- data[data$target == target_level, , drop = FALSE]
    if (nrow(data) == 0) {
      cli::cli_abort(
        sprintf(
          "No data with target = %s found in the provided data.",
          shQuote(target_level)
        )
      )
    }
  }

  if (require_id && !("id" %in% names(data))) {
    cli::cli_abort("Data must contain an 'id' column for this operation.")
  }

  return(data)
}


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
    cli::cli_abort("Input 'data' must be a data frame.")
  }

  # Construct required columns list
  required_cols <- c(y_var, x_var, id_var)
  if (!is.null(factors)) {
    required_cols <- c(required_cols, factors)
  }

  # Check for missing columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing required columns in data: {.field {missing_cols}}."
    )
  }

  # Basic type validation for core variables (fail fast with clear errors)
  if (!is.numeric(data[[x_var]])) {
    cli::cli_abort("{.arg x_var} column {.field {x_var}} must be numeric.")
  }
  if (!is.numeric(data[[y_var]])) {
    cli::cli_abort("{.arg y_var} column {.field {y_var}} must be numeric.")
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
    cli::cli_abort(
      "Collapse specification for {.val {suffix}} must be a named list of factor mappings."
    )
  }

  new_factor_names <- factors
  info <- list()

  for (factor_col in names(collapse_spec)) {
    # Validate factor is in the model
    if (!factor_col %in% factors) {
      cli::cli_warn(
        "Factor {.field {factor_col}} in {.code collapse_levels${suffix}} is not in the {.arg factors} list. Skipping."
      )
      next
    }

    # Validate factor is in the data
    if (!factor_col %in% names(data)) {
      cli::cli_warn(
        "Factor {.field {factor_col}} not found in data. Skipping."
      )
      next
    }

    level_map <- collapse_spec[[factor_col]]

    if (!is.list(level_map) || is.null(names(level_map))) {
      cli::cli_abort(
        "Collapse mapping for factor {.field {factor_col}} must be a named list ({.code new_level = c(old_levels)})."
      )
    }

    # Check for overlapping old levels
    all_old_levels <- unlist(level_map, use.names = FALSE)
    if (length(all_old_levels) != length(unique(all_old_levels))) {
      duplicates <- all_old_levels[duplicated(all_old_levels)]
      cli::cli_abort(c(
        "Overlapping old levels detected in collapse mapping for {.field {factor_col}}: {.val {unique(duplicates)}}.",
        "i" = "Each old level can only map to one new level."
      ))
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
      cli::cli_inform(
        "Note: Factor(s) with only 1 level removed from formula: {.field {dropped}}."
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
    cli::cli_abort(
      "'collapse_levels' must be a named list with keys 'Q0' and/or
'alpha'."
    )
  }

  valid_keys <- c("Q0", "alpha")
  provided_keys <- names(collapse_levels)

  if (is.null(provided_keys) || length(provided_keys) == 0) {
    cli::cli_abort(c(
      "{.arg collapse_levels} must have named elements.",
      "i" = "Expected keys: {.val Q0} and/or {.val alpha}."
    ))
  }

  invalid_keys <- setdiff(provided_keys, valid_keys)
  if (length(invalid_keys) > 0) {
    cli::cli_abort(c(
      "Invalid keys in {.arg collapse_levels}: {.val {invalid_keys}}.",
      "i" = "Only {.val Q0} and {.val alpha} are allowed."
    ))
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
    component = character(),
    estimate_scale = character(),
    term_display = character()
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

#' Split Data by Grouping Variables and Apply Function
#'
#' Internal helper that splits a data frame by the columns in `by`,
#' applies `FUN(slice, key_row)` to each group, and returns a named list
#' of results.
#'
#' @param data A data frame.
#' @param by Character vector of column names to split by.
#' @param FUN Function taking two arguments: the data subset and a one-row
#'   data frame of group key values. Must return the per-group result.
#' @param call Caller environment for error reporting.
#'
#' @return A list with:
#'   \describe{
#'     \item{results}{Named list of per-group FUN outputs}
#'     \item{group_keys}{Data frame of unique group combinations}
#'   }
#'
#' @keywords internal
beezdemand_split_by <- function(data, by, FUN, call = rlang::caller_env()) {
  # Validate by columns exist

  missing <- setdiff(by, names(data))
  if (length(missing) > 0) {
    validation_error(
      paste0(
        "`by` column(s) not found in data: ",
        paste(missing, collapse = ", ")
      ),
      arg = "by",
      call = call
    )
  }

  # Drop rows with NA in grouping columns

  na_mask <- rowSums(is.na(data[, by, drop = FALSE])) > 0
  if (any(na_mask)) {
    n_na <- sum(na_mask)
    cli::cli_warn(sprintf(
      "%d row(s) with NA in grouping column(s) (%s) removed before splitting.",
      n_na, paste(by, collapse = ", ")
    ))
    data <- data[!na_mask, , drop = FALSE]
  }

  # Build unique group keys (sorted)
  group_keys <- unique(data[, by, drop = FALSE])
  group_keys <- group_keys[do.call(order, group_keys), , drop = FALSE]
  rownames(group_keys) <- NULL

  # Build group labels for naming
  group_labels <- apply(group_keys, 1, function(row) {
    paste(by, row, sep = "=", collapse = ", ")
  })

  # Split and apply
  results <- vector("list", nrow(group_keys))
  names(results) <- group_labels

  for (i in seq_len(nrow(group_keys))) {
    key_row <- group_keys[i, , drop = FALSE]

    # Build logical mask for this group
    mask <- rep(TRUE, nrow(data))
    for (col in by) {
      mask <- mask & (data[[col]] == key_row[[col]])
    }
    slice <- data[mask, , drop = FALSE]

    results[[i]] <- FUN(slice, key_row)
  }

  list(results = results, group_keys = group_keys)
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
