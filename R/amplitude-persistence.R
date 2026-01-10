#' Calculate Amplitude and Persistence
#'
#' Calculates Amplitude and Persistence latent factors from demand metrics
#' for various beezdemand model objects.
#'
#' @param fit An object of class `beezdemand_fixed`, `beezdemand_hurdle`, `beezdemand_nlme`,
#'   or a `data.frame`.
#' @param amplitude Character vector of column names to consider for the Amplitude factor.
#'   The function will use the first column found in the data. Default is
#'   `c("Intensity", "Q0d", "Q0")`.
#' @param persistence Character vector of column names to include in the Persistence factor.
#'   Default is `c("BP0", "Pmaxe", "Omaxe", "Alpha")`. All found columns will be used.
#' @param use_inv_alpha Logical. If "Alpha" (or a variation) is present in `persistence`,
#'   should it be inverted (1/Alpha) before standardization? Default is `TRUE`.
#' @param strict Logical. If `TRUE` (default), missing metrics, duplicated `id`s, and other
#'   data integrity problems produce errors instead of warnings.
#' @param min_persistence_components Integer. Minimum number of non-missing standardized
#'   persistence components required to compute `Persistence` for a given `id`.
#'   If fewer are available, `Persistence` is set to `NA`. Default is `2`.
#' @param empirical_y_var For `beezdemand_nlme` objects, optional column name in `fit$data`
#'   to use when computing empirical indices (e.g., `BP0`). This is important when the
#'   fitted `y_var` is transformed (e.g., log10). If `NULL`, the method will attempt to
#'   choose a sensible default and may error in `strict` mode if ambiguous.
#' @param basis_means Optional named numeric vector of means to use for Z-score standardization.
#'   Names must match the columns used (e.g., `c(Intensity = 10, BP0 = 5)`).
#'   If NULL (default), the sample means are used.
#' @param basis_sds Optional named numeric vector of standard deviations to use for Z-score standardization.
#'   Names must match the columns used. If NULL (default), the sample SDs are used.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame with the original ID and calculated Amplitude and Persistence scores,
#'   along with the standardized (Z-scored) constituent metrics.
#' @details
#' This function calculates Amplitude and Persistence by:
#' 1. Extracting the relevant demand metrics from the `fit` object.
#' 2. Resolving requested metric columns (case-insensitive, with limited synonym support
#'    for common beezdemand outputs).
#' 3. Inverting Alpha if requested (1/Alpha).
#' 4. Standardizing (Z-scoring) the metrics. If `basis_means` and `basis_sds` are provided,
#'    they are used; otherwise, the current sample's statistics are used.
#' 5. Aggregating the Z-scores into the two latent factors.
#'
#' **Amplitude** is defined by the variable specified in `amplitude` (typically Intensity/Q0).
#' **Persistence** is defined as the mean of the standardized values of the variables
#' specified in `persistence` (typically Breakpoint, Pmax, Omax, and 1/Alpha).
#'
#' @section Models:
#' * **beezdemand_fixed**: Extracts metrics from `fit$results`.
#' * **beezdemand_hurdle**: Extracts metrics from `fit$subject_pars`.
#' * **beezdemand_nlme**: Calculates subject-specific parameters from fixed and random effects.
#'   Parameters `Q0` and `Alpha` are assumed to be on `log10` scale for `zben` and `simplified`
#'   equations and are converted to linear scale. Omax and Pmax are calculated empirically
#'   from predictions. Breakpoint is calculated empirically from the raw data.
#'
#' @export
calculate_amplitude_persistence <- function(fit,
                                            amplitude = c("Intensity", "Q0d", "Q0"),
                                            persistence = c("BP0", "Pmaxe", "Omaxe", "Alpha"),
                                            use_inv_alpha = TRUE,
                                            strict = TRUE,
                                            min_persistence_components = 2L,
                                            empirical_y_var = NULL,
                                            basis_means = NULL,
                                            basis_sds = NULL,
                                            ...) {
  UseMethod("calculate_amplitude_persistence")
}

#' @export
calculate_amplitude_persistence.default <- function(fit,
                                                    amplitude = c("Intensity", "Q0d", "Q0"),
                                                    persistence = c("BP0", "Pmaxe", "Omaxe", "Alpha"),
                                                    use_inv_alpha = TRUE,
                                                    strict = TRUE,
                                                    min_persistence_components = 2L,
                                                    empirical_y_var = NULL,
                                                    basis_means = NULL,
                                                    basis_sds = NULL,
                                                    ...) {
  df <- fit
  if (!is.data.frame(df)) {
    stop("Input 'fit' must be a data frame or a supported beezdemand object.")
  }

  if (!is.null(empirical_y_var)) {
    warning(
      "`empirical_y_var` is ignored for data.frame inputs; it is only used for ",
      "`beezdemand_nlme` objects."
    )
  }

  # Ensure ID column exists
  if (!"id" %in% names(df)) {
    if ("Group" %in% names(df)) {
      df$id <- df$Group
    } else {
      stop("Data must contain an 'id' column.")
    }
  }

  if (anyDuplicated(df$id) > 0) {
    msg <- paste0(
      "Duplicate 'id' values detected. This function expects one row per id ",
      "(i.e., a metrics table, not raw purchase task rows)."
    )
    if (isTRUE(strict)) {
      stop(msg)
    } else {
      warning(msg, " Using the first row per id.")
      df <- df[!duplicated(df$id), , drop = FALSE]
    }
  }

  # Case-insensitive column resolution with light synonym support
  resolve_cols <- function(requested, available) {
    if (length(requested) == 0) return(character())

    available_lower <- tolower(available)
    requested <- unique(stats::na.omit(as.character(requested)))

    synonyms <- list(
      intensity = c("Intensity", "Q0d", "Q0"),
      breakpoint = c("BP0", "breakpoint", "Breakpoint"),
      pmax = c("Pmaxe", "Pmax", "Pmaxd", "Pmaxa"),
      omax = c("Omaxe", "Omax", "Omaxd", "Omaxa"),
      alpha = c("Alpha", "alpha", "Elasticity", "elasticity")
    )

    resolve_one <- function(name) {
      candidates <- c(name)
      for (syn_set in synonyms) {
        if (tolower(name) %in% tolower(syn_set)) {
          candidates <- unique(c(candidates, syn_set))
        }
      }
      idx <- match(tolower(candidates), available_lower)
      idx <- idx[!is.na(idx)]
      if (length(idx) == 0) return(NA_character_)
      available[idx[1]]
    }

    vapply(requested, resolve_one, FUN.VALUE = character(1))
  }

  # Identify Amplitude column (pick first available)
  amp_resolved <- resolve_cols(amplitude, names(df))
  amp_col <- amp_resolved[!is.na(amp_resolved)][1]
  if (is.na(amp_col) || length(amp_col) == 0) {
    stop(
      paste0(
        "None of the specified amplitude columns found (case-insensitive): ",
        paste(amplitude, collapse = ", ")
      )
    )
  }

  # Identify Persistence columns (use all available)
  pers_resolved <- resolve_cols(persistence, names(df))
  pers_cols <- unique(pers_resolved[!is.na(pers_resolved)])
  if (length(pers_cols) == 0) {
    stop(
      paste0(
        "None of the specified persistence columns found (case-insensitive): ",
        paste(persistence, collapse = ", ")
      )
    )
  }

  missing_requested <- unique(persistence[is.na(pers_resolved)])
  if (length(missing_requested) > 0) {
    msg <- paste0(
      "Some requested persistence columns were not found and will be ignored: ",
      paste(missing_requested, collapse = ", ")
    )
    if (isTRUE(strict)) stop(msg) else warning(msg)
  }

  # Work on a copy
  df_metrics <- df[, c("id", amp_col, pers_cols), drop = FALSE]

  # Validate / coerce numeric metrics
  to_numeric_or_stop <- function(x, name) {
    if (is.numeric(x)) return(x)
    suppressWarnings(x_num <- as.numeric(x))
    if (isTRUE(strict) && any(!is.na(x) & is.na(x_num))) {
      stop("Column '", name, "' could not be safely coerced to numeric.")
    }
    if (any(!is.na(x) & is.na(x_num))) {
      warning("Coercing column '", name, "' to numeric introduced NA values.")
    }
    x_num
  }

  df_metrics[[amp_col]] <- to_numeric_or_stop(df_metrics[[amp_col]], amp_col)
  for (pc in pers_cols) {
    df_metrics[[pc]] <- to_numeric_or_stop(df_metrics[[pc]], pc)
  }

  # Handle Alpha Inversion
  alpha_candidates <- pers_cols[tolower(pers_cols) %in% c("alpha", "elasticity")]
  inv_alpha_used <- FALSE
  if (isTRUE(use_inv_alpha) && length(alpha_candidates) > 0) {
    # If multiple alpha-like columns are supplied, invert each and treat them as separate
    # components with stable names (inv_alpha, inv_alpha_2, ...).
    inv_names <- character(0)
    for (j in seq_along(alpha_candidates)) {
      ac <- alpha_candidates[j]
      vals <- df_metrics[[ac]]

      if (any(vals <= 0, na.rm = TRUE)) {
        warning(
          "Non-positive values found in '",
          ac,
          "'. These are treated as NA before computing 1/alpha."
        )
        vals[vals <= 0] <- NA_real_
      }

      inv_vals <- 1 / vals
      inv_col <- if (j == 1) "inv_alpha" else paste0("inv_alpha_", j)
      df_metrics[[inv_col]] <- inv_vals
      inv_names <- c(inv_names, inv_col)
    }

    # Replace alpha-like components in persistence with the derived inv_* columns.
    # This avoids the interpretability trap where 'z_Alpha' actually means z(1/Alpha).
    pers_cols <- c(setdiff(pers_cols, alpha_candidates), inv_names)
    inv_alpha_used <- TRUE
  }

  # Standardization Helper
  # x: vector to standardize
  # name: column name (to lookup basis)
  standardize <- function(x, name) {
    if (all(is.na(x))) return(x)

    basis_means_lower <- if (!is.null(basis_means)) setNames(basis_means, tolower(names(basis_means))) else NULL
    basis_sds_lower <- if (!is.null(basis_sds)) setNames(basis_sds, tolower(names(basis_sds))) else NULL

    name_lower <- tolower(name)

    mu <- if (!is.null(basis_means_lower) && name_lower %in% names(basis_means_lower)) {
      basis_means_lower[[name_lower]]
    } else {
      mean(x, na.rm = TRUE)
    }

    sigma <- if (!is.null(basis_sds_lower) && name_lower %in% names(basis_sds_lower)) {
      basis_sds_lower[[name_lower]]
    } else {
      stats::sd(x, na.rm = TRUE)
    }

    if (is.na(sigma) || sigma == 0) {
      # If there's no variation, define z=0 for observed values and preserve missingness.
      return(ifelse(is.na(x), NA_real_, 0))
    }

    (x - mu) / sigma
  }

  # Calculate Z-scores
  # Amplitude
  z_amp_vec <- standardize(df_metrics[[amp_col]], amp_col)
  
  # Persistence
  z_pers_df <- data.frame(matrix(ncol = length(pers_cols), nrow = nrow(df_metrics)))
  names(z_pers_df) <- pers_cols

  for (pc in pers_cols) {
    z_pers_df[[pc]] <- standardize(df_metrics[[pc]], pc)
  }

  # Add Z-scores to results
  df_final <- df[, "id", drop = FALSE]
  df_final[[paste0("z_", amp_col)]] <- z_amp_vec
  
  # Rename persistence Z columns
  for (pc in pers_cols) {
    df_final[[paste0("z_", pc)]] <- z_pers_df[[pc]]
  }
  
  # Aggregate Factors
  df_final$Amplitude <- z_amp_vec
  
  non_missing_pers <- rowSums(!is.na(z_pers_df))
  pers_val <- if (ncol(z_pers_df) == 1) {
    z_pers_df[[1]]
  } else {
    rowMeans(z_pers_df, na.rm = TRUE)
  }
  pers_val[is.nan(pers_val)] <- NA_real_

  if (!is.numeric(min_persistence_components) || length(min_persistence_components) != 1) {
    stop("'min_persistence_components' must be a single integer.")
  }
  min_persistence_components <- as.integer(min_persistence_components)
  if (min_persistence_components < 1) min_persistence_components <- 1L
  pers_val[non_missing_pers < min_persistence_components] <- NA_real_
  df_final$Persistence <- pers_val

  attr(df_final, "amplitude_metric") <- amp_col
  attr(df_final, "persistence_components") <- pers_cols
  attr(df_final, "inv_alpha_used") <- inv_alpha_used
  
  return(df_final)
}

#' @export
calculate_amplitude_persistence.beezdemand_fixed <- function(fit,
                                                             amplitude = c("Intensity", "Q0d", "Q0"),
                                                             persistence = c("BP0", "Pmaxe", "Omaxe", "Alpha"),
                                                             use_inv_alpha = TRUE,
                                                             strict = TRUE,
                                                             min_persistence_components = 2L,
                                                             empirical_y_var = NULL,
                                                             basis_means = NULL,
                                                             basis_sds = NULL,
                                                             ...) {
  # fit$results contains the metrics
  results <- fit$results
  
  # Filter out non-converged subjects if 'converged' column exists
  if ("converged" %in% names(results)) {
    n_nonconverged <- sum(!results$converged, na.rm = TRUE)
    if (n_nonconverged > 0) {
      message(
        "Excluding ", n_nonconverged, " non-converged subject(s) from amplitude/persistence calculation."
      )
      results <- results[results$converged == TRUE, , drop = FALSE]
    }
  }
  
  # Set invalid parameter values to NA to avoid downstream issues
  # This handles cases where fit "converged" but produced physiologically implausible values
  if ("Alpha" %in% names(results)) {
    invalid_alpha <- !is.na(results$Alpha) & results$Alpha <= 0
    if (any(invalid_alpha)) {
      results$Alpha[invalid_alpha] <- NA_real_
    }
  }
  if ("Q0d" %in% names(results)) {
    invalid_q0 <- !is.na(results$Q0d) & results$Q0d <= 0
    if (any(invalid_q0)) {
      results$Q0d[invalid_q0] <- NA_real_
    }
  }
  
  # Pass to default
  calculate_amplitude_persistence.default(results, 
                                          amplitude = amplitude, 
                                          persistence = persistence,
                                          use_inv_alpha = use_inv_alpha,
                                          strict = strict,
                                          min_persistence_components = min_persistence_components,
                                          empirical_y_var = empirical_y_var,
                                          basis_means = basis_means,
                                          basis_sds = basis_sds,
                                          ...)
}

#' @export
calculate_amplitude_persistence.beezdemand_hurdle <- function(fit,
                                                              amplitude = c("Q0"), # Hurdle usually only has derived Q0
                                                              persistence = c("breakpoint", "Pmax", "Omax", "alpha"),
                                                              use_inv_alpha = TRUE,
                                                              strict = TRUE,
                                                              min_persistence_components = 2L,
                                                              empirical_y_var = NULL,
                                                              basis_means = NULL,
                                                              basis_sds = NULL,
                                                              ...) {
  # fit$subject_pars contains metrics
  pars <- fit$subject_pars
  
  # Map names to standard expectations if they differ significantly
  # subject_pars: id, Q0, alpha, breakpoint, Pmax, Omax
  
  # Note: Hurdle Q0 and alpha are already linear scale in subject_pars
  
  calculate_amplitude_persistence.default(pars, 
                                          amplitude = amplitude, 
                                          persistence = persistence,
                                          use_inv_alpha = use_inv_alpha,
                                          strict = strict,
                                          min_persistence_components = min_persistence_components,
                                          empirical_y_var = empirical_y_var,
                                          basis_means = basis_means,
                                          basis_sds = basis_sds,
                                          ...)
}

#' @export
calculate_amplitude_persistence.beezdemand_nlme <- function(fit,
                                                            amplitude = c("Q0"),
                                                            persistence = c("BP0", "Pmax", "Omax", "Alpha"),
                                                            use_inv_alpha = TRUE,
                                                            strict = TRUE,
                                                            min_persistence_components = 2L,
                                                            empirical_y_var = NULL,
                                                            basis_means = NULL,
                                                            basis_sds = NULL,
                                                            ...) {
  # 1. Check for CP models (though nlme usually isn't CP, verify just in case)
  # beezdemand_nlme doesn't explicitly store model type like 'cp' but relies on formula
  
  # 2. Extract Coefficients (Subject-specific)
  # This returns data frame with id, Q0, alpha (log10 scale typically)
  coefs <- coef(fit, type = "combined")
  
  # Identify parameter columns
  # They are typically named "Q0" and "alpha" based on formula
  if (!all(c("Q0", "alpha") %in% names(coefs))) {
    warning("Could not find standard 'Q0' and 'alpha' parameters in NLME coefficients. Attempting to proceed with available columns.")
  }
  
  # 3. Handle Log Scale Conversion
  # Check equation form
  eq_form <- fit$param_info$equation_form
  if (is.null(eq_form)) eq_form <- fit$formula_details$equation_form_selected
  
  df_calc <- coefs
  df_calc$id <- rownames(coefs) # coef returns rownames as IDs
  
  if (!is.null(eq_form) && eq_form %in% c("zben", "simplified")) {
    # Parameters are log10 scale
    df_calc$Q0 <- 10^df_calc$Q0
    df_calc$Alpha <- 10^df_calc$alpha
  } else {
    # Assume linear or user-specified custom formula where we can't be sure.
    # We'll use them as is but warn if they look like logs (e.g., negative Q0)
    df_calc$Alpha <- df_calc$alpha
    if (any(df_calc$Q0 < 0)) {
       warning("Negative Q0 values detected in NLME coefficients. If these are log10 values, custom model formula parameters should be named to indicate scale or handled manually.")
    }
  }
  
  # 4. Calculate Empirical Metrics (Omax, Pmax, Breakpoint)
  
  # Breakpoint from raw data (BP0)
  raw_data <- fit$data
  id_var <- fit$param_info$id_var
  x_var <- fit$param_info$x_var
  y_var <- fit$param_info$y_var

  if (is.null(id_var) || is.null(x_var) || is.null(y_var)) {
    stop("NLME object is missing required 'param_info' fields (id_var, x_var, y_var).")
  }

  # Determine which column to use for empirical metrics (e.g., BP0).
  y_emp <- empirical_y_var
  if (is.null(y_emp)) {
    if (!is.null(eq_form) && eq_form == "zben") {
      # zben generally fits log10-transformed y. Empirical indices should be computed on raw y.
      # If a plausible raw column exists, use it; otherwise error in strict mode.
      if ("y" %in% names(raw_data) && y_var != "y") {
        y_emp <- "y"
      } else {
        msg <- paste0(
          "For equation_form='zben', empirical indices like BP0 should be computed on raw y, ",
          "but a raw y column could not be inferred. Provide `empirical_y_var` explicitly."
        )
        if (isTRUE(strict)) stop(msg) else warning(msg)
        y_emp <- y_var
      }
    } else {
      y_emp <- y_var
    }
  } else {
    if (!is.character(y_emp) || length(y_emp) != 1) {
      stop("'empirical_y_var' must be NULL or a single column name.")
    }
    if (!(y_emp %in% names(raw_data))) {
      stop("Column '", y_emp, "' (empirical_y_var) not found in fit$data.")
    }
  }

  emp_data <- data.frame(
    id = as.character(raw_data[[id_var]]),
    x = raw_data[[x_var]],
    y = raw_data[[y_emp]]
  )
  emp_data <- emp_data[!is.na(emp_data$x) & !is.na(emp_data$y), , drop = FALSE]

  emp_metrics <- GetEmpirical(emp_data)

  # Omax/Pmax from model predictions on the observed price schedule per subject.
  ids <- as.character(df_calc$id)
  omax_vec <- rep(NA_real_, length(ids))
  pmax_vec <- rep(NA_real_, length(ids))

  inv_fun <- if (!is.null(eq_form) && eq_form == "zben") {
    function(x) 10^x
  } else {
    identity
  }

  raw_ids_chr <- as.character(raw_data[[id_var]])

  for (i in seq_along(ids)) {
    subject_id <- ids[i]

    subj_rows <- raw_data[raw_ids_chr == subject_id, , drop = FALSE]
    if (nrow(subj_rows) == 0) next

    # Use the subject's observed price schedule (unique, sorted).
    pred_x <- sort(unique(subj_rows[[x_var]]))
    pred_x <- pred_x[!is.na(pred_x)]
    if (length(pred_x) == 0) next

    subj_data_row <- subj_rows[1, , drop = FALSE]
    new_dat <- subj_data_row[rep(1, length(pred_x)), , drop = FALSE]
    new_dat[[x_var]] <- pred_x

    preds <- tryCatch(
      predict(fit, newdata = new_dat, level = 1, inv_fun = inv_fun),
      error = function(e) NA_real_
    )

    if (all(is.na(preds))) next
    preds[preds < 0] <- 0

    expenditure <- pred_x * preds
    max_exp_idx <- which.max(expenditure)
    if (length(max_exp_idx) == 0 || is.na(max_exp_idx)) next

    omax_vec[i] <- expenditure[max_exp_idx]
    pmax_vec[i] <- pred_x[max_exp_idx]
  }

  df_calc$Omax <- omax_vec
  df_calc$Pmax <- pmax_vec

  # Join BP0 without reordering rows.
  emp_metrics$id <- as.character(emp_metrics$id)
  bp0 <- emp_metrics$BP0[match(ids, emp_metrics$id)]
  df_final <- df_calc
  df_final$BP0 <- bp0
  
  # Pass to default
  calculate_amplitude_persistence.default(df_final,
                                          amplitude = amplitude,
                                          persistence = persistence,
                                          use_inv_alpha = use_inv_alpha,
                                          strict = strict,
                                          min_persistence_components = min_persistence_components,
                                          empirical_y_var = NULL,
                                          basis_means = basis_means,
                                          basis_sds = basis_sds,
                                          ...)
}
