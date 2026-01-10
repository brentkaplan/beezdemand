#' Print Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object
#' @param ... Additional arguments (ignored)
#' @export
print.beezdemand_fixed <- function(x, ...) {
  cat("\nFixed-Effect Demand Model\n")
  cat("==========================\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Equation:", x$equation, "\n")
  cat("k:", x$k_spec, "\n")
  if (!is.null(x$agg)) {
    cat("Aggregation:", x$agg, "\n")
  }
  cat("Subjects:", x$n_total, "(", x$n_success, "converged,", x$n_fail, "failed)\n")
  cat("\nUse summary() for parameter summaries, tidy() for tidy output.\n")

  invisible(x)
}

#' Plot Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object.
#' @param type Plot type: "demand", "population", "individual", or "both".
#' @param ids Optional vector of subject IDs to plot. Defaults to all subjects.
#' @param style Plot styling, passed to \code{theme_beezdemand()}.
#' @param show_observed Logical; if TRUE, overlay observed data points where possible.
#' @param show_pred Which prediction layers to plot: "population", "individual",
#'   or "both".
#' @param x_trans X-axis transform: "log", "log10", "linear", or "pseudo_log".
#' @param y_trans Y-axis transform: "log", "log10", "linear", or "pseudo_log".
#' @param free_trans Value used to display free (x = 0) on log scales. Use NULL
#'   to drop x <= 0 values instead.
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#' @param n_points Number of points to use for prediction curves when thinning.
#' @param x_lab Optional x-axis label.
#' @param y_lab Optional y-axis label.
#' @param xlab Deprecated alias for \code{x_lab}.
#' @param ylab Deprecated alias for \code{y_lab}.
#' @param facet Faceting specification (TRUE for \code{~id} or a formula).
#' @param observed_point_alpha Alpha for observed points.
#' @param observed_point_size Size for observed points.
#' @param pop_line_alpha Alpha for population curve.
#' @param pop_line_size Line size for population curve.
#' @param ind_line_alpha Alpha for individual curves.
#' @param ind_line_size Line size for individual curves.
#' @param subtitle Optional subtitle for the plot.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom rlang .data
plot.beezdemand_fixed <- function(
  x,
  type = c("demand", "population", "individual", "both"),
  ids = NULL,
  style = c("modern", "apa"),
  show_observed = TRUE,
  show_pred = NULL,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  free_trans = 0.01,
  x_limits = NULL,
  y_limits = NULL,
  n_points = 200,
  x_lab = NULL,
  y_lab = NULL,
  xlab = NULL,
  ylab = NULL,
  facet = NULL,
  observed_point_alpha = 0.5,
  observed_point_size = 1.8,
  pop_line_alpha = 0.9,
  pop_line_size = 1.0,
  ind_line_alpha = 0.35,
  ind_line_size = 0.7,
  subtitle = NULL,
  ...
) {
  type <- match.arg(type)
  style <- match.arg(style)
  x_trans <- match.arg(x_trans)
  y_trans_missing <- is.null(y_trans)
  if (y_trans_missing) {
    y_trans <- beezdemand_default_y_trans(type = type, equation = x$equation)
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))

  if (is.null(x$predictions) || is.null(x$data_used) || is.null(x$results)) {
    stop("Fit object lacks detailed predictions; refit with detailed = TRUE.")
  }

  labels <- beezdemand_normalize_plot_labels(x_lab, y_lab, xlab, ylab)
  x_lab <- labels$x_lab %||% "Price"
  y_lab <- labels$y_lab %||% "Consumption"

  agg_lower <- if (is.null(x$agg)) NA_character_ else tolower(x$agg)
  has_population <- !is.na(agg_lower) && agg_lower %in% c("mean", "pooled")
  has_individual <- !has_population

  results <- x$results
  ids_all <- if ("id" %in% names(results)) {
    as.character(results$id)
  } else {
    as.character(seq_along(x$predictions))
  }

  if (is.null(show_pred)) {
    show_pred <- if (has_population) "population" else "individual"
  }

  if (type == "population") {
    show_pred <- "population"
  } else if (type == "individual") {
    show_pred <- "individual"
  } else if (type == "both") {
    show_pred <- "both"
  }

  show_pred <- beezdemand_normalize_show_pred(show_pred)

  if (has_population && any(show_pred %in% "individual")) {
    stop("Individual fits are not available when agg = 'Mean' or 'Pooled'.")
  }
  if (has_individual && any(show_pred %in% "population")) {
    stop("Population curve is not available for per-person fits.")
  }

  if (has_population && !is.null(ids)) {
    stop("ids are not available when agg = 'Mean' or 'Pooled'.")
  }

  if (has_individual) {
    if (is.null(ids)) {
      ids <- ids_all
    }
    ids <- as.character(ids)
    idx <- match(ids, ids_all)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) {
      stop("No matching ids found in fit results.")
    }
  }

  y_trans_res <- beezdemand_resolve_y_trans(y_trans, y_is_log = FALSE)
  y_trans <- y_trans_res$y_trans

  obs_df <- NULL
  pred_df <- NULL
  pop_df <- NULL
  subtitle_note <- FALSE
  free_trans_used <- FALSE

  if (any(show_pred %in% "population")) {
    pop_pred <- x$predictions[[1]]
    pop_df <- data.frame(
      id = ids_all[1],
      x = pop_pred$x,
      y = pop_pred$y
    )
    if (nrow(pop_df) > n_points) {
      pop_df <- pop_df[round(seq(1, nrow(pop_df), length.out = n_points)), , drop = FALSE]
    }

    free_pop <- beezdemand_apply_free_trans(pop_df, "x", x_trans, free_trans)
    pop_df <- free_pop$data
    free_trans_used <- free_trans_used || free_pop$replaced

    pop_y <- beezdemand_drop_nonpositive_y(pop_df, "y", y_trans)
    pop_df <- pop_y$data
    subtitle_note <- subtitle_note || pop_y$dropped
  }

  if (show_observed) {
    if (has_population) {
      obs_df <- x$data_used[[1]]
    } else {
      obs_df <- do.call(rbind, lapply(idx, function(i) x$data_used[[i]]))
    }
    obs_df$id <- as.character(obs_df$id)
    free_obs <- beezdemand_apply_free_trans(obs_df, "x", x_trans, free_trans)
    obs_df <- free_obs$data
    free_trans_used <- free_trans_used || free_obs$replaced

    obs_y <- beezdemand_drop_nonpositive_y(obs_df, "y", y_trans)
    obs_df <- obs_y$data
    subtitle_note <- subtitle_note || obs_y$dropped
  }

  if (any(show_pred %in% "individual")) {
    pred_df <- do.call(rbind, lapply(idx, function(i) {
      pred <- x$predictions[[i]]
      pred$id <- ids_all[i]
      if (nrow(pred) > n_points) {
        pred <- pred[round(seq(1, nrow(pred), length.out = n_points)), , drop = FALSE]
      }
      pred
    }))
    pred_df$id <- as.character(pred_df$id)

    free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
    pred_df <- free_pred$data
    free_trans_used <- free_trans_used || free_pred$replaced

    pred_y <- beezdemand_drop_nonpositive_y(pred_df, "y", y_trans)
    pred_df <- pred_y$data
    subtitle_note <- subtitle_note || pred_y$dropped
  }

  color_by <- if (any(show_pred %in% "individual") && length(unique(pred_df$id)) > 1) {
    "id"
  } else {
    NULL
  }

  p <- ggplot2::ggplot()

  if (!is.null(pop_df)) {
    p <- p +
      ggplot2::geom_line(
        data = pop_df,
        ggplot2::aes(x = x, y = y),
        linewidth = pop_line_size,
        alpha = pop_line_alpha,
        color = beezdemand_style_color(style, "primary")
      )
  }

  if (!is.null(pred_df)) {
    aes_pred <- if (is.null(color_by)) {
      ggplot2::aes(x = x, y = y, group = id)
    } else {
      ggplot2::aes(x = x, y = y, color = .data[[color_by]], group = id)
    }
    p <- p +
      ggplot2::geom_line(
        data = pred_df,
        mapping = aes_pred,
        linewidth = ind_line_size,
        alpha = ind_line_alpha
      )
  }

  if (show_observed && !is.null(obs_df) && nrow(obs_df) > 0) {
    aes_obs <- if (is.null(color_by)) {
      ggplot2::aes(x = x, y = y)
    } else {
      ggplot2::aes(x = x, y = y, color = .data[[color_by]])
    }
    p <- p +
      ggplot2::geom_point(
        data = obs_df,
        mapping = aes_obs,
        alpha = observed_point_alpha,
        size = observed_point_size
      )
  }

  if (!is.null(facet)) {
    if (isTRUE(facet)) {
      p <- p + ggplot2::facet_wrap(~id)
    } else if (is.character(facet)) {
      p <- p + ggplot2::facet_wrap(stats::as.formula(facet))
    } else if (inherits(facet, "formula")) {
      p <- p + ggplot2::facet_wrap(facet)
    }
  }

  if (isTRUE(subtitle_note)) {
    if (is.null(subtitle)) {
      subtitle <- "Zeros omitted on log scale."
    } else {
      subtitle <- paste(subtitle, "Zeros omitted on log scale.")
    }
  }
  beezdemand_warn_free_trans(free_trans_used, free_trans)

  x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
  y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

  p <- p +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      limits = x_limits,
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::scale_y_continuous(
      trans = beezdemand_get_trans(y_trans),
      limits = y_limits,
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::labs(
      title = if (type == "population") "Population Demand Curve" else NULL,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab
    ) +
    theme_beezdemand(style = style)

  if (!is.null(color_by)) {
    scale_data <- if (!is.null(pred_df)) pred_df else obs_df
    p <- beezdemand_apply_color_scale(p, style, scale_data, color_by)
  }

  if (x_trans == "log10") {
    p <- p + ggplot2::annotation_logticks(sides = "b")
  }
  if (y_trans == "log10") {
    p <- p + ggplot2::annotation_logticks(sides = "l")
  }

  p
}

#' Summary Method for beezdemand_fixed
#'
#' @param object A beezdemand_fixed object
#' @param report_space Character. Reporting space for core parameters. One of:
#'   - `"natural"`: report natural-scale parameters (default)
#'   - `"log10"`: report `log10()`-scale parameters when defined
#' @param ... Additional arguments (ignored)
#' @return A summary.beezdemand_fixed object (inherits from beezdemand_summary)
#' @export
summary.beezdemand_fixed <- function(
  object,
  report_space = c("natural", "log10"),
  ...
) {
  report_space <- match.arg(report_space)
  # Build coefficients tibble from results if available
  if (!is.null(object$results) && is.data.frame(object$results) &&
      nrow(object$results) > 0) {
    results <- object$results
    id_values <- beezdemand_fixed_id_values(results)

    param_specs <- beezdemand_fixed_param_specs(results)

    coefficients_list <- lapply(names(param_specs), function(term_name) {
      spec <- param_specs[[term_name]]
      tibble::tibble(
        id = id_values,
        term = term_name,
        estimate = results[[spec$estimate]],
        std.error = if (!is.na(spec$se) && spec$se %in% names(results)) results[[spec$se]] else NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        component = "fixed",
        estimate_scale = "natural",
        term_display = term_name
      )
    })
    coefficients <- dplyr::bind_rows(coefficients_list)
    if (report_space == "log10") {
      # Use suppressWarnings to avoid NaN warnings from log10 of non-positive values
      # The case_when condition handles these cases, but log10 is evaluated first
      coefficients <- suppressWarnings(coefficients |>
        dplyr::mutate(
          estimate_internal = .data$estimate,
          estimate = dplyr::case_when(
            .data$term %in% c("Q0", "alpha", "k") & .data$estimate > 0 ~ log10(.data$estimate),
            .data$term %in% c("Q0", "alpha", "k") & .data$estimate <= 0 ~ NA_real_,
            TRUE ~ .data$estimate
          ),
          std.error = dplyr::case_when(
            .data$term %in% c("Q0", "alpha", "k") & .data$estimate_internal > 0 ~
              .data$std.error / (.data$estimate_internal * log(10)),
            TRUE ~ .data$std.error
          ),
          estimate_scale = dplyr::case_when(
            .data$term %in% c("Q0", "alpha", "k") ~ "log10",
            TRUE ~ .data$estimate_scale
          ),
          term_display = dplyr::case_when(
            .data$term == "Q0" ~ "log10(Q0)",
            .data$term == "alpha" ~ "log10(alpha)",
            .data$term == "k" ~ "log10(k)",
            TRUE ~ .data$term_display
          )
        ))
    }

    param_summary <- lapply(names(param_specs), function(term_name) {
      vals <- results[[param_specs[[term_name]]$estimate]]
      vals <- vals[!is.na(vals)]
      # Transform to log10 scale if requested and parameter supports it
      if (report_space == "log10" && term_name %in% c("Q0", "alpha", "k")) {
        # Only transform positive values; filter out non-positive
        vals <- vals[vals > 0]
        if (length(vals) > 0) {
          vals <- log10(vals)
        }
      }
      if (length(vals) > 0) summary(vals) else NULL
    })
    names(param_summary) <- names(param_specs)

    # Count observations if data_used is available
    nobs <- if (!is.null(object$data_used)) {
      sum(vapply(object$data_used, nrow, integer(1)))
    } else {
      NA_integer_
    }
  } else {
    coefficients <- tibble::tibble(
      id = character(),
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric(),
      component = character()
    )
    param_summary <- list()
    nobs <- NA_integer_
  }

  # Compute derived metrics (pmax/omax) per subject using unified engine
  derived_metrics <- beezdemand_empty_derived_metrics()
  pmax_method_info <- list()
  
  if (!is.null(object$results) && is.data.frame(object$results) &&
      nrow(object$results) > 0 && object$equation %in% c("hs", "koff")) {
    # Get parameter columns
    q0_col <- if ("Q0" %in% names(object$results)) "Q0" else NULL
    alpha_col <- if ("Alpha" %in% names(object$results)) "Alpha" else NULL
    k_col <- if ("K" %in% names(object$results)) "K" else NULL
    
    if (!is.null(q0_col) && !is.null(alpha_col) && !is.null(k_col)) {
      # Determine parameter scale based on param_space
      param_scale <- object$param_space %||% "natural"
      
      # For each subject, compute pmax/omax
      pmax_results <- lapply(seq_len(nrow(object$results)), function(i) {
        row <- object$results[i, ]
        
        # Get price observations for this subject if available
        price_obs <- NULL
        if (!is.null(object$data_used) && length(object$data_used) >= i) {
          price_obs <- object$data_used[[i]]$x
        }
        
        beezdemand_calc_pmax_omax(
          model_type = object$equation,
          params = list(
            alpha = row[[alpha_col]],
            q0 = row[[q0_col]],
            k = row[[k_col]]
          ),
          param_scales = list(
            alpha = param_scale,
            q0 = param_scale,
            k = "natural"  # k is typically natural even when others are log10
          ),
          price_obs = price_obs,
          compute_observed = FALSE
        )
      })
      
      # Aggregate results
      pmax_vals <- sapply(pmax_results, function(x) x$pmax_model)
      omax_vals <- sapply(pmax_results, function(x) x$omax_model)
      methods <- sapply(pmax_results, function(x) x$method_model)
      
      # Store per-subject in results
      object$results$pmax_model <- pmax_vals
      object$results$omax_model <- omax_vals
      object$results$pmax_method <- methods
      
      # Summary metrics
      pmax_summary <- if (any(!is.na(pmax_vals))) summary(pmax_vals[!is.na(pmax_vals)]) else NULL
      omax_summary <- if (any(!is.na(omax_vals))) summary(omax_vals[!is.na(omax_vals)]) else NULL
      
      param_summary$pmax_model <- pmax_summary
      param_summary$omax_model <- omax_summary
      
      # Method info (use most common method)
      if (length(methods) > 0) {
        method_table <- table(methods[!is.na(methods)])
        if (length(method_table) > 0) {
          pmax_method_info <- list(
            method_model = names(which.max(method_table)),
            n_analytic = sum(grepl("analytic", methods, ignore.case = TRUE)),
            n_numerical = sum(grepl("numerical", methods, ignore.case = TRUE))
          )
        }
      }
    }
  }

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_fixed",
      backend = "legacy",
      equation = object$equation,
      param_space = object$param_space %||% "natural",
      report_space = report_space,
      k_spec = object$k_spec,
      agg = object$agg,
      nobs = nobs,
      n_subjects = object$n_total,
      n_success = object$n_success,
      n_fail = object$n_fail,
      converged = NA,
      logLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      coefficients = coefficients,
      derived_metrics = derived_metrics,
      param_summary = param_summary,
      pmax_method_info = pmax_method_info,
      results = object$results,
      notes = character(0)
    ),
    class = c("summary.beezdemand_fixed", "beezdemand_summary")
  )
}

#' Print Method for summary.beezdemand_fixed
#'
#' @param x A summary.beezdemand_fixed object
#' @param digits Number of significant digits to print
#' @param n Number of subjects (ids) to print (default 20)
#' @param ... Additional arguments (ignored)
#' @export
print.summary.beezdemand_fixed <- function(x, digits = 4, n = 20, ...) {
  cat("\n")
  cat("Fixed-Effect Demand Model Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat("Equation:", x$equation, "\n")
  cat("k:", x$k_spec, "\n")
  if (!is.null(x$agg)) {
    cat("Aggregation:", x$agg, "\n")
  }
  cat("\n")

  cat("Fit Summary:\n")
  cat("  Total subjects:", x$n_subjects, "\n")
  cat("  Converged:", x$n_success, "\n")
  cat("  Failed:", x$n_fail, "\n")
  if (!is.na(x$nobs)) {
    cat("  Total observations:", x$nobs, "\n")
  }
  cat("\n")

  if (length(x$param_summary) > 0) {
    cat("Parameter Summary (across subjects):\n")

    if (!is.null(x$param_summary$Q0)) {
      q0_sum <- x$param_summary$Q0
      cat("  Q0:\n")
      cat("    Median:", round(q0_sum["Median"], digits), "\n")
      cat("    Range: [", round(q0_sum["Min."], digits), ",",
          round(q0_sum["Max."], digits), "]\n")
    }

    if (!is.null(x$param_summary$alpha)) {
      alpha_sum <- x$param_summary$alpha
      cat("  alpha:\n")
      cat("    Median:", round(alpha_sum["Median"], 6), "\n")
      cat("    Range: [", round(alpha_sum["Min."], 6), ",",
          round(alpha_sum["Max."], 6), "]\n")
    }
  }

  if (!is.null(x$coefficients) && nrow(x$coefficients) > 0) {
    n_to_print <- n
    if (is.null(n_to_print)) {
      n_to_print <- 20
    }

    coef_all <- x$coefficients |>
      dplyr::mutate(id = as.character(.data$id), term = as.character(.data$term)) |>
      dplyr::arrange(.data$id, .data$term)

    ids <- unique(coef_all$id)
    ids_to_print <- ids
    if (is.finite(n_to_print) && length(ids) > n_to_print) {
      ids_to_print <- utils::head(ids, n_to_print)
    }

    coef_print <- coef_all |>
      dplyr::filter(.data$id %in% ids_to_print) |>
      dplyr::arrange(.data$id, .data$term)

    cat("\nPer-subject coefficients:\n")
    cat("-------------------------\n")
    print(coef_print, row.names = FALSE)

    if (is.finite(n_to_print) && length(ids) > n_to_print) {
      cat("\n(Showing first", n_to_print, "ids of", length(ids), ")\n")
    }
  }

  if (length(x$notes) > 0) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat("  -", note, "\n")
    }
  }

  invisible(x)
}

#' Tidy Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object
#' @param report_space Character. Reporting space for core parameters. One of
#'   `"natural"` (default) or `"log10"`.
#' @param ... Additional arguments (ignored)
#' @return A tibble of model coefficients with columns: id, term, estimate,
#'   std.error, statistic, p.value, component
#' @export
tidy.beezdemand_fixed <- function(
  x,
  report_space = c("natural", "log10"),
  ...
) {
  report_space <- match.arg(report_space)
  if (is.null(x$results) || !is.data.frame(x$results) || nrow(x$results) == 0) {
    return(tibble::tibble(
      id = character(),
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric(),
      component = character()
    ))
  }
  results <- x$results
  id_values <- beezdemand_fixed_id_values(results)

  param_specs <- beezdemand_fixed_param_specs(results)
  coefficient_rows <- lapply(names(param_specs), function(term_name) {
    spec <- param_specs[[term_name]]
    tibble::tibble(
      id = id_values,
      term = term_name,
      estimate = results[[spec$estimate]],
      std.error = if (!is.na(spec$se) && spec$se %in% names(results)) results[[spec$se]] else NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      component = "fixed",
      estimate_scale = "natural",
      term_display = term_name
    )
  })

  out <- dplyr::bind_rows(coefficient_rows)
  beezdemand_transform_coef_table(
    coef_tbl = out,
    report_space = report_space,
    internal_space = "natural"
  )
}

#' Extract Coefficients from Fixed-Effect Demand Fit
#'
#' @param object A `beezdemand_fixed` object.
#' @param report_space One of `"internal"`, `"natural"`, or `"log10"`. Default `"internal"`.
#' @param ... Unused.
#' @return A tibble with columns `id`, `term`, `estimate`, `estimate_scale`, `term_display`.
#' @export
coef.beezdemand_fixed <- function(
  object,
  report_space = c("internal", "natural", "log10"),
  ...
) {
  report_space <- match.arg(report_space)

  if (is.null(object$fits) || !length(object$fits)) {
    # Fallback: no per-id model objects available; use results table (natural scale).
    out <- tidy.beezdemand_fixed(object, report_space = "natural")
    if (report_space == "internal") return(out)
    return(tidy.beezdemand_fixed(object, report_space = report_space))
  }

  ids <- names(object$fits)
  rows <- lapply(seq_along(object$fits), function(i) {
    fit <- object$fits[[i]]
    id <- if (!is.null(ids) && length(ids) >= i) ids[[i]] else NA_character_
    if (inherits(fit, "try-error") || is.null(fit)) return(NULL)
    cf <- tryCatch(stats::coef(fit), error = function(e) NULL)
    if (is.null(cf)) return(NULL)
    tibble::tibble(
      id = as.character(id),
      term = names(cf),
      estimate = as.numeric(cf)
    )
  })

  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    return(tibble::tibble(
      id = character(),
      term = character(),
      estimate = numeric(),
      estimate_scale = character(),
      term_display = character()
    ))
  }

  internal_space <- object$param_space %||% "natural"
  internal_space <- if (internal_space == "log10") "log10" else "natural"
  requested <- if (report_space == "internal") internal_space else report_space

  out <- out |>
    dplyr::mutate(
      estimate_scale = internal_space,
      term_display = .data$term
    )

  if (requested != internal_space) {
    out <- out |>
      dplyr::mutate(
        estimate_internal = .data$estimate
      )

    if (internal_space == "natural" && requested == "log10") {
      out <- out |>
        dplyr::mutate(
          estimate = dplyr::case_when(
            .data$term %in% c("q0", "alpha", "k") & .data$estimate > 0 ~ log10(.data$estimate),
            TRUE ~ .data$estimate
          ),
          estimate_scale = dplyr::case_when(
            .data$term %in% c("q0", "alpha", "k") ~ "log10",
            TRUE ~ .data$estimate_scale
          ),
          term_display = dplyr::case_when(
            .data$term == "q0" ~ "log10(Q0)",
            .data$term == "alpha" ~ "log10(alpha)",
            .data$term == "k" ~ "log10(k)",
            TRUE ~ .data$term
          )
        )
    } else if (internal_space == "log10" && requested == "natural") {
      out <- out |>
        dplyr::mutate(
          estimate = dplyr::case_when(
            .data$term %in% c("q0", "alpha", "k") ~ 10^.data$estimate,
            TRUE ~ .data$estimate
          ),
          estimate_scale = dplyr::case_when(
            .data$term %in% c("q0", "alpha", "k") ~ "natural",
            TRUE ~ .data$estimate_scale
          ),
          term_display = dplyr::case_when(
            .data$term == "q0" ~ "Q0",
            .data$term == "alpha" ~ "alpha",
            .data$term == "k" ~ "k",
            TRUE ~ .data$term
          )
        )
    }
  }

  out
}

#' Glance Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object
#' @param ... Additional arguments (ignored)
#' @return A one-row tibble of model statistics
#' @export
glance.beezdemand_fixed <- function(x, ...) {
  # Count observations if data_used is available
  nobs <- if (!is.null(x$data_used)) {
    sum(vapply(x$data_used, nrow, integer(1)))
  } else {
    NA_integer_
  }

  tibble::tibble(
    model_class = "beezdemand_fixed",
    backend = "legacy",
    equation = x$equation,
    k_spec = x$k_spec,
    nobs = nobs,
    n_subjects = x$n_total,
    n_success = x$n_success,
    n_fail = x$n_fail,
    converged = NA,
    logLik = NA_real_,
    AIC = NA_real_,
    BIC = NA_real_
  )
}
