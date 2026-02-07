#' Plot Helpers (Internal)
#'
#' @description
#' Internal utilities for consistent plotting behavior.
#'
#' @noRd
NULL

beezdemand_is_log_scale <- function(trans) {
  trans %in% c("log", "log10")
}

beezdemand_normalize_show_pred <- function(show_pred) {
  if (is.character(show_pred) && length(show_pred) == 1 && show_pred == "both") {
    return(c("population", "individual"))
  }
  show_pred
}

beezdemand_normalize_plot_labels <- function(x_lab, y_lab, xlab, ylab) {
  if (!is.null(xlab)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "plot(x, xlab = )",
      "plot(x, x_lab = )"
    )
    if (is.null(x_lab)) {
      x_lab <- xlab
    }
  }
  if (!is.null(ylab)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "plot(x, ylab = )",
      "plot(x, y_lab = )"
    )
    if (is.null(y_lab)) {
      y_lab <- ylab
    }
  }
  list(x_lab = x_lab, y_lab = y_lab)
}

beezdemand_apply_free_trans <- function(data, x_col, x_trans, free_trans) {
  if (is.null(data) || !x_col %in% names(data)) {
    return(list(data = data, replaced = FALSE))
  }

  if (!beezdemand_is_log_scale(x_trans)) {
    return(list(data = data, replaced = FALSE))
  }

  x_vals <- data[[x_col]]
  keep <- is.finite(x_vals)

  if (is.null(free_trans)) {
    keep <- keep & x_vals > 0
    data <- data[keep, , drop = FALSE]
    return(list(data = data, replaced = FALSE))
  }

  keep <- keep & x_vals >= 0
  data <- data[keep, , drop = FALSE]
  x_vals <- data[[x_col]]
  replaced <- any(x_vals == 0, na.rm = TRUE)
  x_vals[x_vals == 0] <- free_trans
  data[[x_col]] <- x_vals
  list(data = data, replaced = replaced)
}

beezdemand_drop_nonpositive_y <- function(data, y_col, y_trans) {
  if (is.null(data) || !y_col %in% names(data)) {
    return(list(data = data, dropped = FALSE))
  }

  if (!beezdemand_is_log_scale(y_trans)) {
    return(list(data = data, dropped = FALSE))
  }

  y_vals <- data[[y_col]]
  keep <- is.finite(y_vals) & y_vals > 0
  list(
    data = data[keep, , drop = FALSE],
    dropped = any(!keep)
  )
}

beezdemand_resolve_y_trans <- function(y_trans, y_is_log) {
  if (isTRUE(y_is_log) && beezdemand_is_log_scale(y_trans)) {
    return(list(y_trans = "linear", adjusted = TRUE))
  }
  list(y_trans = y_trans, adjusted = FALSE)
}

beezdemand_warn_log_override <- function(adjusted) {
  if (isTRUE(adjusted)) {
    message(
      "y appears to be already on a log scale; using y_trans = \"linear\" ",
      "to avoid double-logging. Provide a back-transform via inv_fun if needed."
    )
  }
}

beezdemand_warn_free_trans <- function(replaced, free_trans) {
  if (isTRUE(replaced)) {
    message("Free is shown as `", free_trans, "` for purposes of plotting.")
  }
}

beezdemand_get_trans <- function(trans) {
  switch(
    trans,
    log = scales::log_trans(),
    log10 = scales::log10_trans(),
    pseudo_log = scales::pseudo_log_trans(),
    linear = scales::identity_trans(),
    scales::identity_trans()
  )
}

beezdemand_axis_labels <- function(accuracy = 0.01) {
  scales::label_number(accuracy = accuracy, trim = TRUE, big.mark = ",")
}

beezdemand_resolve_limits <- function(limits, trans, axis = "y") {
  if (is.null(limits)) {
    if (beezdemand_is_log_scale(trans) && axis == "y") {
      return(c(0.01, NA_real_))
    }
    return(NULL)
  }

  if (!is.numeric(limits) || length(limits) != 2) {
    stop(axis, "_limits must be a numeric vector of length 2.")
  }

  if (beezdemand_is_log_scale(trans) && !is.na(limits[1]) && limits[1] <= 0) {
    stop(axis, "_limits lower bound must be > 0 for log scales.")
  }

  limits
}

beezdemand_style_color <- function(style, key = "primary") {
  if (identical(style, "apa")) {
    return(switch(
      key,
      primary = "grey20",
      secondary = "grey50",
      accent = "grey60",
      dark = "grey20",
      "grey30"
    ))
  }
  beezdemand_colors[[key]] %||% "#2B4560"
}

beezdemand_style_scales <- function(p, style, color = TRUE, fill = FALSE) {
  if (identical(style, "apa")) {
    if (color) {
      p <- p + ggplot2::scale_color_grey(start = 0.2, end = 0.7)
    }
    if (fill) {
      p <- p + ggplot2::scale_fill_grey(start = 0.2, end = 0.7)
    }
    return(p)
  }

  if (color) {
    p <- p + scale_color_beezdemand()
  }
  if (fill) {
    p <- p + scale_fill_beezdemand()
  }
  p
}

beezdemand_apply_color_scale <- function(p, style, data, color_col, max_colors = 6) {
  if (is.null(color_col) || is.null(data) || !color_col %in% names(data)) {
    return(p)
  }

  n_groups <- length(unique(data[[color_col]]))
  if (identical(style, "apa") || n_groups > max_colors) {
    return(p + ggplot2::scale_color_grey(start = 0.2, end = 0.7))
  }

  p + scale_color_beezdemand()
}

beezdemand_default_y_trans <- function(type = "demand",
                                       equation = NULL,
                                       y_is_log = FALSE) {
  if (type %in% c("probability", "parameters")) {
    return("linear")
  }

  if (!is.null(equation)) {
    equation <- normalize_equation(equation)
    if (equation == "hs") {
      return("log10")
    }
    if (equation == "koff") {
      return("linear")
    }
    if (equation == "linear") {
      return("log10")
    }
  }

  if (isTRUE(y_is_log)) {
    return("linear")
  }

  "log10"
}
