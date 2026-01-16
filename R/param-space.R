beezdemand_validate_param_space <- function(
  param_space,
  choices = c("natural", "log10")
) {
  if (is.null(param_space) || length(param_space) != 1) {
    stop("'param_space' must be a single character value.", call. = FALSE)
  }
  param_space <- as.character(param_space)
  if (!param_space %in% choices) {
    stop(
      "'param_space' must be one of: ",
      paste(sprintf('"%s"', choices), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  param_space
}

beezdemand_validate_report_space <- function(
  report_space,
  choices = c("natural", "log", "log10", "internal")
) {
  if (is.null(report_space) || length(report_space) != 1) {
    stop("'report_space' must be a single character value.", call. = FALSE)
  }
  report_space <- as.character(report_space)
  if (!report_space %in% choices) {
    stop(
      "'report_space' must be one of: ",
      paste(sprintf('"%s"', choices), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  report_space
}

beezdemand_term_display_space <- function(term, report_space) {
  if (is.na(term) || is.null(term)) return(NA_character_)
  term <- as.character(term)
  report_space <- as.character(report_space)

  if (grepl("^Q0", term)) {
    suffix <- sub("^Q0", "", term)
    prefix <- dplyr::case_when(
      report_space == "log10" ~ "log10(Q0)",
      report_space == "log" ~ "log(Q0)",
      TRUE ~ "Q0"
    )
    return(paste0(prefix, suffix))
  }
  if (grepl("^alpha", term)) {
    suffix <- sub("^alpha", "", term)
    prefix <- dplyr::case_when(
      report_space == "log10" ~ "log10(alpha)",
      report_space == "log" ~ "log(alpha)",
      TRUE ~ "alpha"
    )
    return(paste0(prefix, suffix))
  }
  if (grepl("^k", term)) {
    suffix <- sub("^k", "", term)
    prefix <- dplyr::case_when(
      report_space == "log10" ~ "log10(k)",
      report_space == "log" ~ "log(k)",
      TRUE ~ "k"
    )
    return(paste0(prefix, suffix))
  }
  if (grepl("^Qalone", term)) {
    suffix <- sub("^Qalone", "", term)
    prefix <- dplyr::case_when(
      report_space == "log10" ~ "log10(Qalone)",
      report_space == "log" ~ "log(Qalone)",
      TRUE ~ "Qalone"
    )
    return(paste0(prefix, suffix))
  }
  if (grepl("^beta($|_)", term)) {
    suffix <- sub("^beta", "", term)
    prefix <- dplyr::case_when(
      report_space == "log10" ~ "log10(beta)",
      report_space == "log" ~ "log(beta)",
      TRUE ~ "beta"
    )
    return(paste0(prefix, suffix))
  }
  term
}

beezdemand_param_space_details_core <- function(
  internal_names = list(Q0 = "Q0", alpha = "alpha", k = "k"),
  internal_spaces = list(Q0 = "natural", alpha = "natural", k = "natural")
) {
  list(
    Q0 = list(
      canonical = "Q0",
      internal = internal_names$Q0,
      internal_space = internal_spaces$Q0
    ),
    alpha = list(
      canonical = "alpha",
      internal = internal_names$alpha,
      internal_space = internal_spaces$alpha
    ),
    k = list(
      canonical = "k",
      internal = internal_names$k,
      internal_space = internal_spaces$k
    )
  )
}

beezdemand_transform_est_se <- function(estimate, se, from, to) {
  if (identical(from, to) || to == "internal") {
    return(list(estimate = estimate, se = se))
  }

  ln10 <- log(10)

  if (from == "natural" && to == "log10") {
    new_est <- ifelse(is.finite(estimate) & estimate > 0, log10(estimate), NA_real_)
    new_se <- ifelse(is.finite(se) & is.finite(estimate) & estimate > 0,
      se / (estimate * ln10),
      NA_real_
    )
    return(list(estimate = new_est, se = new_se))
  }

  if (from == "log10" && to == "natural") {
    new_est <- 10^estimate
    new_se <- ifelse(is.finite(se),
      ln10 * (10^estimate) * se,
      NA_real_
    )
    return(list(estimate = new_est, se = new_se))
  }

  if (from == "log" && to == "natural") {
    new_est <- exp(estimate)
    new_se <- ifelse(is.finite(se),
      exp(estimate) * se,
      NA_real_
    )
    return(list(estimate = new_est, se = new_se))
  }

  if (from == "natural" && to == "log") {
    new_est <- ifelse(is.finite(estimate) & estimate > 0, log(estimate), NA_real_)
    new_se <- ifelse(is.finite(se) & is.finite(estimate) & estimate > 0,
      se / estimate,
      NA_real_
    )
    return(list(estimate = new_est, se = new_se))
  }

  if (from == "log" && to == "log10") {
    new_est <- estimate / ln10
    new_se <- ifelse(is.finite(se), se / ln10, NA_real_)
    return(list(estimate = new_est, se = new_se))
  }

  if (from == "log10" && to == "log") {
    new_est <- estimate * ln10
    new_se <- ifelse(is.finite(se), se * ln10, NA_real_)
    return(list(estimate = new_est, se = new_se))
  }

  stop(
    "Unsupported transform from '",
    from,
    "' to '",
    to,
    "'.",
    call. = FALSE
  )
}

beezdemand_transform_coef_table <- function(
  coef_tbl,
  report_space,
  internal_space,
  term_col = "term",
  estimate_col = "estimate",
  se_col = "std.error",
  scale_col = "estimate_scale",
  display_col = "term_display"
) {
  report_space <- beezdemand_validate_report_space(report_space)

  if (!nrow(coef_tbl)) return(coef_tbl)
  if (!all(c(term_col, estimate_col, se_col) %in% names(coef_tbl))) {
    return(coef_tbl)
  }

  is_core <- coef_tbl[[term_col]] %in% c("Q0", "alpha", "k") |
    grepl("^Q0", coef_tbl[[term_col]]) |
    grepl("^alpha", coef_tbl[[term_col]]) |
    grepl("^k", coef_tbl[[term_col]]) |
    coef_tbl[[term_col]] %in% c("Qalone", "beta") |
    grepl("^Qalone", coef_tbl[[term_col]]) |
    grepl("^beta($|_)", coef_tbl[[term_col]])

  out <- coef_tbl
  if (!("estimate_internal" %in% names(out))) out$estimate_internal <- NA_real_
  if (!(display_col %in% names(out))) out[[display_col]] <- as.character(out[[term_col]])
  if (!(scale_col %in% names(out))) out[[scale_col]] <- NA_character_

  for (i in which(is_core)) {
    term <- as.character(out[[term_col]][i])

    # Infer which base-space applies: Q0 can be log/log10/natural depending on backend,
    # but this helper is called per-backend with explicit internal_space.
    from_space <- out[[scale_col]][i] %||% internal_space
    if (is.na(from_space) || !nzchar(from_space)) from_space <- internal_space

    to_space <- report_space
    if (to_space == "internal") to_space <- from_space

    trans <- beezdemand_transform_est_se(
      estimate = out[[estimate_col]][i],
      se = out[[se_col]][i],
      from = from_space,
      to = to_space
    )

    out$estimate_internal[i] <- out[[estimate_col]][i]
    out[[estimate_col]][i] <- trans$estimate
    out[[se_col]][i] <- trans$se

    out[[scale_col]][i] <- to_space
    out[[display_col]][i] <- beezdemand_term_display_space(term, to_space)
  }

  out
}
