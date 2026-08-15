# Parametric bootstrap CIs on derived demand metrics (TICKET-024)

#' Bootstrap Confidence Intervals for Derived Demand Metrics
#'
#' @description
#' Computes confidence intervals on derived demand metrics (Pmax, Omax, Qmax,
#' EV, elasticity-at-Pmax) for a TMB mixed-effects demand fit, via a parametric
#' bootstrap. Draws of the fixed-effect parameter vector are taken from the
#' joint asymptotic Gaussian posterior \eqn{N(\hat\beta, \hat\Sigma)}, mapped to
#' per-condition \eqn{(Q_0, \alpha, k)} through the model's fixed-effect design,
#' passed through the canonical Pmax/Omax engine, and summarized by empirical
#' quantiles.
#'
#' For a factor-expanded fit, one CI is returned per factor cell (e.g. one row
#' per `gender` level); for an intercept-only fit, one row per statistic with
#' `condition = NA`. The per-cell point estimate reproduces
#' \code{\link{calc_group_metrics}(fit, at = cell)}; the bootstrap supplies only
#' the interval.
#'
#' @details
#' The parametric bootstrap is asymptotically equivalent to the delta method but
#' avoids its linearization, so it is the more defensible recourse for the
#' strongly nonlinear derived metrics (Pmax/Omax via Lambert-W). Draws are
#' fixed-effect-only (population / per-condition metrics); per-subject metric CIs
#' would require random-effect-aware draws and are out of scope for now.
#'
#' When `k` is estimated, its uncertainty is propagated (the `log_k` column is in
#' the draw matrix); when `k` is fixed, the fixed value is used. The point
#' estimate always uses the point `k`, matching \code{calc_group_metrics()}.
#'
#' Note that percentile intervals of a nonlinear transform are not guaranteed to
#' bracket the point estimate; `conf.low <= conf.high` always holds, but
#' `conf.low <= estimate <= conf.high` may not at boundary cases.
#'
#' Some draws can leave a metric's domain (e.g. Pmax is undefined when a drawn
#' `k` falls below the Lambert-W threshold). Such non-finite draws are excluded
#' from the quantiles; the per-row count of excluded draws is recorded in
#' `attr(x, "n_nonfinite")` (so the realized draw count is `R` minus that). If
#' *every* draw of a requested metric/condition is non-finite, an error is
#' raised because the interval is undefined.
#'
#' @param fit A \code{beezdemand_tmb} object. NLME (\code{beezdemand_nlme}) and
#'   hurdle fits are not supported in this version and error helpfully.
#' @param statistics Character vector; any of
#'   \code{c("Pmax", "Omax", "Qmax", "EV", "elasticity_at_pmax")}. Default
#'   \code{c("Pmax", "Omax", "EV")}.
#' @param method Resampling scheme. Only \code{"parametric"} is available in this
#'   version (nonparametric subject resampling is planned).
#' @param R Integer number of bootstrap draws; minimum 100, default 1000.
#' @param ci_level Confidence level for the empirical-quantile interval
#'   (default 0.95).
#' @param at Optional named list of factor-level filters / continuous-covariate
#'   value overrides, with the same shape as the `at` argument of
#'   \code{\link{calc_group_metrics}}. When `NULL` (default) all factor cells are
#'   enumerated; supplying `at` conditions to (or filters) the requested cell(s).
#' @param seed Optional integer seed for reproducible draws. The caller's RNG
#'   state is left unperturbed.
#' @param ... Reserved for future extension; must be empty. Unknown arguments
#'   (e.g. a misspelled `statistics`) raise an error rather than being silently
#'   ignored.
#'
#' @return A tibble with one row per `(statistic, condition)`:
#'   \describe{
#'     \item{statistic}{Metric name.}
#'     \item{condition}{Factor-cell label (e.g. \code{"gender=Male"}); `NA` when
#'       the fit has no factors.}
#'     \item{estimate}{Point estimate (from the coefficient vector and point `k`).}
#'     \item{conf.low, conf.high}{Empirical-quantile interval bounds.}
#'     \item{level}{The confidence level used.}
#'   }
#'   The bootstrap settings are attached as attributes `"method"`, `"R"`, and
#'   `"seed"`, plus `"n_nonfinite"` (a per-row count of draws excluded as
#'   non-finite).
#'
#' @examples
#' \donttest{
#' data(apt, package = "beezdemand")
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' boot_demand(fit, statistics = c("Pmax", "Omax", "EV"), R = 500, seed = 1)
#' }
#'
#' @seealso \code{\link{calc_group_metrics}}, \code{\link{fit_demand_tmb}},
#'   \code{\link{confint.beezdemand_tmb}}
#' @export
boot_demand <- function(
  fit,
  statistics = c("Pmax", "Omax", "EV"),
  method     = c("parametric"),
  R          = 1000L,
  ci_level   = 0.95,
  at         = NULL,
  seed       = NULL,
  ...
) {
  # ---- validation ----------------------------------------------------------
  rlang::check_dots_empty()
  if (!inherits(fit, "beezdemand_tmb")) {
    cls <- class(fit)[1]
    cli::cli_abort(c(
      "{.fn boot_demand} supports {.cls beezdemand_tmb} fits only.",
      "x" = "Received a {.cls {cls}} object.",
      "i" = "NLME ({.cls beezdemand_nlme}) and hurdle support is planned for a follow-up."
    ))
  }
  method <- match.arg(method)

  all_stats <- c("Pmax", "Omax", "Qmax", "EV", "elasticity_at_pmax")
  statistics <- match.arg(statistics, choices = all_stats, several.ok = TRUE)

  if (!is.numeric(R) || length(R) != 1L || !is.finite(R) ||
        R < 100 || R != round(R)) {
    cli::cli_abort(c(
      "{.arg R} must be a single whole number >= 100.",
      "i" = "Recommend {.code R >= 1000} for stable quantile estimates."
    ))
  }
  R <- as.integer(R)
  if (!is.numeric(ci_level) || length(ci_level) != 1L ||
        is.na(ci_level) || ci_level <= 0 || ci_level >= 1) {
    cli::cli_abort("{.arg ci_level} must be a single number in (0, 1).")
  }

  # Divergent-factor guard (v1 boundary): `collapse_levels` is the only public
  # path to factors_q0 != factors_alpha; the union-grid mapping it would need is
  # out of scope here.
  if (!identical(fit$param_info$factors_q0, fit$param_info$factors_alpha)) {
    cli::cli_abort(c(
      "{.fn boot_demand} does not support fits made with {.arg collapse_levels}.",
      "x" = "Q0 and alpha have divergent factor structure.",
      "i" = "Refit without {.arg collapse_levels} for bootstrap CIs (union-grid support is planned)."
    ))
  }

  has_k <- isTRUE(fit$param_info$has_k)
  is_zben <- identical(fit$param_info$equation, "zben")
  # zben (no k, LL4-scale decay) has no SND closed form; routed through the
  # engine's numerical fallback instead (GH #19). Needs a price domain for
  # the numerical search, which "hs"/"snd" do not (both have analytic
  # closed-form Pmax).
  model_type <- if (has_k) "hs" else if (is_zben) "zben" else "snd"
  param_scales <- if (has_k) {
    list(alpha = "natural", q0 = "natural", k = "natural")
  } else {
    list(alpha = "natural", q0 = "natural")
  }
  price_range_boot <- if (is_zben) {
    range(fit$data[[fit$param_info$x_var]], na.rm = TRUE)
  } else {
    NULL
  }

  # ---- cells (validate `at` once, then build aligned Q0/alpha designs) -----
  .tmb_validate_at(fit, at)
  cells <- .boot_demand_cells(fit, at)
  n_cell <- nrow(cells$X_q0)

  coefs <- fit$model$coefficients
  beta_q0 <- coefs[names(coefs) == "beta_q0"]
  beta_alpha <- coefs[names(coefs) == "beta_alpha"]
  k_point <- .tmb_get_k(fit)

  # ---- parametric draws on the fixed-effect block --------------------------
  draws <- .tmb_parametric_draws(fit, R = R, seed = seed)
  bq0 <- draws[, colnames(draws) == "beta_q0", drop = FALSE]
  ba  <- draws[, colnames(draws) == "beta_alpha", drop = FALSE]
  k_draw <- if ("log_k" %in% colnames(draws)) {
    exp(draws[, "log_k"])
  } else {
    rep(k_point, R)
  }

  # Defensive: design columns, coefficient vectors, and draw-matrix widths must
  # align, or `sum(xq * beta_q0)` would silently recycle and corrupt estimates
  # (mirrors the explicit dimension guard in get_demand_param_emms()).
  if (ncol(cells$X_q0) != length(beta_q0) ||
        ncol(cells$X_alpha) != length(beta_alpha) ||
        ncol(bq0) != length(beta_q0) || ncol(ba) != length(beta_alpha)) {
    cli::cli_abort(
      "Internal error: design / coefficient / draw dimensions are misaligned."
    )
  }

  probs <- c((1 - ci_level) / 2, (1 + ci_level) / 2)
  stat_col <- c(
    Pmax = "pmax_model", Omax = "omax_model", Qmax = "q_at_pmax_model",
    elasticity_at_pmax = "elasticity_at_pmax_model"
  )

  nf_vec <- integer(0)
  # Codex review of GH #19 (BLOCKING follow-up): count zben bootstrap draws
  # whose numerical Pmax search hit the engine's domain-expansion cap
  # without finding the true (interior) maximum. Surfaced as a warning
  # (not attached per-draw to the returned tibble, which has a fixed,
  # already-documented schema) naming how many draws were affected.
  n_boundary_draws <- 0L
  rows <- list()
  for (i in seq_len(n_cell)) {
    xq <- cells$X_q0[i, ]
    xa <- cells$X_alpha[i, ]

    # Point estimate: identical path to calc_group_metrics(at = cell).
    q0_pt <- exp(sum(xq * beta_q0))
    alpha_pt <- exp(sum(xa * beta_alpha))
    pt_params <- if (has_k) {
      list(alpha = alpha_pt, q0 = q0_pt, k = k_point)
    } else {
      list(alpha = alpha_pt, q0 = q0_pt)
    }
    pt <- beezdemand_calc_pmax_omax(
      model_type = model_type, params = pt_params, param_scales = param_scales,
      price_obs = price_range_boot
    )
    # EV mirrors analyze.R's two conventions exactly: k-bearing forms use the
    # literature (Hursh & Silberberg) formula 1/(100*alpha*k^1.5); the k-free
    # SND/"simplified" form has no k term at all, so analyze.R's own
    # "simplified" branch uses 1/alpha (no /100). Do not conflate the two.
    ev_pt <- if (has_k) {
      1 / (100 * alpha_pt * (k_point^1.5))
    } else {
      1 / alpha_pt
    }
    point_vals <- c(
      Pmax = pt$pmax_model,
      Omax = pt$omax_model,
      Qmax = pt$q_at_pmax_model,
      EV = ev_pt,
      elasticity_at_pmax = pt$elasticity_at_pmax_model
    )

    # Per-draw metrics through the canonical engine.
    q0_draws <- exp(as.numeric(bq0 %*% xq))
    alpha_draws <- exp(as.numeric(ba %*% xa))
    pdf <- if (has_k) {
      data.frame(q0 = q0_draws, alpha = alpha_draws, k = k_draw)
    } else {
      data.frame(q0 = q0_draws, alpha = alpha_draws)
    }
    price_list_boot <- if (is_zben) {
      replicate(R, price_range_boot, simplify = FALSE)
    } else {
      NULL
    }
    md <- beezdemand_calc_pmax_omax_vec(
      pdf, model_type = model_type, param_scales = param_scales,
      price_list = price_list_boot
    )
    if (is_zben) {
      n_boundary_draws <- n_boundary_draws +
        sum(md$is_boundary_model, na.rm = TRUE)
    }
    ev_draws <- if (has_k) {
      1 / (100 * alpha_draws * (k_draw^1.5))
    } else {
      1 / alpha_draws
    }

    for (s in statistics) {
      draw_vec <- if (s == "EV") ev_draws else md[[stat_col[[s]]]]
      ci <- .boot_demand_ci(draw_vec, probs, s, cells$labels[i])
      nf_vec <- c(nf_vec, ci$n_failed)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        statistic = s,
        condition = cells$labels[i],
        estimate  = unname(point_vals[[s]]),
        conf.low  = ci$conf.low,
        conf.high = ci$conf.high,
        level     = ci_level
      )
    }
  }

  # Codex review of GH #19 (BLOCKING follow-up): warn (rather than silently
  # returning underestimated Pmax/Omax/Qmax/elasticity_at_pmax) when zben
  # bootstrap draws hit the numerical Pmax search's domain-expansion cap.
  pmax_derived_stats <- c("Pmax", "Omax", "Qmax", "elasticity_at_pmax")
  if (is_zben && n_boundary_draws > 0L && any(statistics %in% pmax_derived_stats)) {
    cli::cli_warn(c(
      "!" = "{n_boundary_draws} zben bootstrap draw{?s} hit the Pmax search's domain-expansion cap without finding the true (interior) maximum.",
      "i" = "Affected draws' Pmax/Omax/Qmax/elasticity_at_pmax are underestimated (lower-bound only); the reported CI may be too narrow.",
      "i" = "See {.field pmax_at_bound} in {.fn get_subject_pars} / {.fn calc_group_metrics} for the point-estimate diagnostic."
    ))
  }

  result <- tibble::as_tibble(do.call(rbind, rows))
  attr(result, "method") <- method
  attr(result, "R") <- as.integer(R)
  attr(result, "seed") <- seed
  # Per-row count of draws excluded as non-finite (boundary draws where a metric
  # is undefined -- e.g. Pmax when a drawn k falls below the Lambert-W domain).
  # Surfaced as an attribute rather than a warning: such draws are an inherent,
  # often-routine feature of the parametric bootstrap of these metrics, so a
  # warning would be noise. `.boot_demand_ci()` still errors if *all* draws of a
  # requested metric/condition are non-finite (the interval is then undefined).
  attr(result, "n_nonfinite") <- nf_vec
  result
}

# Empirical-quantile CI for one metric's bootstrap draws. Non-finite draws
# (engine returns NA at a boundary, or EV when alpha under/overflows) are
# excluded and counted; if *every* draw is non-finite the CI is undefined and we
# abort rather than return a meaningless interval computed from nothing.
.boot_demand_ci <- function(draw_vec, probs, statistic, condition) {
  ok <- is.finite(draw_vec)
  if (!any(ok)) {
    where <- if (is.na(condition)) statistic else paste0(statistic, " (", condition, ")")
    cli::cli_abort(c(
      "All {length(draw_vec)} bootstrap draws of {.val {where}} were non-finite.",
      "i" = "The fit may sit at a boundary where this metric is undefined; no CI can be formed."
    ))
  }
  qs <- stats::quantile(draw_vec[ok], probs = probs, names = FALSE)
  list(conf.low = qs[[1L]], conf.high = qs[[2L]], n_failed = sum(!ok))
}

# Build per-condition cell designs for boot_demand(). Returns aligned Q0/alpha
# design matrices (one row per cell) plus cell labels. Relies on the caller's
# guarantee that factors_q0 == factors_alpha, so a single shared reference grid
# describes both parameters' cells.
.boot_demand_cells <- function(fit, at = NULL) {
  grid_q0 <- .tmb_build_emm_ref_grid(fit, param = "Q0", at = at, validate = FALSE)
  grid_alpha <- .tmb_build_emm_ref_grid(fit, param = "alpha", at = at, validate = FALSE)

  if (isTRUE(grid_q0$is_intercept_only)) {
    return(list(
      labels = NA_character_,
      X_q0 = matrix(1, nrow = 1L, ncol = 1L),
      X_alpha = matrix(1, nrow = 1L, ncol = 1L)
    ))
  }

  use_factors <- grid_q0$use_factors
  level_combos <- grid_q0$level_combos
  n_cell <- nrow(grid_q0$ref_X)

  if (length(use_factors) > 0L) {
    labels <- vapply(seq_len(n_cell), function(i) {
      paste(
        vapply(use_factors, function(f) {
          paste0(f, "=", level_combos[[f]][i])
        }, character(1)),
        collapse = ", "
      )
    }, character(1))
  } else {
    # Covariate-only fit: no factor cells, single covariate-at-mean row.
    labels <- rep(NA_character_, n_cell)
  }

  list(labels = labels, X_q0 = grid_q0$ref_X, X_alpha = grid_alpha$ref_X)
}
