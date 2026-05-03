#' @useDynLib beezdemand, .registration = TRUE
NULL

# ==============================================================================
# Internal Helper Functions for TMB Mixed-Effects Demand Models
# ==============================================================================

#' Prepare Data for TMB Mixed-Effects Demand Model
#'
#' @description
#' Equation-aware data preparation for TMB continuous-only demand models.
#' Handles zero filtering, log transformation, and subject ID mapping.
#'
#' @param data A validated data frame.
#' @param y_var Character string, name of consumption variable.
#' @param x_var Character string, name of price variable.
#' @param id_var Character string, name of subject ID variable.
#' @param equation Character string, one of "exponential", "exponentiated",
#'   "simplified", "zben".
#'
#' @return A list containing prepared data vectors and subject mapping.
#' @keywords internal
.tmb_prepare_data <- function(data, y_var, x_var, id_var, equation) {
  ids <- data[[id_var]]
  price <- as.numeric(data[[x_var]])
  consumption <- as.numeric(data[[y_var]])

  # Equation-specific response handling
  n_zeros <- sum(consumption == 0, na.rm = TRUE)
  n_dropped <- 0L

  if (equation == "exponential") {
    # Filter to Q > 0, compute log(Q). Defensive na-handling: NA in consumption
    # would otherwise make `sum(!pos_idx)` return NA and crash the `if` below.
    pos_idx <- !is.na(consumption) & consumption > 0
    n_dropped <- sum(!pos_idx)
    if (n_dropped > 0) {
      message(sprintf(
        "  equation='exponential': Dropped %d zero-consumption observations (%d remaining).",
        n_dropped, sum(pos_idx)
      ))
    }
    ids <- ids[pos_idx]
    price <- price[pos_idx]
    consumption <- consumption[pos_idx]
    y <- log(consumption)
  } else if (equation == "exponentiated" || equation == "simplified") {
    # Raw Q, zeros OK
    y <- consumption
  } else {
    # zben: user provides LL4-transformed y directly
    y <- consumption
  }

  # Create subject mapping (0-indexed for C++)
  subject_levels <- levels(droplevels(as.factor(ids)))
  n_subjects <- length(subject_levels)
  subject_map <- setNames(
    seq_along(subject_levels) - 1L,
    subject_levels
  )
  subject_id <- as.integer(subject_map[as.character(ids)])

  list(
    y = y,
    price = price,
    subject_id = subject_id,
    subject_levels = subject_levels,
    n_subjects = n_subjects,
    n_obs = length(y),
    n_zeros = n_zeros,
    n_dropped = n_dropped,
    data = data  # Keep original for downstream

  )
}


#' Build Design Matrices for TMB Model
#'
#' @param data Data frame with factors already applied.
#' @param factors_q0 Character vector of factor names for Q0.
#' @param factors_alpha Character vector of factor names for alpha.
#' @param factor_interaction Logical.
#' @param continuous_covariates Character vector.
#'
#' @return A list with X_q0, X_alpha design matrices and formula strings.
#' @keywords internal
.tmb_build_design_matrices <- function(
  data,
  factors_q0 = NULL,
  factors_alpha = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL
) {
  rhs_q0 <- build_fixed_rhs(
    factors = factors_q0,
    factor_interaction = factor_interaction,
    continuous_covariates = continuous_covariates,
    data = data
  )
  rhs_alpha <- build_fixed_rhs(
    factors = factors_alpha,
    factor_interaction = factor_interaction,
    continuous_covariates = continuous_covariates,
    data = data
  )

  # Build model matrices
  X_q0 <- model.matrix(stats::as.formula(rhs_q0), data = data)
  X_alpha <- model.matrix(stats::as.formula(rhs_alpha), data = data)

  list(
    X_q0 = X_q0,
    X_alpha = X_alpha,
    rhs_q0 = rhs_q0,
    rhs_alpha = rhs_alpha
  )
}


#' Build subject-level random-effects design matrices from a parsed RE spec
#'
#' Consumes the canonical block representation produced by
#' `.normalize_re_input()` (Phase 1) and emits the per-observation design
#' matrices `Z_q0` and `Z_alpha` that the Phase-2 generalized
#' `src/MixedDemand.h` template will consume.
#'
#' For each block, the helper extracts the `model.matrix()` columns
#' corresponding to that block's `terms_q0` / `terms_alpha` from `data`
#' and stacks them column-wise. Order: block 1 q0 cols, block 2 q0
#' cols, ..., then block 1 alpha cols, etc.
#'
#' Intercept-only blocks (`terms_*` is `"(Intercept)"`) produce a column
#' of 1s; factor-expanded blocks reproduce the model.matrix expansion.
#'
#' @param re_parsed Output of `.normalize_re_input()`.
#' @param data Long-format data frame the model is fit on.
#' @param id_var Subject id column (currently unused — reserved for
#'   future per-subject reductions).
#'
#' @return A list with components:
#'   \describe{
#'     \item{Z_q0}{Numeric matrix, `n_obs` rows by `re_dim_q0` columns.}
#'     \item{Z_alpha}{Numeric matrix, `n_obs` rows by `re_dim_alpha` columns.}
#'     \item{re_dim_q0}{Integer; total Q0 RE columns.}
#'     \item{re_dim_alpha}{Integer; total alpha RE columns.}
#'   }
#'
#' @keywords internal
.tmb_build_z_matrices <- function(re_parsed, data, id_var = "id") {
  n_obs <- nrow(data)
  z_q0_cols <- list()
  z_alpha_cols <- list()

  for (b in re_parsed$blocks) {
    if (length(b$terms_q0) > 0L) {
      z_q0_cols[[length(z_q0_cols) + 1L]] <-
        .tmb_block_design_columns(b, data, parameter = "q0")
    }
    if (length(b$terms_alpha) > 0L) {
      z_alpha_cols[[length(z_alpha_cols) + 1L]] <-
        .tmb_block_design_columns(b, data, parameter = "alpha")
    }
  }

  Z_q0 <- if (length(z_q0_cols) > 0L) {
    do.call(cbind, z_q0_cols)
  } else {
    matrix(numeric(0), nrow = n_obs, ncol = 0L)
  }
  Z_alpha <- if (length(z_alpha_cols) > 0L) {
    do.call(cbind, z_alpha_cols)
  } else {
    matrix(numeric(0), nrow = n_obs, ncol = 0L)
  }

  list(
    Z_q0 = Z_q0,
    Z_alpha = Z_alpha,
    re_dim_q0 = ncol(Z_q0),
    re_dim_alpha = ncol(Z_alpha)
  )
}

# Build the design columns for one (block, parameter) pair. The block's
# `terms_*` field already names the model-matrix columns the parser
# produced; we re-derive them here from the block's RHS formula so the
# column order is canonical.
.tmb_block_design_columns <- function(block, data, parameter = c("q0", "alpha")) {
  parameter <- match.arg(parameter)
  terms_target <- if (parameter == "q0") block$terms_q0 else block$terms_alpha
  if (length(terms_target) == 0L) {
    return(matrix(numeric(0), nrow = nrow(data), ncol = 0L))
  }

  # Strip the LHS so we can build a one-sided RHS formula.
  rhs_form <- stats::as.formula(
    paste("~", deparse1(block$formula[[3]]))
  )
  X <- stats::model.matrix(rhs_form, data = data)

  # Reorder to match the parser's `terms_*` ordering (defensive — they
  # should already match).
  X[, terms_target, drop = FALSE]
}


#' Build the block-structure metadata vectors for the Phase-2 TMB template
#'
#' Emits the integer vectors that tell the C++ template how to slice the
#' parameter vector into per-block covariance components and how to map
#' subject-level random effects through each block's Cholesky.
#'
#' Conventions:
#'   * `block_types[b] = 0` for pdDiag, `1` for pdSymm.
#'   * `block_q0_offset[b]` / `block_alpha_offset[b]` are 0-indexed
#'     starting columns within `Z_q0` / `Z_alpha` respectively.
#'   * `n_logsigma` is `sum(block_q0_dim + block_alpha_dim)` -- one
#'     standard-deviation parameter per RE column across all blocks.
#'   * `n_rho` is `sum(over pdSymm blocks: block_dim*(block_dim-1)/2)`
#'     where `block_dim = block_q0_dim + block_alpha_dim`. pdDiag
#'     blocks contribute 0 free correlations.
#'
#' @param re_parsed Output of `.normalize_re_input()`.
#'
#' @return A list of integer scalars and integer vectors describing the
#'   block structure (see Conventions above).
#'
#' @keywords internal
.tmb_build_block_map <- function(re_parsed) {
  blocks <- re_parsed$blocks
  n_blocks <- length(blocks)

  block_q0_dim <- vapply(blocks, function(b) length(b$terms_q0), integer(1))
  block_alpha_dim <- vapply(blocks, function(b) length(b$terms_alpha), integer(1))
  block_types <- vapply(
    blocks,
    function(b) if (b$pdmat_class == "pdSymm") 1L else 0L,
    integer(1)
  )

  block_q0_offset <- if (n_blocks == 0L) integer(0) else
    c(0L, cumsum(block_q0_dim)[-n_blocks])
  block_alpha_offset <- if (n_blocks == 0L) integer(0) else
    c(0L, cumsum(block_alpha_dim)[-n_blocks])

  n_logsigma <- sum(block_q0_dim + block_alpha_dim)
  block_dims <- block_q0_dim + block_alpha_dim
  n_rho <- sum(ifelse(
    block_types == 1L,
    block_dims * (block_dims - 1L) / 2L,
    0L
  ))

  list(
    n_blocks = as.integer(n_blocks),
    block_q0_dim = as.integer(block_q0_dim),
    block_alpha_dim = as.integer(block_alpha_dim),
    block_types = as.integer(block_types),
    block_q0_offset = as.integer(block_q0_offset),
    block_alpha_offset = as.integer(block_alpha_offset),
    n_logsigma = as.integer(n_logsigma),
    n_rho = as.integer(n_rho)
  )
}


#' Build TMB Data List
#'
#' @param prepared Output from .tmb_prepare_data().
#' @param design Output from .tmb_build_design_matrices().
#' @param equation Character string, equation type.
#' @param re_parsed Canonical RE block structure from `.normalize_re_input()`.
#' @param data The (possibly cleaned) data frame the model is fit on.
#' @param id_var Subject id column name.
#'
#' @return A list suitable for TMB::MakeADFun data argument, including the
#'   per-observation RE design matrices (`Z_q0`, `Z_alpha`) and block-map
#'   metadata vectors consumed by the Phase-2 generalized template.
#' @keywords internal
.tmb_build_tmb_data <- function(prepared, design, equation, re_parsed,
                                 data, id_var = "id") {
  eqn_type <- switch(equation,
    exponential = 0L,
    exponentiated = 1L,
    simplified = 2L,
    zben = 3L,
    stop("Unknown equation: ", equation)
  )

  # Build Z matrices on the SAME row order as `prepared` -- the prepared
  # data path may have dropped NA rows etc., so use the cleaned subset.
  z <- .tmb_build_z_matrices(re_parsed, data, id_var = id_var)
  bmap <- .tmb_build_block_map(re_parsed)

  list(
    model = "MixedDemand",
    y = prepared$y,
    price = prepared$price,
    subject_id = prepared$subject_id,
    n_subjects = prepared$n_subjects,
    X_q0 = design$X_q0,
    X_alpha = design$X_alpha,
    Z_q0 = z$Z_q0,
    Z_alpha = z$Z_alpha,
    eqn_type = eqn_type,
    n_blocks = bmap$n_blocks,
    block_q0_dim = bmap$block_q0_dim,
    block_alpha_dim = bmap$block_alpha_dim,
    block_types = bmap$block_types,
    block_q0_offset = bmap$block_q0_offset,
    block_alpha_offset = bmap$block_alpha_offset
  )
}


#' Generate Default Starting Values
#'
#' @param prepared Output from .tmb_prepare_data().
#' @param design Output from .tmb_build_design_matrices().
#' @param equation Character string.
#' @param re_parsed Canonical RE block structure.
#' @param has_k Logical.
#' @param k_fixed Numeric or NULL.
#'
#' @return Named list of starting values shaped for the Phase-2 template:
#'   `logsigma` is a vector of length `sum(block dims)`, `rho_raw` is a
#'   vector of length `sum(over pdSymm: d*(d-1)/2)`, and `u` has columns
#'   ordered \[block1_q0, block1_alpha, block2_q0, ...\].
#' @keywords internal
.tmb_default_starts <- function(prepared, design, equation, re_parsed, has_k,
                                 k_fixed = NULL) {
  y <- prepared$y
  price <- prepared$price
  n_subjects <- prepared$n_subjects
  p_q0 <- ncol(design$X_q0)
  p_alpha <- ncol(design$X_alpha)

  # Data-driven Q0 intercept
  if (equation == "exponential") {
    # y is already log(Q); use median of zero-price or lowest-price obs
    zero_price_idx <- price == 0 | price == min(price)
    q0_start <- median(y[zero_price_idx], na.rm = TRUE)
    if (is.na(q0_start) || !is.finite(q0_start)) q0_start <- log(10)
  } else if (equation == "zben") {
    # y is LL4-transformed; Q0 in log10 space for zben
    zero_price_idx <- price == 0 | price == min(price)
    q0_log10 <- median(y[zero_price_idx], na.rm = TRUE)
    if (is.na(q0_log10) || !is.finite(q0_log10)) q0_log10 <- 1
    q0_start <- log(q0_log10 * log(10))  # log(Q0) from log10(Q0)
  } else {
    # exponentiated/simplified: raw Q
    zero_price_idx <- price == 0 | price == min(price)
    q0_raw <- median(y[zero_price_idx], na.rm = TRUE)
    if (is.na(q0_raw) || q0_raw <= 0) q0_raw <- 10
    q0_start <- log(q0_raw)
  }

  # beta_q0: intercept = q0_start, rest = 0
  beta_q0 <- rep(0, p_q0)
  beta_q0[1] <- q0_start

  # beta_alpha: intercept = log(0.001), rest = 0
  beta_alpha <- rep(0, p_alpha)
  beta_alpha[1] <- log(0.001)

  bmap <- .tmb_build_block_map(re_parsed)
  re_dim_total <- sum(bmap$block_q0_dim) + sum(bmap$block_alpha_dim)

  starts <- list(
    beta_q0 = beta_q0,
    beta_alpha = beta_alpha,
    log_k = if (has_k && !is.null(k_fixed)) log(k_fixed) else log(2),
    logsigma = rep(log(0.5), bmap$n_logsigma),
    logsigma_e = log(1),
    rho_raw = rep(0, bmap$n_rho),
    u = matrix(0, nrow = n_subjects, ncol = re_dim_total)
  )

  starts
}


#' Build TMB Map List
#'
#' Phase 2 expresses pdDiag-vs-pdSymm via the dimensionality of `rho_raw`
#' (zero-length for pdDiag blocks, length d*(d-1)/2 for pdSymm blocks of
#' size d). The map argument therefore only needs to handle `log_k`
#' (mapped out for simplified / zben).
#'
#' @param has_k Logical, whether k is estimated.
#'
#' @return Named list for TMB map argument.
#' @keywords internal
.tmb_build_map <- function(has_k) {
  map <- list()
  if (!has_k) {
    map$log_k <- factor(NA)
  }
  map
}


#' Expand Partial Bounds to Full Parameter Vector
#'
#' @param bounds Named numeric vector of user-specified bounds (possibly partial),
#'   or NULL.
#' @param par_names Character vector of optimizer parameter names (from
#'   `names(obj$par)`). May contain repeated names for vector parameters.
#' @param default_val Default bound value: `-Inf` for lower, `Inf` for upper.
#'
#' @return Numeric vector of length `length(par_names)` with user bounds applied
#'   to all matching positions and `default_val` elsewhere.
#' @keywords internal
.expand_bounds <- function(bounds, par_names, default_val) {
  if (is.null(bounds)) return(rep(default_val, length(par_names)))
  result <- rep(default_val, length(par_names))
  names(result) <- par_names
  for (nm in names(bounds)) {
    idx <- which(par_names == nm)
    if (length(idx) > 0) {
      result[idx] <- bounds[nm]
    } else {
      warning("Bounds specified for unknown parameter '", nm, "' (ignored)",
              call. = FALSE)
    }
  }
  result
}


#' Run a Single TMB Optimization
#'
#' Dispatches to either `nlminb` or `optim(method = "L-BFGS-B")` and
#' normalizes the return value so that downstream code always sees the same
#' field names regardless of optimizer.
#'
#' @param obj TMB objective function object (from `TMB::MakeADFun`).
#' @param start Named numeric vector of starting parameter values.
#' @param tmb_control Control list (merged defaults + user overrides).
#' @param user_specified Character vector of field names the user explicitly
#'   provided in `tmb_control`.
#' @param verbose Integer verbosity level.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`opt`}{Named list with `$par`, `$objective`, `$convergence`,
#'       `$message` -- guaranteed non-NULL character for `$message`.}
#'     \item{`warnings`}{Character vector of optimizer warnings captured during
#'       the run.}
#'   }
#' @keywords internal
.tmb_run_optimizer <- function(obj, start, tmb_control, user_specified, verbose) {
  optimizer <- tmb_control$optimizer
  iter_max <- tmb_control$iter_max
  eval_max <- tmb_control$eval_max
  rel_tol <- tmb_control$rel_tol

  # Effective trace: user-specified takes precedence

  trace <- if ("trace" %in% user_specified) {
    as.integer(tmb_control$trace)
  } else if (verbose >= 2) {
    1L
  } else {
    0L
  }

  # Expand bounds
  par_names <- names(start)
  lower <- .expand_bounds(tmb_control$lower, par_names, -Inf)
  upper <- .expand_bounds(tmb_control$upper, par_names, Inf)

  opt_warnings <- character(0)

  if (optimizer == "nlminb") {
    opt <- tryCatch(
      withCallingHandlers(
        nlminb(
          start = start,
          objective = obj$fn,
          gradient = obj$gr,
          lower = lower,
          upper = upper,
          control = list(
            eval.max = eval_max,
            iter.max = iter_max,
            rel.tol = rel_tol,
            trace = trace
          )
        ),
        warning = function(w) {
          opt_warnings <<- c(opt_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        list(
          par = start,
          objective = Inf,
          convergence = 99L,
          message = conditionMessage(e)
        )
      }
    )
  } else {
    # L-BFGS-B via stats::optim
    opt <- tryCatch(
      {
        raw <- withCallingHandlers(
          stats::optim(
            par = start,
            fn = obj$fn,
            gr = obj$gr,
            method = "L-BFGS-B",
            lower = lower,
            upper = upper,
            control = list(
              maxit = iter_max,
              trace = trace
            )
          ),
          warning = function(w) {
            opt_warnings <<- c(opt_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        # Normalize to nlminb field names
        list(
          par = raw$par,
          objective = raw$value,
          convergence = raw$convergence,
          message = raw$message %||% "maximum iterations reached"
        )
      },
      error = function(e) {
        list(
          par = start,
          objective = Inf,
          convergence = 99L,
          message = conditionMessage(e)
        )
      }
    )
  }

  # Guarantee $message is non-NULL character
  if (is.null(opt$message)) opt$message <- "unknown"

  list(opt = opt, warnings = opt_warnings)
}


#' Multi-Start TMB Optimization
#'
#' @param tmb_data TMB data list.
#' @param start_values Default starting values list.
#' @param map TMB map list.
#' @param tmb_control Control parameters.
#' @param user_specified Character vector of user-specified tmb_control fields.
#' @param verbose Integer verbosity level.
#' @param prepared Output from .tmb_prepare_data() for data-driven start offsets.
#'
#' @return List with obj, opt, start_used.
#' @keywords internal
.tmb_multi_start <- function(tmb_data, start_values, map,
                              tmb_control, user_specified, verbose,
                              prepared = NULL) {
  # Derive data-adaptive offset scales
  q0_offset <- 1
  sigma_b_high <- log(1)
  sigma_b_low <- log(0.3)
  sigma_e_high <- log(0.5)
  sigma_e_low <- log(2)
  alpha_aggressive <- log(0.0001)
  alpha_conservative <- log(0.01)

  if (!is.null(prepared)) {
    y <- prepared$y
    price <- prepared$price
    sid <- prepared$subject_id

    # Between-subject SD of y at lowest price (for Q0 offset)
    min_p <- min(price, na.rm = TRUE)
    y_at_min <- y[price == min_p]
    if (length(y_at_min) > 2) {
      q0_offset <- max(sd(y_at_min, na.rm = TRUE), 0.5)
    }

    # Between/within subject variance (for sigma_b/sigma_e)
    subj_means <- tapply(y, sid, mean, na.rm = TRUE)
    if (length(subj_means) > 1) {
      sigma_b_high <- log(max(sd(subj_means, na.rm = TRUE), 0.1))
      sigma_b_low <- log(max(sd(subj_means, na.rm = TRUE) * 0.3, 0.05))
    }
    resids <- y - subj_means[as.character(sid)]
    within_sd <- sd(resids, na.rm = TRUE)
    if (!is.na(within_sd) && within_sd > 0) {
      sigma_e_high <- log(max(within_sd * 0.5, 0.1))
      sigma_e_low <- log(max(within_sd * 2, 0.5))
    }

    # Data-driven alpha from half-life price
    q_max <- median(y_at_min, na.rm = TRUE)
    if (!is.na(q_max) && q_max > 0) {
      half_idx <- y < (q_max / 2) & price > min_p
      if (any(half_idx, na.rm = TRUE)) {
        p_half <- min(price[half_idx], na.rm = TRUE)
        if (is.finite(p_half) && p_half > 0) {
          alpha_est <- log(2) / (exp(start_values$beta_q0[1]) * p_half)
          alpha_aggressive <- log(max(alpha_est * 0.1, 1e-8))
          alpha_conservative <- log(max(alpha_est * 10, 1e-6))
        }
      }
    }
  }

  # Generate 3 starting value sets
  start_sets <- list()
  start_sets[[1]] <- start_values  # Data-driven

  # Aggressive starts (higher Q0, lower alpha)
  s2 <- start_values
  s2$beta_q0[1] <- start_values$beta_q0[1] + q0_offset
  s2$beta_alpha[1] <- alpha_aggressive
  if (!is.null(map$log_k) && is.na(map$log_k)) {
    # k is fixed, don't change
  } else {
    s2$log_k <- log(3)
  }
  if (length(s2$logsigma) > 0L) {
    s2$logsigma <- rep_len(sigma_b_high, length(s2$logsigma))
  }
  s2$logsigma_e <- sigma_e_high
  start_sets[[2]] <- s2

  # Conservative starts (lower Q0, higher alpha)
  s3 <- start_values
  s3$beta_q0[1] <- start_values$beta_q0[1] - q0_offset * 0.5
  s3$beta_alpha[1] <- alpha_conservative
  if (!is.null(map$log_k) && is.na(map$log_k)) {
    # k is fixed
  } else {
    s3$log_k <- log(1.5)
  }
  if (length(s3$logsigma) > 0L) {
    s3$logsigma <- rep_len(sigma_b_low, length(s3$logsigma))
  }
  s3$logsigma_e <- sigma_e_low
  start_sets[[3]] <- s3

  best_nll <- Inf
  best_result <- NULL

  for (s in seq_along(start_sets)) {
    starts_i <- start_sets[[s]]
    opt_warnings_i <- character(0)

    result <- tryCatch({
      obj_i <- TMB::MakeADFun(
        data = tmb_data,
        parameters = starts_i,
        map = map,
        random = "u",
        DLL = "beezdemand",
        silent = verbose < 2
      )

      opt_result_i <- .tmb_run_optimizer(
        obj_i, obj_i$par, tmb_control, user_specified, verbose
      )
      opt_i <- opt_result_i$opt
      opt_warnings_i <- opt_result_i$warnings

      list(obj = obj_i, opt = opt_i, nll = opt_i$objective, start_idx = s,
           opt_warnings = opt_warnings_i)
    }, error = function(e) {
      if (verbose >= 2) {
        message(sprintf("  Start set %d failed: %s", s, e$message))
      }
      NULL
    })

    if (!is.null(result) && is.finite(result$nll) && result$nll < best_nll) {
      best_nll <- result$nll
      best_result <- result
    }
  }

  if (is.null(best_result)) {
    stop(
      "All starting value sets failed. ",
      "Check data quality or try different start values.",
      call. = FALSE
    )
  }

  if (verbose >= 1) {
    message(sprintf(
      "  Multi-start: best NLL = %.2f (start set %d of %d)",
      best_nll, best_result$start_idx, length(start_sets)
    ))
  }

  best_result
}


#' Extract Estimates from TMB Fit
#'
#' @param obj TMB objective function object.
#' @param opt nlminb optimization result.
#' @param re_dim_total Integer; total RE columns per subject
#'   (`re_dim_q0 + re_dim_alpha`).
#' @param n_subjects Integer.
#' @param has_k Logical.
#' @param verbose Integer.
#'
#' @return List with coefficients, se, sdr, variance_components, u_hat.
#' @keywords internal
.tmb_extract_estimates <- function(obj, opt, re_dim_total, n_subjects, has_k, verbose) {
  # Compute sdreport
  sdr <- tryCatch(
    TMB::sdreport(obj),
    error = function(e1) {
      sdr2 <- tryCatch(
        TMB::sdreport(obj, getJointPrecision = FALSE),
        error = function(e2) NULL
      )
      if (is.null(sdr2) && verbose >= 1) {
        warning("Standard error computation failed: ", e1$message)
      }
      sdr2
    }
  )

  # Hessian positive-definiteness gate (TICKET-008). When TMB reports
  # pdHess = FALSE the inverse Hessian is unstable, so any standard errors,
  # z-statistics, p-values, and Wald CIs derived from it are unreliable. We
  # surface this immediately at fit time (matching glmmTMB's convention) and
  # propagate the status to the fit object so summary()/tidy() can flag it.
  hessian_pd <- NA
  if (!is.null(sdr)) {
    hessian_pd <- isTRUE(sdr$pdHess)
    if (!hessian_pd && verbose >= 1) {
      cli::cli_warn(c(
        "!" = "Hessian is not positive definite ({.code pdHess = FALSE}).",
        "i" = "Standard errors, p-values, and confidence intervals may be unreliable.",
        "i" = "Run {.fn check_demand_model} for detailed diagnostics.",
        "i" = "Consider simplifying the model (fewer random effects) or checking data quality."
      ))
    }
  }

  # Extract fixed effects
  par_full <- opt$par
  par_names <- names(par_full)

  # Build coefficient vector with proper names
  coefficients <- par_full
  se_vec <- rep(NA_real_, length(par_full))
  names(se_vec) <- par_names

  if (!is.null(sdr)) {
    fixed_summary <- summary(sdr, "fixed")
    # Match by name
    for (nm in par_names) {
      idx <- which(rownames(fixed_summary) == nm)
      if (length(idx) > 0) {
        se_vec[nm] <- fixed_summary[idx[1], "Std. Error"]
      }
    }

    # Handle vector parameters (beta_q0, beta_alpha)
    beta_q0_idx <- which(par_names == "beta_q0")
    if (length(beta_q0_idx) > 0) {
      fixed_beta_q0 <- fixed_summary[rownames(fixed_summary) == "beta_q0", , drop = FALSE]
      if (nrow(fixed_beta_q0) == length(beta_q0_idx)) {
        se_vec[beta_q0_idx] <- fixed_beta_q0[, "Std. Error"]
      }
    }
    beta_alpha_idx <- which(par_names == "beta_alpha")
    if (length(beta_alpha_idx) > 0) {
      fixed_beta_alpha <- fixed_summary[rownames(fixed_summary) == "beta_alpha", , drop = FALSE]
      if (nrow(fixed_beta_alpha) == length(beta_alpha_idx)) {
        se_vec[beta_alpha_idx] <- fixed_beta_alpha[, "Std. Error"]
      }
    }

    # Extract random effects
    re_summary <- summary(sdr, "random")
    u_hat <- matrix(re_summary[, "Estimate"], nrow = n_subjects, ncol = re_dim_total)
  } else {
    u_hat <- matrix(0, nrow = n_subjects, ncol = re_dim_total)
  }

  # Extract variance components from ADREPORT
  variance_components <- NULL
  if (!is.null(sdr)) {
    adr <- tryCatch(summary(sdr, "report"), error = function(e) NULL)
    if (!is.null(adr)) {
      variance_components <- adr
    }
  }

  list(
    coefficients = coefficients,
    se = se_vec,
    sdr = sdr,
    variance_components = variance_components,
    u_hat = u_hat,
    hessian_pd = hessian_pd
  )
}


#' Compute Subject-Specific Parameters
#'
#' @param coefficients Named coefficient vector.
#' @param u_hat Random effects matrix; columns ordered
#'   \[block1_q0, block1_alpha, block2_q0, ...\].
#' @param subject_levels Character vector of subject IDs.
#' @param re_parsed Canonical RE block structure.
#' @param has_k Logical.
#' @param equation Character.
#' @param price Numeric vector of prices.
#' @param subject_id Integer vector of 0-indexed subject IDs.
#' @param k_fixed Numeric or NULL.
#' @param X_q0,X_alpha Fixed-effect design matrices.
#' @param Z_q0,Z_alpha Random-effect design matrices.
#' @param validate_subject_pars Logical (default `TRUE`); see Phase 0 NA fallback.
#'
#' @note For factor-expanded RE specs (e.g. `pdDiag(Q0+alpha~condition)`),
#'   subject-level Q0 / alpha here use the first observed row of `Z_q0` /
#'   `Z_alpha` per subject -- which encodes the subject's first observed
#'   condition only. Per-(subject, condition) rows are planned for Phase 5;
#'   meanwhile use `predict()` for cell-level values.
#'
#' @return Data frame of subject-specific parameters.
#' @keywords internal
.tmb_compute_subject_pars <- function(
  coefficients, u_hat, subject_levels, re_parsed, has_k,
  equation, price, subject_id, k_fixed = NULL,
  X_q0 = NULL, X_alpha = NULL,
  Z_q0 = NULL, Z_alpha = NULL,
  validate_subject_pars = TRUE
) {
  n_subjects <- length(subject_levels)

  # Per-subject check: does any column of X_q0 or X_alpha vary within an id?
  # When it does, a single row-wise collapse (first_obs_per_subject below) has
  # no well-defined meaning, and subject-level Q0/alpha/Pmax/Omax would be
  # silently wrong. Flag the affected subjects so we can NA their derived
  # parameters at the end. Phase 2/3 of TICKET-011 replaces this with per-cell
  # rows once the template has RE slopes on within-subject factors.
  affected_subjects <- logical(n_subjects)
  offending_cols <- character(0)
  if (isTRUE(validate_subject_pars)) {
    .check_within_id <- function(mat, mat_name) {
      if (is.null(mat) || ncol(mat) == 0L) {
        return(list(affected = logical(n_subjects), cols = character(0)))
      }
      aff <- logical(n_subjects)
      cols <- character(0)
      col_names <- colnames(mat)
      if (is.null(col_names)) {
        col_names <- paste0(mat_name, "[,", seq_len(ncol(mat)), "]")
      }
      for (j in seq_len(ncol(mat))) {
        subj_split <- split(mat[, j], subject_id)
        varies <- vapply(
          subj_split,
          function(v) length(unique(v)) > 1L,
          logical(1)
        )
        if (any(varies)) {
          cols <- c(cols, col_names[j])
          aff[as.integer(names(varies)[varies]) + 1L] <- TRUE
        }
      }
      list(affected = aff, cols = cols)
    }
    q0_check <- .check_within_id(X_q0, "X_q0")
    alpha_check <- .check_within_id(X_alpha, "X_alpha")
    affected_subjects <- q0_check$affected | alpha_check$affected
    offending_cols <- unique(c(q0_check$cols, alpha_check$cols))
    if (length(offending_cols) > 0L) {
      cli::cli_warn(c(
        "Design matrix column{?s} {.val {offending_cols}} var{?ies/y} within {.field id}.",
        "i" = "Subject-level {.field Q0}, {.field alpha}, {.field Pmax}, and {.field Omax} in {.field subject_pars} are set to {.val NA} for affected subjects because a row-wise collapse has no well-defined meaning here.",
        "i" = "Use {.code validate_subject_pars = FALSE} to force row-order-dependent values (not recommended).",
        "i" = "Factor-expanded random-effects support is planned in TICKET-011 Phases 2-3."
      ))
    }
  }

  # Get beta vectors
  beta_q0_idx <- which(names(coefficients) == "beta_q0")
  beta_alpha_idx <- which(names(coefficients) == "beta_alpha")
  beta_q0 <- unname(coefficients[beta_q0_idx])
  beta_alpha <- unname(coefficients[beta_alpha_idx])

  # Per-block Cholesky reconstruction. Mirrors the C++ template loop in
  # src/MixedDemand.h: for each block, build L_b = diag(sigma) * L_corr
  # via LKJ-Cholesky from rho_raw (or just diag(sigma) for pdDiag), then
  # transform u_block to re_block per subject.
  bmap <- .tmb_build_block_map(re_parsed)
  re_dim_q0 <- sum(bmap$block_q0_dim)
  re_dim_alpha <- sum(bmap$block_alpha_dim)
  re_q0_mat <- matrix(0, nrow = n_subjects, ncol = re_dim_q0)
  re_alpha_mat <- matrix(0, nrow = n_subjects, ncol = re_dim_alpha)

  if (re_dim_q0 + re_dim_alpha > 0L && bmap$n_blocks > 0L) {
    logsigma_full <- unname(coefficients[names(coefficients) == "logsigma"])
    rho_raw_full <- unname(coefficients[names(coefficients) == "rho_raw"])

    sigma_offset <- 0L
    rho_offset <- 0L
    u_offset <- 0L

    for (b in seq_len(bmap$n_blocks)) {
      d_q0 <- bmap$block_q0_dim[b]
      d_alpha <- bmap$block_alpha_dim[b]
      d <- d_q0 + d_alpha
      if (d == 0L) next

      sigma_b <- exp(logsigma_full[(sigma_offset + 1L):(sigma_offset + d)])

      L_b <- matrix(0, nrow = d, ncol = d)
      L_b[1L, 1L] <- sigma_b[1L]

      if (bmap$block_types[b] == 1L && d > 1L) {
        L_corr <- matrix(0, nrow = d, ncol = d)
        L_corr[1L, 1L] <- 1
        for (j in 2L:d) {
          sum_sq <- 0
          for (k in seq_len(j - 1L)) {
            r <- tanh(rho_raw_full[rho_offset + 1L])
            rho_offset <- rho_offset + 1L
            if (k == 1L) {
              L_corr[j, k] <- r
            } else {
              L_corr[j, k] <- r * sqrt(max(0, 1 - sum_sq))
            }
            sum_sq <- sum_sq + L_corr[j, k]^2
          }
          L_corr[j, j] <- sqrt(max(0, 1 - sum_sq))
        }
        L_b <- diag(sigma_b, nrow = d) %*% L_corr
      } else if (d > 1L) {
        for (j in 2L:d) L_b[j, j] <- sigma_b[j]
      }

      for (i in seq_len(n_subjects)) {
        u_block <- u_hat[i, (u_offset + 1L):(u_offset + d)]
        re_block <- as.numeric(L_b %*% u_block)
        if (d_q0 > 0L) {
          re_q0_mat[i, (bmap$block_q0_offset[b] + 1L):(bmap$block_q0_offset[b] + d_q0)] <-
            re_block[seq_len(d_q0)]
        }
        if (d_alpha > 0L) {
          re_alpha_mat[i, (bmap$block_alpha_offset[b] + 1L):(bmap$block_alpha_offset[b] + d_alpha)] <-
            re_block[(d_q0 + 1L):(d_q0 + d_alpha)]
        }
      }

      sigma_offset <- sigma_offset + d
      u_offset <- u_offset + d
    }
  }

  # Subject-specific Q0 and alpha using first-observation row of X and Z
  # per subject (Phase 2 deferral; per-(subject, condition) rows are
  # planned for Phase 5 -- see @note above).
  first_obs_per_subject <- match(seq_len(n_subjects) - 1L, subject_id)

  subj_log_q0 <- numeric(n_subjects)
  subj_log_alpha <- numeric(n_subjects)

  for (i in seq_len(n_subjects)) {
    first_obs <- first_obs_per_subject[i]

    if (!is.null(X_q0)) {
      subj_log_q0[i] <- sum(X_q0[first_obs, ] * beta_q0)
    } else {
      subj_log_q0[i] <- beta_q0[1]
    }
    if (re_dim_q0 > 0L && !is.null(Z_q0)) {
      subj_log_q0[i] <- subj_log_q0[i] +
        sum(Z_q0[first_obs, ] * re_q0_mat[i, ])
    }

    if (!is.null(X_alpha)) {
      subj_log_alpha[i] <- sum(X_alpha[first_obs, ] * beta_alpha)
    } else {
      subj_log_alpha[i] <- beta_alpha[1]
    }
    if (re_dim_alpha > 0L && !is.null(Z_alpha)) {
      subj_log_alpha[i] <- subj_log_alpha[i] +
        sum(Z_alpha[first_obs, ] * re_alpha_mat[i, ])
    }
  }

  subj_Q0 <- exp(subj_log_q0)
  subj_alpha <- exp(subj_log_alpha)

  # Compute Pmax/Omax
  if (has_k) {
    if ("log_k" %in% names(coefficients)) {
      k_val <- exp(coefficients[["log_k"]])
    } else if (!is.null(k_fixed)) {
      k_val <- k_fixed
    } else {
      k_val <- 2  # fallback default
    }

    # Both exponential and exponentiated use the same Pmax formula
    model_type <- "hs"

    price_split <- split(price, subject_id)
    price_list <- lapply(seq_len(n_subjects), function(i) {
      ps <- price_split[[as.character(i - 1L)]]
      if (is.null(ps)) numeric(0) else ps
    })

    omax_pmax <- beezdemand_calc_pmax_omax_vec(
      params_df = data.frame(
        alpha = subj_alpha,
        q0 = subj_Q0,
        k = rep(k_val, n_subjects)
      ),
      model_type = model_type,
      param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
      price_list = price_list,
      compute_observed = FALSE
    )
  } else {
    # simplified/zben: no k
    price_split <- split(price, subject_id)
    price_list <- lapply(seq_len(n_subjects), function(i) {
      ps <- price_split[[as.character(i - 1L)]]
      if (is.null(ps)) numeric(0) else ps
    })

    omax_pmax <- beezdemand_calc_pmax_omax_vec(
      params_df = data.frame(
        alpha = subj_alpha,
        q0 = subj_Q0
      ),
      model_type = "snd",
      param_scales = list(alpha = "natural", q0 = "natural"),
      price_list = price_list,
      compute_observed = FALSE
    )
  }

  # NA out derived parameters for subjects flagged by the within-id check;
  # b_i / c_i remain populated because they are well-defined per subject
  # regardless of fixed-effect design-column variation within id.
  if (any(affected_subjects)) {
    subj_Q0[affected_subjects] <- NA_real_
    subj_alpha[affected_subjects] <- NA_real_
    omax_pmax$pmax_model[affected_subjects] <- NA_real_
    omax_pmax$omax_model[affected_subjects] <- NA_real_
  }

  # Build output. For backward compat, populate `b_i` (and `c_i` if alpha
  # has REs) from the first column of each per-block RE matrix. For
  # intercept-only fits this matches the Phase-1 behavior bit-for-bit.
  # For factor-expanded fits, b_i / c_i hold the FIRST RE coefficient
  # per subject (typically the intercept under treatment contrasts) --
  # downstream consumers that need the full per-condition RE vector
  # should read fit$param_info$re_q0_mat / re_alpha_mat (Phase 5).
  out <- data.frame(
    id = subject_levels,
    Q0 = subj_Q0,
    alpha = subj_alpha,
    Pmax = omax_pmax$pmax_model,
    Omax = omax_pmax$omax_model,
    stringsAsFactors = FALSE
  )

  if (re_dim_q0 >= 1L) {
    out$b_i <- re_q0_mat[, 1L]
  }
  if (re_dim_alpha >= 1L) {
    out$c_i <- re_alpha_mat[, 1L]
  }

  cols_order <- c("id")
  if ("b_i" %in% names(out)) cols_order <- c(cols_order, "b_i")
  if ("c_i" %in% names(out)) cols_order <- c(cols_order, "c_i")
  cols_order <- c(cols_order, "Q0", "alpha", "Pmax", "Omax")
  out <- out[, cols_order]

  # Attach the full per-block RE matrices as attributes for downstream
  # methods that need cell-level values (Phase 5 promotes to columns).
  attr(out, "re_q0_mat") <- re_q0_mat
  attr(out, "re_alpha_mat") <- re_alpha_mat

  out
}


# ==============================================================================
# Main Fitting Function
# ==============================================================================

#' Fit Mixed-Effects Demand Models via TMB
#'
#' @description
#' Fits nonlinear mixed-effects demand models using Template Model Builder (TMB)
#' for exact automatic differentiation and Laplace approximation. This provides
#' reliable convergence where traditional NLME approaches fail.
#'
#' @param data A data frame in long format with columns for subject ID, price,
#'   and consumption.
#' @param y_var Character. Name of the consumption/response variable.
#' @param x_var Character. Name of the price variable.
#' @param id_var Character. Name of the subject identifier variable.
#' @param equation Character. The demand equation to fit. One of:
#'   \describe{
#'     \item{`"exponentiated"`}{Koffarnus et al. (2015). Gaussian on raw Q. Zeros
#'       allowed. Has k parameter.}
#'     \item{`"exponential"`}{Hursh & Silberberg (2008). Gaussian on log(Q).
#'       Observations with Q = 0 are automatically dropped. Has k parameter.}
#'     \item{`"simplified"`}{Simplified exponential (no k). Gaussian on raw Q.
#'       Zeros allowed.}
#'     \item{`"zben"`}{Zero-bounded exponential (no k). Gaussian on LL4-
#'       transformed Q. User must pass LL4-transformed y_var. Note: Q0 on
#'       the log10 scale is clamped to a minimum of 0.001 to avoid a
#'       singularity at Q0 = 1 (where log10(Q0) = 0 causes division by zero
#'       in the decay rate). Subjects with estimated Q0 near 1 may have
#'       biased parameter estimates.}
#'   }
#' @param estimate_k Logical. If `TRUE` (default), estimate k as a free
#'   parameter. If `FALSE`, fix k at the value given in `k`. Only relevant for
#'   "exponentiated" and "exponential" equations.
#' @param k Numeric or `NULL`. Fixed value of k when `estimate_k = FALSE`.
#'   If `NULL` and `estimate_k = FALSE`, k defaults to 2.
#' @param random_effects Specification of subject-level random effects.
#'   Accepts any of the following, in order of generality:
#'   \describe{
#'     \item{formula (default)}{`Q0 + alpha ~ 1` -- random intercepts on
#'       both parameters (equivalent to the legacy `c("q0", "alpha")`
#'       shortcut). `Q0 ~ 1` limits REs to Q0. Formulas with non-intercept
#'       RHS terms (e.g., `Q0 + alpha ~ condition`) are parsed but
#'       currently rejected by the Phase-1 template -- those shapes land
#'       in Phase 2/3 of TICKET-011.}
#'     \item{`nlme::pdMat`}{e.g., `nlme::pdDiag(Q0 + alpha ~ 1)`.
#'       Pre-constructed pdMat objects are accepted and their covariance
#'       class is honored (overrides `covariance_structure`).}
#'     \item{list of `pdMat` / `nlme::pdBlocked`}{Multi-block covariance
#'       structures like `list(pdSymm(Q0+alpha~1), pdDiag(Q0+alpha~cond-1))`.
#'       Parsed, but fitting is deferred to Phase 3 of TICKET-011.}
#'     \item{character vector (deprecated)}{`c("q0", "alpha")` or `"q0"`.
#'       Soft-deprecated in 0.4.0; emits a `lifecycle::deprecate_soft()`
#'       message. Translated internally to the formula `Q0 + alpha ~ 1`
#'       or `Q0 ~ 1`.}
#'   }
#' @param covariance_structure `"pdSymm"` (default; unstructured) or
#'   `"pdDiag"` (diagonal). Applies only when `random_effects` is a
#'   formula; ignored for pre-constructed pdMat / list / pdBlocked inputs.
#' @param factors Character vector of factor variable names for group comparisons.
#' @param factor_interaction Logical. If `TRUE` and two factors provided, include
#'   their interaction.
#' @param continuous_covariates Character vector of continuous covariate names.
#' @param collapse_levels Named list for asymmetric factor collapsing. Structure:
#'   `list(Q0 = list(factor = list(new = c(old))), alpha = list(...))`.
#' @param start_values Named list of starting values. If `NULL`, data-driven
#'   defaults are used.
#' @param tmb_control List of control parameters for the optimizer:
#'   \describe{
#'     \item{`optimizer`}{Character. `"nlminb"` (default) or `"L-BFGS-B"`.
#'       L-BFGS-B can recover from nlminb convergence failures (code 1 or 8).}
#'     \item{`iter_max`}{Maximum iterations (default 1000).}
#'     \item{`eval_max`}{Maximum function evaluations (default 2000). Only
#'       applies to nlminb; L-BFGS-B has no function evaluation limit.}
#'     \item{`rel_tol`}{Relative convergence tolerance (default 1e-10). Only
#'       applies to nlminb.}
#'     \item{`lower`}{Named numeric vector of lower bounds on optimizer-scale
#'       parameters (default NULL = no bounds). Names must match optimizer
#'       parameter names (e.g., `log_k`, `beta_q0`, `logsigma_b`). Note that
#'       most parameters are in log-space: e.g., to constrain k between 0.14
#'       and 55, use `lower = c(log_k = -2)`, `upper = c(log_k = 4)`. A bound name
#'       applies to *all* occurrences of that parameter (e.g., both elements
#'       of `beta_q0`).}
#'     \item{`upper`}{Named numeric vector of upper bounds (see `lower`).}
#'     \item{`warm_start`}{Named numeric vector of starting values in
#'       optimizer space (e.g., from a previous `fit$opt$par`). When provided,
#'       `multi_start` is automatically disabled. This differs from
#'       `start_values`, which operates in parameter space before
#'       `TMB::MakeADFun()`. Length must match the number of free parameters.}
#'     \item{`trace`}{Non-negative integer controlling optimizer trace output
#'       (default 0). When not explicitly set, inherits from `verbose >= 2`.}
#'   }
#' @param multi_start Logical. If `TRUE` (default), try 3 starting value sets
#'   and select the best.
#' @param validate_subject_pars Logical. If `TRUE` (default), validate that
#'   every column of the fixed-effect design matrices is constant within
#'   each `id` before computing `subject_pars`. When a factor or continuous
#'   covariate varies within subject, Q0/alpha/Pmax/Omax are set to
#'   `NA_real_` for affected subjects and a warning names the offending
#'   columns. Set to `FALSE` to force row-order-dependent values (not
#'   recommended; proper factor-expanded RE support lands in TICKET-011
#'   Phases 2-3).
#' @param verbose Integer. Verbosity level: 0 = silent, 1 = progress, 2 = debug.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `beezdemand_tmb` containing:
#'   \describe{
#'     \item{model}{List with coefficients, se, variance_components}
#'     \item{subject_pars}{Data frame of subject-specific Q0, alpha, Pmax, Omax}
#'     \item{tmb_obj}{TMB objective function object}
#'     \item{opt}{Optimization result (normalized across optimizers)}
#'     \item{sdr}{TMB sdreport object}
#'     \item{converged}{Logical convergence indicator}
#'     \item{loglik}{Log-likelihood at convergence}
#'     \item{AIC}{Akaike Information Criterion}
#'     \item{BIC}{Bayesian Information Criterion}
#'     \item{data}{Original data (after any filtering)}
#'     \item{param_info}{List of model metadata}
#'     \item{formula_details}{Design matrix and formula information}
#'     \item{collapse_info}{Collapse levels information (if used)}
#'   }
#'
#' @details
#' Traditional NLME approaches using \code{nlme::nlme()} universally fail for
#' demand equations because the PNLS algorithm with numerical finite-difference
#' gradients cannot navigate the likelihood surface. TMB succeeds using exact
#' automatic differentiation, Laplace approximation, and joint marginal
#' likelihood optimization.
#'
#' When \code{estimate_k = TRUE}, k is estimated as a free parameter alongside
#' Q0 and alpha. This typically improves model fit substantially. The
#' conventional fixed-k approach (Hursh & Silberberg, 2008) often overestimates
#' k by 3-8x.
#'
#' **Error model considerations:** The \code{exponentiated} and
#' \code{simplified} equations use a Gaussian error model on raw consumption
#' (Q), which assigns non-zero density to negative values. For data with many
#' near-zero observations, prefer \code{exponential} (Gaussian on log Q, zeros
#' dropped) or \code{zben} (Gaussian on LL4-transformed Q, zeros handled by
#' the transformation).
#'
#' @examples
#' \donttest{
#' data(apt)
#'
#' # Exponential (HS) on log(Q)
#' fit <- fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
#'                       equation = "exponential")
#' summary(fit)
#' plot(fit)
#'
#' # Exponentiated (Koffarnus) on raw Q
#' fit2 <- fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
#'                        equation = "exponentiated")
#'
#' # With covariates
#' data(apt_full)
#' fit3 <- fit_demand_tmb(apt_full, y_var = "y", x_var = "x", id_var = "id",
#'                        equation = "exponential", factors = "gender")
#' get_demand_param_emms(fit3, param = "alpha")
#' }
#'
#' @seealso [fit_demand_mixed()] for NLME-based fitting,
#'   [fit_demand_hurdle()] for two-part hurdle models,
#'   [fit_demand_fixed()] for individual NLS curves.
#' @family demand-fitting
#'
#' @export
fit_demand_tmb <- function(
  data,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  equation = c("exponentiated", "exponential", "simplified", "zben"),
  estimate_k = TRUE,
  k = NULL,
  random_effects = Q0 + alpha ~ 1,
  covariance_structure = c("pdSymm", "pdDiag"),
  factors = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL,
  collapse_levels = NULL,
  start_values = NULL,
  tmb_control = list(iter_max = 1000, eval_max = 2000),
  multi_start = TRUE,
  validate_subject_pars = TRUE,
  verbose = 1,
  ...
) {
  cl <- match.call()
  # Normalize aliases before match.arg (only when user passed a scalar value)
  if (length(equation) == 1) {
    equation <- normalize_equation(equation, tier = "tmb")
  }
  equation <- match.arg(equation)
  covariance_structure <- match.arg(covariance_structure)

  # Normalize random_effects via the shared parser. Character input is
  # translated to an equivalent formula block and a soft-deprecation
  # message is emitted. Formula / pdMat / list / pdBlocked inputs are all
  # accepted; shapes richer than intercept-only are gated by Phase 1 and
  # will land in Phase 2 of TICKET-011.
  re_parsed <- .normalize_re_input(
    random_effects,
    covariance_structure = covariance_structure,
    data = data
  )
  if (re_parsed$source == "character") {
    .deprecate_character_re()
  }
  .validate_re_input(re_parsed, data = data, id_var = id_var)

  if (!.re_is_phase2_fittable(re_parsed)) {
    stop(
      "random_effects specification requires multi-block support ",
      "(TICKET-011 Phase 3, not yet shipped). Supported today: a single ",
      "pdDiag or pdSymm block on Q0 and/or alpha (intercept-only or ",
      "factor-expanded). Use `fit_demand_mixed()` for `pdBlocked` / ",
      "`list()` multi-block structures in the meantime.",
      call. = FALSE
    )
  }

  # Phase-2 RE dimensions derived from the canonical block representation.
  bmap_preview <- .tmb_build_block_map(re_parsed)
  re_dim_total <- sum(bmap_preview$block_q0_dim) +
                  sum(bmap_preview$block_alpha_dim)

  # Determine if k is used
  has_k <- equation %in% c("exponentiated", "exponential")
  if (has_k && !estimate_k) {
    if (is.null(k)) k <- 2
    if (!is.numeric(k) || length(k) != 1 || k <= 0) {
      stop("`k` must be a positive number.", call. = FALSE)
    }
  }

  # Validate and prepare data
  data <- validate_demand_data(
    data = data,
    y_var = y_var,
    x_var = x_var,
    id_var = id_var,
    factors = factors
  )

  # Handle collapse_levels
  collapse_info <- NULL
  factors_q0 <- factors
  factors_alpha <- factors

  if (!is.null(collapse_levels)) {
    validate_collapse_levels(collapse_levels)

    if ("Q0" %in% names(collapse_levels) && !is.null(collapse_levels$Q0)) {
      cl_q0 <- collapse_factor_levels(
        data, collapse_levels$Q0, factors, suffix = "Q0"
      )
      data <- cl_q0$data
      factors_q0 <- cl_q0$new_factor_names
      collapse_info$Q0 <- cl_q0$info
    }

    if ("alpha" %in% names(collapse_levels) && !is.null(collapse_levels$alpha)) {
      cl_alpha <- collapse_factor_levels(
        data, collapse_levels$alpha, factors, suffix = "alpha"
      )
      data <- cl_alpha$data
      factors_alpha <- cl_alpha$new_factor_names
      collapse_info$alpha <- cl_alpha$info
    }
  }

  # Validate continuous covariates
  if (!is.null(continuous_covariates)) {
    missing_cov <- setdiff(continuous_covariates, names(data))
    if (length(missing_cov) > 0) {
      stop("Continuous covariates not found in data: ",
           paste(missing_cov, collapse = ", "), call. = FALSE)
    }
  }

  # Drop rows with NAs in any modeling column (mirrors fit_demand_mixed)
  model_cols <- unique(c(id_var, x_var, y_var,
                         factors_q0, factors_alpha, continuous_covariates))
  model_cols <- intersect(model_cols, names(data))
  complete_mask <- stats::complete.cases(data[, model_cols, drop = FALSE])
  n_dropped_na <- sum(!complete_mask)
  if (n_dropped_na > 0) {
    ids_affected <- length(unique(data[[id_var]][!complete_mask]))
    if (verbose >= 1) {
      cli::cli_inform(c(
        "i" = "Removed {n_dropped_na} row{?s} with missing values in modeling columns ({ids_affected} subject{?s} affected)."
      ))
    }
    data <- data[complete_mask, , drop = FALSE]
    if (is.factor(data[[id_var]])) data[[id_var]] <- droplevels(data[[id_var]])
    for (f in unique(c(factors_q0, factors_alpha))) {
      if (!is.null(f) && nzchar(f) && f %in% names(data) && is.factor(data[[f]])) {
        data[[f]] <- droplevels(data[[f]])
      }
    }
  }
  if (nrow(data) == 0) {
    cli::cli_abort("No complete cases remain after removing rows with missing values.")
  }

  # Prepare data
  if (verbose >= 1) {
    message(sprintf("Fitting TMB mixed-effects demand model..."))
    message(sprintf("  Equation: %s", equation))
  }

  prepared <- .tmb_prepare_data(data, y_var, x_var, id_var, equation)

  # Build design matrices (using the data after any filtering)
  # For exponential equation, we need the filtered data
  if (equation == "exponential") {
    # Rebuild data for positive observations only
    pos_idx <- data[[y_var]] > 0
    data_for_design <- data[pos_idx, , drop = FALSE]
  } else {
    data_for_design <- data
  }

  design <- .tmb_build_design_matrices(
    data = data_for_design,
    factors_q0 = factors_q0,
    factors_alpha = factors_alpha,
    factor_interaction = factor_interaction,
    continuous_covariates = continuous_covariates
  )

  if (verbose >= 1) {
    message(sprintf(
      "  Subjects: %d, Observations: %d",
      prepared$n_subjects, prepared$n_obs
    ))
    message(sprintf(
      "  Random effects: %d total RE columns per subject (%s)",
      re_dim_total, .re_shape_summary(re_parsed)
    ))
    message(sprintf(
      "  Design matrices: X_q0 [%d x %d], X_alpha [%d x %d]",
      nrow(design$X_q0), ncol(design$X_q0),
      nrow(design$X_alpha), ncol(design$X_alpha)
    ))
  }

  # Build TMB data (Phase-2 generalized: includes Z matrices and block
  # metadata). Use data_for_design so Z's row order matches the prepared
  # subset (e.g., zero rows dropped for the exponential equation).
  tmb_data <- .tmb_build_tmb_data(
    prepared, design, equation, re_parsed,
    data = data_for_design, id_var = id_var
  )

  # Build map. With the Phase-2 vectorized parameterization, pdDiag-vs-
  # pdSymm is expressed via the dimensionality of `rho_raw` (zero-length
  # for pdDiag), so the map only needs to handle log_k.
  map <- .tmb_build_map(has_k = has_k && estimate_k)

  # Default starting values
  default_starts <- .tmb_default_starts(
    prepared, design, equation, re_parsed,
    has_k = has_k && estimate_k,
    k_fixed = if (has_k && !estimate_k) k else NULL
  )

  # Merge user start values
  if (!is.null(start_values)) {
    for (nm in names(start_values)) {
      if (nm %in% names(default_starts)) {
        default_starts[[nm]] <- start_values[[nm]]
      }
    }
  }

  # Fix k if not estimated
  if (has_k && !estimate_k) {
    default_starts$log_k <- log(k)
  }

  # Fill in default tmb_control values
  default_control <- list(
    iter_max   = 1000,
    eval_max   = 2000,
    optimizer  = "nlminb",
    rel_tol    = 1e-10,
    lower      = NULL,
    upper      = NULL,
    warm_start = NULL,
    trace      = 0
  )
  user_specified <- names(tmb_control)
  tmb_control <- modifyList(default_control, tmb_control)


  # --- Input validation ---
  valid_optimizers <- c("nlminb", "L-BFGS-B")
  if (!tmb_control$optimizer %in% valid_optimizers) {
    stop(sprintf("tmb_control$optimizer must be one of: %s (got '%s')",
                 paste(valid_optimizers, collapse = ", "),
                 tmb_control$optimizer), call. = FALSE)
  }

  if (!is.numeric(tmb_control$rel_tol) || length(tmb_control$rel_tol) != 1 ||
      !is.finite(tmb_control$rel_tol) || tmb_control$rel_tol <= 0) {
    stop("tmb_control$rel_tol must be a single positive finite number",
         call. = FALSE)
  }

  if (!is.numeric(tmb_control$trace) || length(tmb_control$trace) != 1 ||
      tmb_control$trace < 0) {
    stop("tmb_control$trace must be a single non-negative number",
         call. = FALSE)
  }

  if (!is.null(tmb_control$lower)) {
    if (!is.numeric(tmb_control$lower))
      stop("tmb_control$lower must be a named numeric vector", call. = FALSE)
    if (is.null(names(tmb_control$lower)))
      stop("tmb_control$lower must be a named numeric vector", call. = FALSE)
  }
  if (!is.null(tmb_control$upper)) {
    if (!is.numeric(tmb_control$upper))
      stop("tmb_control$upper must be a named numeric vector", call. = FALSE)
    if (is.null(names(tmb_control$upper)))
      stop("tmb_control$upper must be a named numeric vector", call. = FALSE)
  }

  if (!is.null(tmb_control$warm_start) && !is.numeric(tmb_control$warm_start)) {
    stop("tmb_control$warm_start must be a numeric vector", call. = FALSE)
  }

  # Warn about rel_tol + L-BFGS-B only when user explicitly provided rel_tol
  if (tmb_control$optimizer == "L-BFGS-B" && "rel_tol" %in% user_specified) {
    warning("rel_tol is ignored by L-BFGS-B optimizer; it only applies to nlminb",
            call. = FALSE)
  }

  # warm_start overrides multi_start
  if (!is.null(tmb_control$warm_start) && isTRUE(multi_start)) {
    message("multi_start disabled when warm_start is provided")
    multi_start <- FALSE
  }

  # Fit model
  if (verbose >= 1) message("  Optimizing...")

  if (isTRUE(multi_start)) {
    result <- .tmb_multi_start(
      tmb_data, default_starts, map, tmb_control, user_specified, verbose,
      prepared = prepared
    )
    obj <- result$obj
    opt <- result$opt
  } else {
    obj <- TMB::MakeADFun(
      data = tmb_data,
      parameters = default_starts,
      map = map,
      random = "u",
      DLL = "beezdemand",
      silent = verbose < 2
    )

    # Apply warm_start after MakeADFun (replaces optimizer starting point)
    if (!is.null(tmb_control$warm_start)) {
      ws <- tmb_control$warm_start
      if (length(ws) != length(obj$par))
        stop(sprintf(
          "warm_start has %d elements but model expects %d free parameters",
          length(ws), length(obj$par)), call. = FALSE)
      if (any(!is.finite(ws)))
        stop("warm_start contains non-finite values", call. = FALSE)
      if (!is.null(names(ws)) && !identical(names(ws), names(obj$par))) {
        warning("warm_start names don't match model parameters; using positional matching",
                call. = FALSE)
      }
      obj$par[] <- ws
    }

    opt_result <- .tmb_run_optimizer(
      obj, obj$par, tmb_control, user_specified, verbose
    )
    opt <- opt_result$opt
    opt_warnings <- opt_result$warnings
  }

  # Collect optimization warnings
  if (isTRUE(multi_start)) {
    opt_warnings <- result$opt_warnings %||% character(0)
  }
  if (length(opt_warnings) > 0 && verbose >= 1) {
    unique_warnings <- unique(opt_warnings)
    for (w in unique_warnings) {
      message("  Optimizer warning: ", w)
    }
  }

  converged <- opt$convergence == 0
  try(obj$fn(opt$par), silent = TRUE)

  if (verbose >= 1) {
    if (converged) {
      message(sprintf("  Converged (NLL = %.2f)", opt$objective))
    } else {
      message(sprintf(
        "  WARNING: Did not converge (code %d: %s)",
        opt$convergence, opt$message
      ))
    }
  }

  # Extract estimates
  if (verbose >= 1) message("  Computing standard errors...")
  estimates <- .tmb_extract_estimates(
    obj, opt, re_dim_total, prepared$n_subjects,
    has_k = has_k && estimate_k, verbose = verbose
  )

  # Compute subject-specific parameters
  subject_pars <- .tmb_compute_subject_pars(
    coefficients = estimates$coefficients,
    u_hat = estimates$u_hat,
    subject_levels = prepared$subject_levels,
    re_parsed = re_parsed,
    has_k = has_k,
    equation = equation,
    price = prepared$price,
    subject_id = prepared$subject_id,
    k_fixed = if (has_k && !estimate_k) k else NULL,
    X_q0 = design$X_q0,
    X_alpha = design$X_alpha,
    Z_q0 = tmb_data$Z_q0,
    Z_alpha = tmb_data$Z_alpha,
    validate_subject_pars = validate_subject_pars
  )

  # Compute log-likelihood, AIC, BIC
  nll <- opt$objective
  loglik <- -nll
  n_fixed_params <- length(opt$par)
  n_obs <- prepared$n_obs
  aic <- 2 * nll + 2 * n_fixed_params
  bic <- 2 * nll + n_fixed_params * log(n_obs)

  # Build return object
  result <- structure(
    list(
      call = cl,
      model = list(
        coefficients = estimates$coefficients,
        se = estimates$se,
        variance_components = estimates$variance_components
      ),
      random_effects = random_effects,
      opt_warnings = opt_warnings,
      se_available = !is.null(estimates$sdr),
      subject_pars = subject_pars,
      tmb_obj = obj,
      opt = opt,
      sdr = estimates$sdr,
      hessian_pd = estimates$hessian_pd,
      converged = converged,
      loglik = loglik,
      AIC = aic,
      BIC = bic,
      data = if (equation == "exponential") data_for_design else data,
      data_all = data,
      param_info = list(
        equation = equation,
        n_subjects = prepared$n_subjects,
        n_obs = prepared$n_obs,
        n_zeros = prepared$n_zeros,
        n_dropped = prepared$n_dropped,
        n_random_effects = re_dim_total,
        random_effects_spec = random_effects,
        random_effects_parsed = re_parsed,
        random_effects_shape = .re_shape_summary(re_parsed),
        covariance_structure = covariance_structure,
        has_k = has_k,
        estimate_k = estimate_k,
        k_fixed = if (has_k && !estimate_k) k else NULL,
        y_var = y_var,
        x_var = x_var,
        id_var = id_var,
        factors = factors,
        factors_q0 = factors_q0,
        factors_alpha = factors_alpha,
        factor_interaction = factor_interaction,
        continuous_covariates = continuous_covariates,
        subject_levels = prepared$subject_levels
      ),
      formula_details = list(
        X_q0 = design$X_q0,
        X_alpha = design$X_alpha,
        rhs_q0 = design$rhs_q0,
        rhs_alpha = design$rhs_alpha
      ),
      collapse_info = collapse_info
    ),
    class = "beezdemand_tmb"
  )

  # Warn if zben Q0 estimates are near the clamping boundary
 if (equation == "zben" && !is.null(subject_pars)) {
    q0_col <- intersect(c("Q0", "q0"), names(subject_pars))
    if (length(q0_col) > 0) {
      q0_vals <- subject_pars[[q0_col[1]]]
      n_near_boundary <- sum(q0_vals < 1.01, na.rm = TRUE)
      if (n_near_boundary > 0) {
        cli::cli_warn(c(
          "Estimated Q0 for {n_near_boundary} subject{?s} is near 1.0, where the zben equation has a mathematical singularity.",
          "i" = "When Q0 < 1, the zben decay rate flips sign (demand increases with price). Values are clamped to Q0 >= ~1.002.",
          "i" = "Consider using {.arg equation = \"exponential\"} or {.arg equation = \"exponentiated\"} for low-intensity demand."
        ))
      }
    }
  }

  if (verbose >= 1) message("Done.")

  result
}
