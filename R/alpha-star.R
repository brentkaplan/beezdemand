.calc_alpha_star <- function(params, param_scales, vcov = NULL, base = c("e", "10")) {
  base <- match.arg(base)
  b <- if (identical(base, "e")) exp(1) else 10
  c_const <- log(b)

  params <- as.list(params %||% list())
  param_scales <- as.list(param_scales %||% list())

  pick_name <- function(candidates) {
    candidates <- as.character(candidates)
    hit <- candidates[candidates %in% names(params)]
    if (length(hit)) return(hit[[1]])
    NA_character_
  }

  alpha_name <- pick_name(c("alpha", "Alpha", "log_alpha", "logAlpha", "log10_alpha", "log10Alpha"))
  k_name <- pick_name(c("k", "K", "log_k", "logK", "log10_k", "log10K"))

  if (is.na(alpha_name) || is.na(k_name)) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      note = "alpha_star undefined: missing alpha and/or k"
    ))
  }

  alpha_theta <- as.numeric(params[[alpha_name]])
  k_theta <- as.numeric(params[[k_name]])

  infer_scale <- function(param_name) {
    scale_in <- param_scales[[param_name]]
    if (!is.null(scale_in)) return(as.character(scale_in))
    if (grepl("^log10", param_name)) return("log10")
    if (grepl("^log_", param_name) || grepl("^log[A-Z]", param_name) || identical(param_name, "log")) return("log")
    "natural"
  }

  alpha_scale <- infer_scale(alpha_name)
  k_scale <- infer_scale(k_name)

  to_nat_and_jac <- function(theta, scale) {
    if (is.na(theta)) {
      return(list(value = NA_real_, d_nat_d_theta = NA_real_))
    }
    if (identical(scale, "natural")) {
      return(list(value = theta, d_nat_d_theta = 1))
    }
    if (identical(scale, "log")) {
      val <- exp(theta)
      return(list(value = val, d_nat_d_theta = val))
    }
    if (identical(scale, "log10")) {
      val <- 10^theta
      return(list(value = val, d_nat_d_theta = val * log(10)))
    }
    stop("Unsupported parameter scale: ", scale, call. = FALSE)
  }

  alpha_nat_res <- to_nat_and_jac(alpha_theta, alpha_scale)
  k_nat_res <- to_nat_and_jac(k_theta, k_scale)

  alpha_nat <- alpha_nat_res$value
  k_nat <- k_nat_res$value

  if (!is.finite(alpha_nat) || !is.finite(k_nat) || alpha_nat <= 0 || k_nat <= 0) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      note = "alpha_star undefined: alpha and k must be finite and positive"
    ))
  }

  if (!(k_nat * c_const > 1)) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      note = sprintf(
        "alpha_star undefined: k * ln(base) must be > 1 (k=%.6g, ln(base)=%.6g)",
        k_nat, c_const
      )
    ))
  }

  L_k <- log(1 - 1 / (k_nat * c_const))
  if (!is.finite(L_k) || L_k == 0) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      note = "alpha_star undefined: invalid log term in denominator"
    ))
  }

  alpha_star <- -alpha_nat / L_k

  # Natural-scale partial derivatives
  d_as_d_alpha <- -1 / L_k
  d_as_d_k <- alpha_nat / (k_nat * (c_const * k_nat - 1) * (L_k^2))

  # Chain rule to estimation parameters
  g_alpha <- d_as_d_alpha * alpha_nat_res$d_nat_d_theta
  g_k <- d_as_d_k * k_nat_res$d_nat_d_theta
  grad <- c(g_alpha, g_k)
  names(grad) <- c(alpha_name, k_name)

  alpha_star_se <- NA_real_
  note <- NULL

  if (!is.null(vcov)) {
    Sigma <- NULL

    if (is.numeric(vcov) && is.null(dim(vcov))) {
      se_vec <- as.numeric(vcov)
      if (is.null(names(vcov))) {
        if (length(se_vec) == 1) {
          names(se_vec) <- alpha_name
        } else if (length(se_vec) == 2) {
          names(se_vec) <- c(alpha_name, k_name)
        } else {
          stop("Unnamed SE vector must have length 1 or 2.", call. = FALSE)
        }
      } else {
        names(se_vec) <- names(vcov)
      }

      Sigma <- diag(se_vec^2)
      dimnames(Sigma) <- list(names(se_vec), names(se_vec))
      note <- c(note, "Covariance assumed 0 (SE vector provided)")
    } else if (is.matrix(vcov)) {
      Sigma <- vcov
      if (is.null(colnames(Sigma)) || is.null(rownames(Sigma))) {
        Sigma <- as.matrix(Sigma)
        if (nrow(Sigma) == length(grad) && ncol(Sigma) == length(grad)) {
          dimnames(Sigma) <- list(names(grad), names(grad))
        } else {
          stop("vcov matrix must have dimnames for alpha/k parameters.", call. = FALSE)
        }
      }
    } else {
      stop("'vcov' must be a matrix or a named numeric vector of SEs.", call. = FALSE)
    }

    common <- intersect(names(grad), colnames(Sigma))
    if (length(common) == 0) {
      note <- c(note, "vcov missing alpha/k; SE unavailable")
    } else {
      g <- grad[common]
      Sigma_sub <- Sigma[common, common, drop = FALSE]
      var_as <- as.numeric(t(g) %*% Sigma_sub %*% g)
      if (is.finite(var_as) && var_as >= 0) {
        alpha_star_se <- sqrt(var_as)
      } else {
        note <- c(note, "Delta-method variance was not finite/positive; SE unavailable")
      }
    }
  }

  list(
    estimate = as.numeric(alpha_star),
    se = as.numeric(alpha_star_se),
    note = if (length(note)) paste(note, collapse = "; ") else NULL
  )
}
