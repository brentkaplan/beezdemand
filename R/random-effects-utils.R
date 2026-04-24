# Internal helpers for the formula / pdMat / pdBlocked random-effects API
# shared by fit_demand_mixed() (NLME backend) and fit_demand_tmb() (TMB
# backend). Introduced in TICKET-011 Phase 1 — the canonical block
# representation returned by .normalize_re_input() is what Phase 2's
# Z-matrix builder will consume.

# ---------------------------------------------------------------------------
# Type detection
# ---------------------------------------------------------------------------

#' Classify a random_effects argument by structural type
#'
#' @param x User-supplied `random_effects` value.
#' @return One of `"character"`, `"formula"`, `"pdmat_single"`,
#'   `"pdmat_list"`, `"pdblocked"`. Errors on unrecognized input.
#' @keywords internal
#' @noRd
.classify_re_input <- function(x) {
  if (is.character(x)) return("character")
  if (inherits(x, "formula")) return("formula")
  if (inherits(x, "pdBlocked")) return("pdblocked")
  if (inherits(x, "pdMat")) return("pdmat_single")
  if (is.list(x) && length(x) > 0L &&
      all(vapply(x, inherits, logical(1), "pdMat"))) {
    return("pdmat_list")
  }
  stop(
    "unrecognized `random_effects` input; expected one of: character ",
    "vector, formula, nlme::pdMat, list of pdMat, or nlme::pdBlocked.",
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# Canonical block representation
# ---------------------------------------------------------------------------

#' Normalize any supported `random_effects` input to a canonical block list
#'
#' The returned structure is backend-agnostic: both the NLME glue and the
#' TMB glue consume it. Each block describes a single `pdMat` component
#' (or the semantic equivalent of the character shortcut).
#'
#' @param random_effects Raw input (character vector, formula, pdMat, list,
#'   or pdBlocked).
#' @param covariance_structure `"pdDiag"` or `"pdSymm"`. Used only when
#'   `random_effects` is a formula. Ignored (with no warning at this level)
#'   for pdMat / list / pdBlocked inputs, which carry their own covariance
#'   class.
#' @param data Optional data frame. Required when the formula or pdMat has
#'   any non-intercept RHS terms so the parser can expand contrasts.
#'
#' @return A list with `blocks` (list of per-block specs), `source` (the
#'   detected input type), and `original` (the raw input preserved for
#'   round-tripping).
#' @keywords internal
#' @noRd
.normalize_re_input <- function(random_effects, covariance_structure, data = NULL) {
  type <- .classify_re_input(random_effects)
  blocks <- switch(
    type,
    character = .normalize_character_re(random_effects),
    formula   = .normalize_formula_re(random_effects, covariance_structure, data),
    pdmat_single = list(.normalize_pdmat_block(random_effects, data)),
    pdmat_list   = lapply(random_effects, .normalize_pdmat_block, data = data),
    pdblocked    = {
      n <- length(random_effects)
      lapply(seq_len(n), function(i) .normalize_pdmat_block(random_effects[[i]], data))
    }
  )
  list(
    blocks = blocks,
    source = type,
    original = random_effects
  )
}

# Character shortcut: c("q0","alpha") or "q0"
.normalize_character_re <- function(chr) {
  valid_re <- c("q0", "alpha")
  bad <- setdiff(chr, valid_re)
  if (length(bad) > 0L) {
    stop(
      "`random_effects` must be a subset of c('q0','alpha'); got ",
      paste(shQuote(bad), collapse = ", "),
      call. = FALSE
    )
  }
  chr <- intersect(valid_re, chr)  # canonical order
  if (!("q0" %in% chr)) {
    stop(
      "`random_effects` must include 'q0' (alpha-only REs are not supported).",
      call. = FALSE
    )
  }
  if (identical(chr, "q0")) {
    list(list(
      pdmat_class = "pdDiag",
      formula     = Q0 ~ 1,
      terms_q0    = "(Intercept)",
      terms_alpha = character(0),
      dim         = 1L
    ))
  } else {
    # c("q0","alpha") — unstructured 2x2 for back-compat with the legacy
    # character API (matches MixedDemand.h n_re == 2 behavior).
    list(list(
      pdmat_class = "pdSymm",
      formula     = Q0 + alpha ~ 1,
      terms_q0    = "(Intercept)",
      terms_alpha = "(Intercept)",
      dim         = 2L
    ))
  }
}

# Formula input: Q0 [+ alpha] ~ <rhs>
.normalize_formula_re <- function(form, covariance_structure, data) {
  if (!inherits(form, "formula") || length(form) != 3L) {
    stop("formula `random_effects` must be two-sided (e.g., Q0 + alpha ~ 1).",
         call. = FALSE)
  }
  valid_params <- c("Q0", "alpha")
  lhs_vars <- all.vars(form[[2]])
  bad <- setdiff(lhs_vars, valid_params)
  if (length(bad) > 0L) {
    stop("LHS of RE formula must be Q0 and/or alpha; got ",
         paste(shQuote(bad), collapse = ", "), ".", call. = FALSE)
  }
  if (!("Q0" %in% lhs_vars)) {
    stop("RE formula LHS must include Q0 (alpha-only REs are not supported).",
         call. = FALSE)
  }
  if (!(covariance_structure %in% c("pdDiag", "pdSymm"))) {
    stop("covariance_structure must be 'pdDiag' or 'pdSymm'.", call. = FALSE)
  }

  rhs_form <- stats::as.formula(paste("~", deparse1(form[[3]])))
  terms_template <- .re_rhs_terms(rhs_form, data)

  terms_q0 <- if ("Q0" %in% lhs_vars) terms_template else character(0)
  terms_alpha <- if ("alpha" %in% lhs_vars) terms_template else character(0)

  list(list(
    pdmat_class = covariance_structure,
    formula     = form,
    terms_q0    = terms_q0,
    terms_alpha = terms_alpha,
    dim         = length(terms_q0) + length(terms_alpha)
  ))
}

# Single pdMat block (pdSymm or pdDiag) — passed either directly or as a
# component of pdBlocked / list-of-pdMat.
.normalize_pdmat_block <- function(pd, data) {
  cls <- class(pd)[1]
  if (!(cls %in% c("pdSymm", "pdDiag"))) {
    stop(
      "pdMat class '", cls, "' is not supported; use pdSymm or pdDiag. ",
      "Additional classes (pdIdent, pdLogChol, pdCompSymm) can be added in a ",
      "follow-up ticket.", call. = FALSE
    )
  }
  form_list <- stats::formula(pd)  # listForm: one one-sided formula per param
  if (length(form_list) == 0L) {
    stop("pdMat has no formula; cannot normalize.", call. = FALSE)
  }
  lhs_vars <- vapply(form_list, function(f) as.character(f[[2]]), character(1))
  # All components in a listForm share the same RHS
  rhs_expr <- form_list[[1]][[length(form_list[[1]])]]
  rhs_form <- stats::as.formula(paste("~", deparse1(rhs_expr)))
  terms_template <- .re_rhs_terms(rhs_form, data)

  terms_q0    <- if ("Q0"    %in% lhs_vars) terms_template else character(0)
  terms_alpha <- if ("alpha" %in% lhs_vars) terms_template else character(0)

  composite <- stats::as.formula(
    paste(paste(lhs_vars, collapse = " + "), "~", deparse1(rhs_expr))
  )

  list(
    pdmat_class = cls,
    formula     = composite,
    terms_q0    = terms_q0,
    terms_alpha = terms_alpha,
    dim         = length(terms_q0) + length(terms_alpha)
  )
}

# Expand an RHS formula (one-sided) into contrast column names.
# For `~ 1` this is just `"(Intercept)"` — no data needed.
.re_rhs_terms <- function(rhs_form, data) {
  tt <- stats::terms(rhs_form)
  vars <- all.vars(rhs_form)
  if (length(vars) == 0L) {
    # Intercept-only; no data needed
    if (attr(tt, "intercept") == 1L) "(Intercept)" else character(0)
  } else {
    if (is.null(data)) {
      stop(
        "RE formula has non-intercept terms (", paste(vars, collapse = ", "),
        "); supply `data` so the parser can expand contrasts.",
        call. = FALSE
      )
    }
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0L) {
      stop(
        "RE formula references variable(s) not in `data`: ",
        paste(shQuote(missing_vars), collapse = ", "),
        call. = FALSE
      )
    }
    mf <- stats::model.frame(rhs_form, data = data)
    colnames(stats::model.matrix(rhs_form, data = mf))
  }
}

# ---------------------------------------------------------------------------
# Phase-1 template gate
# ---------------------------------------------------------------------------

#' Is this RE spec fittable by the existing (Phase-1) TMB template?
#'
#' The `src/MixedDemand.h` template ships with a single 1x1 (pdDiag) or
#' 2x2 (pdSymm) covariance on intercepts only. Anything richer must wait
#' for Phase 2 (Z-matrix generalization).
#' @keywords internal
#' @noRd
.re_is_phase1_fittable <- function(re_parsed) {
  if (length(re_parsed$blocks) != 1L) return(FALSE)
  b <- re_parsed$blocks[[1]]
  ok_q0 <- length(b$terms_q0) == 0L || identical(b$terms_q0, "(Intercept)")
  ok_al <- length(b$terms_alpha) == 0L || identical(b$terms_alpha, "(Intercept)")
  ok_q0 && ok_al
}

#' Collapse a Phase-1-fittable re_parsed back to the character shortcut
#' `c("q0")` / `c("q0","alpha")` so the existing fit_demand_tmb() body
#' (map builder, template dispatch) can continue to consume it unchanged.
#' @keywords internal
#' @noRd
.re_parsed_to_character <- function(re_parsed) {
  if (!.re_is_phase1_fittable(re_parsed)) {
    stop(".re_parsed_to_character() called on a non-Phase-1 RE spec.",
         call. = FALSE)
  }
  b <- re_parsed$blocks[[1]]
  out <- character(0)
  if (length(b$terms_q0) > 0L) out <- c(out, "q0")
  if (length(b$terms_alpha) > 0L) out <- c(out, "alpha")
  out
}

# ---------------------------------------------------------------------------
# Deprecation helper and summary
# ---------------------------------------------------------------------------

#' One-shot soft-deprecation message for character-vector random_effects
#' @keywords internal
#' @noRd
.deprecate_character_re <- function() {
  lifecycle::deprecate_soft(
    when  = "0.4.0",
    what  = "fit_demand_tmb(random_effects)",
    details = c(
      "i" = "Character-vector input is deprecated; use a formula instead.",
      "i" = "Replace `c(\"q0\", \"alpha\")` with `Q0 + alpha ~ 1` (unstructured 2x2).",
      "i" = "Replace `c(\"q0\")` with `Q0 ~ 1` (Q0 only).",
      "i" = "See ?fit_demand_tmb for the full formula / pdMat API (TICKET-011)."
    ),
    id    = "fit_demand_tmb_chr_re"
  )
}

#' Short human-readable summary of the RE shape — attached to fit objects
#' for downstream S3 methods and `check_demand_model()`.
#' @keywords internal
#' @noRd
.re_shape_summary <- function(re_parsed) {
  block_descs <- vapply(re_parsed$blocks, function(b) {
    sprintf(
      "%s(Q0:%d, alpha:%d)",
      b$pdmat_class,
      length(b$terms_q0),
      length(b$terms_alpha)
    )
  }, character(1))
  paste(block_descs, collapse = " + ")
}

# ---------------------------------------------------------------------------
# Identifiability / data-shape validation
# ---------------------------------------------------------------------------

#' Validate that the parsed RE spec makes sense against the provided data
#'
#' Phase-1-fittable shapes are trivially valid (intercept-only on a single
#' grouping factor). For richer shapes — which the Phase-1 gate will reject
#' before fitting anyway — verify that every RE formula variable exists in
#' the data and varies within `id_var`.
#' @keywords internal
#' @noRd
.validate_re_input <- function(re_parsed, data, id_var) {
  if (.re_is_phase1_fittable(re_parsed)) return(invisible(NULL))
  if (!(id_var %in% names(data))) {
    stop("id_var '", id_var, "' not in data.", call. = FALSE)
  }
  for (b in re_parsed$blocks) {
    rhs_vars <- setdiff(all.vars(b$formula), c("Q0", "alpha"))
    if (length(rhs_vars) == 0L) next
    missing_vars <- setdiff(rhs_vars, names(data))
    if (length(missing_vars) > 0L) {
      stop(
        "RE formula references variable(s) not in `data`: ",
        paste(shQuote(missing_vars), collapse = ", "),
        call. = FALSE
      )
    }
    for (v in rhs_vars) {
      varies <- tapply(
        data[[v]], data[[id_var]],
        function(vals) length(unique(vals)) > 1L
      )
      if (!any(varies, na.rm = TRUE)) {
        stop(
          "Random slopes on '", v, "' require a within-subject factor; ",
          "'", v, "' is between-subjects in this data. ",
          "Use `factors = '", v, "'` for between-subjects fixed effects instead.",
          call. = FALSE
        )
      }
    }
  }
  invisible(NULL)
}
