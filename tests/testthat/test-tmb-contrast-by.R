# Tests for TICKET-032: contrast_by (by-grouped contrasts) on the TMB backend,
# plus the additive NLME-side changes (contrast_by_map attribute population and
# the `by` not-in-`compare_specs` pre-validation that replaces NLME's old
# silent-empty path).
#
# Fixtures are heavy (TMB interaction fit ~16s), so they are file-level memoized
# via local() closures per the CI-OOM lesson. Balanced 2x2 fixture:
# apt_full[gender in {Female, Male}] x age_cut (median split), balanced BY
# SUBJECT (id is the unit; each id keeps all its price points).

# ---- Shared balanced 2x2 data (by-subject balance) ------------------------
.h32_balanced_data <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    data(apt_full, package = "beezdemand", envir = environment())
    dat <- apt_full[apt_full$gender %in% c("Female", "Male"), ]
    dat$gender <- factor(dat$gender, levels = c("Female", "Male"))
    dat$age_cut <- factor(
      ifelse(dat$age < stats::median(dat$age, na.rm = TRUE), "young", "old"),
      levels = c("young", "old")
    )
    set.seed(2032)
    ids <- dplyr::distinct(dat, .data$id, .data$gender, .data$age_cut)
    keep <- ids |>
      dplyr::group_by(.data$gender, .data$age_cut) |>
      dplyr::slice_sample(n = 50) |>
      dplyr::ungroup()
    dat <- dat[dat$id %in% keep$id, ]
    dat$y_ll4 <- ll4(dat$y, lambda = 4)
    cached <<- dat
    cached
  }
})

# ---- TMB interaction fit (by-cells genuinely differ) ----------------------
.h32_balanced_fit <- local({
  fit <- NULL
  function() {
    if (!is.null(fit)) return(fit)
    fit <<- suppressWarnings(fit_demand_tmb(
      .h32_balanced_data(), equation = "exponential",
      factors = c("gender", "age_cut"), factor_interaction = TRUE,
      verbose = 0
    ))
    fit
  }
})

# ---- Asymmetric-collapse TMB fit (Q0: 2 levels, alpha: 3 levels) ----------
# age_group collapses to junior/old for Q0 (factors_q0 = age_group_Q0) but keeps
# young/mid/old for alpha (factors_alpha = age_group_alpha). gender is shared.
.h32_collapse_data <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    data(apt_full, package = "beezdemand", envir = environment())
    d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
    d$gender <- factor(d$gender, levels = c("Female", "Male"))
    d$age_group <- factor(cut(d$age, c(0, 25, 35, Inf),
                              labels = c("young", "mid", "old")))
    d <- d[!is.na(d$age_group), ]
    set.seed(99)
    ids <- dplyr::distinct(d, .data$id, .data$gender, .data$age_group)
    keep <- ids |>
      dplyr::group_by(.data$gender, .data$age_group) |>
      dplyr::slice_sample(n = 20) |>
      dplyr::ungroup()
    d <- d[d$id %in% keep$id, ]
    d$id <- droplevels(as.factor(d$id))
    cached <<- d
    cached
  }
})

.h32_collapse_fit <- local({
  fit <- NULL
  function() {
    if (!is.null(fit)) return(fit)
    fit <<- suppressWarnings(fit_demand_tmb(
      .h32_collapse_data(), equation = "exponential",
      factors = c("gender", "age_group"),
      collapse_levels = list(
        Q0    = list(age_group = list(junior = c("young", "mid"), old = "old")),
        alpha = list(age_group = list(young = "young", mid = "mid", old = "old"))
      ),
      verbose = 0
    ))
    fit
  }
})

# ---- NLME asymmetric-collapse fit (for the boundary/pre-validation gap) ----
.h32_nlme_collapse_fit <- local({
  fit <- NULL
  function() {
    if (!is.null(fit)) return(fit)
    d <- .h32_collapse_data()
    d$y_ll4 <- ll4(d$y, lambda = 4)
    fit <<- suppressMessages(suppressWarnings(fit_demand_mixed(
      d, y_var = "y_ll4", x_var = "x", id_var = "id",
      factors = c("gender", "age_group"), equation_form = "zben",
      collapse_levels = list(
        Q0    = list(age_group = list(junior = c("young", "mid"), old = "old")),
        alpha = list(age_group = list(young = "young", mid = "mid", old = "old"))
      )
    )))
    fit
  }
})

# ---- NLME fit (zben, additive by default) ---------------------------------
.h32_nlme_fit <- local({
  fit <- NULL
  function() {
    if (!is.null(fit)) return(fit)
    fit <<- suppressMessages(suppressWarnings(fit_demand_mixed(
      .h32_balanced_data(), y_var = "y_ll4", x_var = "x", id_var = "id",
      factors = c("gender", "age_cut"), equation_form = "zben"
    )))
    fit
  }
})

# ===========================================================================
# Test 5 (Critical) — by not in compare_specs aborts loudly on BOTH backends
# ===========================================================================
test_that("by not in compare_specs aborts loudly on both backends", {
  skip_on_cran()

  # age_cut IS a fitted factor but is NOT named in compare_specs (~ gender).
  expect_error(
    suppressMessages(get_demand_comparisons(
      .h32_balanced_fit(), param = "Q0",
      compare_specs = ~ gender, contrast_by = "age_cut"
    )),
    regexp = "compare_specs"
  )

  expect_error(
    suppressMessages(get_demand_comparisons(
      .h32_nlme_fit(), param = "Q0",
      compare_specs = ~ gender, contrast_by = "age_cut"
    )),
    regexp = "compare_specs"
  )
})

# ===========================================================================
# Test 5b (Critical) — NLME COLLAPSE path also aborts (Codex Blocking 1)
# ===========================================================================
# The collapse mapping must NOT silently drop a typo or a valid-but-not-in-
# compare_specs contrast_by. Boundary validation catches the typo; per-param
# resolution against the FULL parameter factor set + pre-validation catches the
# valid-factor-omitted-from-compare_specs case.
test_that("NLME collapse: typo and not-in-compare_specs contrast_by abort", {
  skip_on_cran()
  fit <- .h32_nlme_collapse_fit()
  # (A) valid fitted factor, but NOT named in compare_specs (~ gender)
  expect_error(
    suppressMessages(get_demand_comparisons(
      fit, param = "Q0", compare_specs = ~ gender, contrast_by = "age_group"
    )),
    regexp = "compare_specs"
  )
  # (B) typo (no such fitted factor)
  expect_error(
    suppressMessages(get_demand_comparisons(
      fit, param = "Q0",
      compare_specs = ~ gender * age_group, contrast_by = "age_grp"
    )),
    regexp = "age_grp|not in the fit"
  )
})

# ===========================================================================
# Test 4 (High) — boundary abort on a contrast_by typo (not in any factor set)
# ===========================================================================
test_that("boundary abort on contrast_by typo (not in any factor set)", {
  skip_on_cran()
  expect_error(
    suppressMessages(get_demand_comparisons(
      .h32_balanced_fit(), param = "Q0",
      compare_specs = ~ gender * age_cut, contrast_by = "agecut"  # typo
    )),
    regexp = "agecut"
  )
})

# ===========================================================================
# Test 1 (Critical) — self-consistency: by-cell equals at-filtered route
# ===========================================================================
test_that("by-cell {age_cut='young'} equals at = list(age_cut='young') (TMB)", {
  skip_on_cran()
  fit <- .h32_balanced_fit()

  comps_by <- suppressMessages(get_demand_comparisons(
    fit, param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  ))
  comps_at <- suppressMessages(get_demand_comparisons(
    fit, param = "Q0",
    compare_specs = ~ gender, at = list(age_cut = "young")
  ))

  # Drop the internal `std_labels` attribute (plumbing, not part of the
  # compared contract); compare the public columns only.
  strip <- function(d) {
    d <- as.data.frame(d)
    attr(d, "std_labels") <- NULL
    d
  }
  by_young <- comps_by$Q0$contrasts_log10 |>
    dplyr::filter(.data$age_cut == "young") |>
    dplyr::select("contrast", "estimate", "std.error", "p.value") |>
    strip()
  at_route <- comps_at$Q0$contrasts_log10 |>
    dplyr::select("contrast", "estimate", "std.error", "p.value") |>
    strip()

  expect_equal(by_young, at_route, tolerance = 1e-12)
})

# ===========================================================================
# Test 13 (Critical) — $contrasts_ratio carries the same by-cols as log10 (TMB)
# ===========================================================================
test_that("$contrasts_ratio has by-cols matching $contrasts_log10 (TMB)", {
  skip_on_cran()
  res <- suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  ))
  log10_tbl <- res$Q0$contrasts_log10
  ratio_tbl <- res$Q0$contrasts_ratio

  # by-col present in BOTH, as the first column, with user-original name
  expect_true("age_cut" %in% names(log10_tbl))
  expect_true("age_cut" %in% names(ratio_tbl))
  expect_identical(names(ratio_tbl)[1], "age_cut")
  # ratio nested schema: by-cols + contrast + ratio + conf.low + conf.high + p.value
  expect_identical(
    names(ratio_tbl),
    c("age_cut", "contrast", "ratio", "conf.low", "conf.high", "p.value")
  )
  # ratio = 10^estimate per row (log10 scale on TMB)
  expect_equal(ratio_tbl$ratio, 10^log10_tbl$estimate, tolerance = 1e-10)
})

# ===========================================================================
# Test 14 (Critical regression) — $contrasts_ratio no-by schema unchanged
# ===========================================================================
test_that("$contrasts_ratio no-by schema unchanged (regression pin)", {
  skip_on_cran()
  res <- suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0", compare_specs = ~ gender
  ))
  expect_identical(
    names(res$Q0$contrasts_ratio),
    c("contrast", "ratio", "conf.low", "conf.high", "p.value")
  )
})

# ===========================================================================
# Test 10 (Critical) — flat tidy() no-by schema is exactly the 9-col contract
# ===========================================================================
test_that("flat tidy() no-by schema is exactly 9 cols", {
  skip_on_cran()
  contract <- c("param", "contrast", "estimate", "std.error",
                "statistic", "df", "conf.low", "conf.high", "p.value")
  td <- broom::tidy(suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0", compare_specs = ~ gender * age_cut
  )))
  expect_identical(names(td), contract)
})

# ===========================================================================
# Test 11 (High) — flat tidy() with-by inserts by-cols with user-original names
# ===========================================================================
test_that("flat tidy() with-by inserts by-cols (user-original) before param", {
  skip_on_cran()
  td <- broom::tidy(suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  )))
  # by-col present, user-original name, inserted FIRST (before param)
  expect_identical(names(td)[1], "age_cut")
  expect_true(all(c("param", "contrast", "estimate") %in% names(td)))
  # contrast labels exclude the by-var
  expect_true(all(grepl("Female - Male", td$contrast)))
  expect_setequal(unique(td$age_cut), c("young", "old"))
})

# ===========================================================================
# Test 12 (Medium) — flat tidy() treats the "NULL" literal sentinel as inactive
# ===========================================================================
test_that("flat tidy() treats 'NULL' literal sentinel as inactive", {
  skip_on_cran()
  contract <- c("param", "contrast", "estimate", "std.error",
                "statistic", "df", "conf.low", "conf.high", "p.value")
  res <- suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0", compare_specs = ~ gender
  ))
  expect_identical(attr(res, "contrast_by_used"), "NULL")
  expect_identical(names(broom::tidy(res)), contract)
})

# ===========================================================================
# Test 17 (Low) — empty-active tidy returns zero-row tibble with by-cols
# ===========================================================================
test_that("empty-active tidy returns zero-row tibble with by-cols inserted", {
  skip_on_cran()
  # Hand-build a beezdemand_comparison whose contrast table is empty but whose
  # contrast_by_used attribute is active.
  obj <- list(Q0 = list(
    emmeans = tibble::tibble(),
    contrasts_log10 = tibble::tibble()
  ))
  class(obj) <- "beezdemand_comparison"
  attr(obj, "backend") <- "tmb"
  attr(obj, "contrast_by_used") <- "age_cut"
  attr(obj, "contrast_by_map") <- list(Q0 = stats::setNames(character(0), character(0)))
  attr(obj, "adjustment_method") <- "holm"

  td <- broom::tidy(obj)
  expect_equal(nrow(td), 0L)
  expect_identical(names(td)[1], "age_cut")
  expect_true("param" %in% names(td))
})

# ===========================================================================
# Test 18 (Low) — print() includes by-cols before contrast when active
# ===========================================================================
test_that("print() includes by-cols before contrast when contrast_by active", {
  skip_on_cran()
  res <- suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  ))
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "age_cut")
})

# ===========================================================================
# Test 16 (Low) — within-param collision aborts
# ===========================================================================
test_that("within-param collision aborts when two by-names map to one column", {
  skip_on_cran()
  # Hand-build a fit whose param_info would resolve two distinct requested
  # by-names to the SAME effective column. We simulate by directly exercising
  # the resolution path: a fit with a factor `age_cut` plus `age_cut_Q0`.
  fit <- .h32_balanced_fit()
  fit2 <- fit
  # both "age_cut" and "age_cut_X" resolve to "age_cut" if X == Q0 alias; here
  # we craft param_info so age_cut (direct) and age (via age_Q0 alias) collide.
  fit2$param_info$factors_q0 <- c("gender", "age_cut")
  expect_error(
    suppressMessages(get_demand_comparisons(
      fit2, param = "Q0",
      compare_specs = ~ gender * age_cut,
      contrast_by = c("age_cut", "age_cut")
    )),
    regexp = "same column|resolve"
  )
})

# ===========================================================================
# Test 9 (Medium) — additive heads-up message fires under !factor_interaction
# ===========================================================================
test_that("additive heads-up message fires under !factor_interaction (NLME)", {
  skip_on_cran()
  # NLME default fit is additive (factor_interaction == FALSE).
  expect_message(
    get_demand_comparisons(
      .h32_nlme_fit(), param = "Q0",
      compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
    ),
    regexp = "additive"
  )
})

# ===========================================================================
# Test 15 (Medium) — contrast_by_map populated on both backends
# ===========================================================================
test_that("contrast_by_map populated on both backends", {
  skip_on_cran()
  res_tmb <- suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  ))
  res_nlme <- suppressMessages(get_demand_comparisons(
    .h32_nlme_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  ))
  m_tmb <- attr(res_tmb, "contrast_by_map")
  m_nlme <- attr(res_nlme, "contrast_by_map")
  expect_type(m_tmb, "list")
  expect_type(m_nlme, "list")
  # no collapse here -> original maps to itself
  expect_identical(unname(m_tmb$Q0[["age_cut"]]), "age_cut")
  expect_identical(unname(m_nlme$Q0[["age_cut"]]), "age_cut")
})

# ===========================================================================
# Test 2 (High) — cross-backend direction parity (sign agreement)
# ===========================================================================
test_that("cross-backend direction parity (TMB vs NLME sign)", {
  skip_on_cran()
  td_tmb <- broom::tidy(suppressMessages(get_demand_comparisons(
    .h32_balanced_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  )))
  td_nlme <- broom::tidy(suppressMessages(get_demand_comparisons(
    .h32_nlme_fit(), param = "Q0",
    compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
  )))
  # same by-cells, same contrast labels (compared as character; emmeans returns
  # the NLME contrast column as a factor), same sign of the estimate per cell
  key <- function(d) {
    d$contrast <- as.character(d$contrast)
    d <- d[order(d$age_cut, d$contrast), c("age_cut", "contrast")]
    as.data.frame(d, row.names = FALSE)
  }
  expect_identical(key(td_tmb), key(td_nlme))
  td_tmb$contrast <- as.character(td_tmb$contrast)
  td_nlme$contrast <- as.character(td_nlme$contrast)
  merged <- merge(td_tmb, td_nlme, by = c("age_cut", "contrast"))
  expect_true(all(sign(merged$estimate.x) == sign(merged$estimate.y)))
})

# ===========================================================================
# Test 3 (High) — asymmetric collapse maps the by-var per parameter
# ===========================================================================
test_that("asymmetric collapse maps by-var to per-parameter column", {
  skip_on_cran()
  res <- suppressMessages(get_demand_comparisons(
    .h32_collapse_fit(), param = c("Q0", "alpha"),
    compare_specs = ~ gender * age_group, contrast_by = "age_group"
  ))
  m <- attr(res, "contrast_by_map")
  expect_identical(unname(m$Q0[["age_group"]]), "age_group_Q0")
  expect_identical(unname(m$alpha[["age_group"]]), "age_group_alpha")
  # nested by-column carries the USER-ORIGINAL name on both parameters
  expect_true("age_group" %in% names(res$Q0$contrasts_log10))
  expect_true("age_group" %in% names(res$alpha$contrasts_log10))
})

# ===========================================================================
# Test 6 (High) — per-param resolution is independent under asymmetric collapse
# ===========================================================================
# NB: the literal "alpha-only soft skip" of Decision 2 is architecturally
# unreachable via the public API -- a collapsed factor's per-parameter column
# (age_group_Q0 / age_group_alpha) always remains in param_info$factors_*, so it
# RESOLVES rather than skips, and `compare_specs` naming a fully-collapsed-away
# factor aborts at the TICKET-016 resolver first. The `else` skip branch is
# therefore defensive. This test instead pins the substantive guarantee: each
# parameter's by-grouping uses ITS OWN collapsed column, yielding the correct
# (different) number of by-cells (Q0: 2, alpha: 3).
test_that("per-param by-grouping uses each parameter's own collapsed column", {
  skip_on_cran()
  res <- suppressMessages(get_demand_comparisons(
    .h32_collapse_fit(), param = c("Q0", "alpha"),
    compare_specs = ~ gender * age_group, contrast_by = "age_group"
  ))
  # Q0 collapsed to 2 age levels -> 2 by-cells; alpha keeps 3 -> 3 by-cells.
  expect_setequal(unique(res$Q0$contrasts_log10$age_group), c("junior", "old"))
  expect_setequal(unique(res$alpha$contrasts_log10$age_group),
                  c("young", "mid", "old"))
})

# ===========================================================================
# Test 7 (Medium) — redundant-by (length-1, set-equal) message + fall-through
# ===========================================================================
test_that("redundant-by emits message and falls through to plain pairwise", {
  skip_on_cran()
  expect_message(
    res <- get_demand_comparisons(
      .h32_balanced_fit(), param = "Q0",
      compare_specs = ~ gender, contrast_by = "gender"
    ),
    regexp = "redundant"
  )
  # fell through to plain pairwise: no by-column, single gender contrast
  expect_false("gender" %in% names(res$Q0$contrasts_log10))
  expect_equal(nrow(res$Q0$contrasts_log10), 1L)
  # contrast_by_used reports "NULL" (no by applied) and tidy() is the clean
  # 9-col contract with NO all-NA by-column (Codex review Recommended 1).
  expect_identical(attr(res, "contrast_by_used"), "NULL")
  expect_identical(
    names(broom::tidy(res)),
    c("param", "contrast", "estimate", "std.error",
      "statistic", "df", "conf.low", "conf.high", "p.value")
  )
})

# ===========================================================================
# Test 8 (Medium) — multi-factor compare_specs does NOT trigger redundancy
# ===========================================================================
test_that("multi-factor compare_specs does not trigger redundant-by", {
  skip_on_cran()
  msgs <- testthat::capture_messages(
    res <- get_demand_comparisons(
      .h32_balanced_fit(), param = "Q0",
      compare_specs = ~ gender * age_cut, contrast_by = "age_cut"
    )
  )
  expect_false(any(grepl("redundant", msgs)))
  # by-grouping proceeded: by-column present
  expect_true("age_cut" %in% names(res$Q0$contrasts_log10))
})
