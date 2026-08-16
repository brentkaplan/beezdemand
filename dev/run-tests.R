#!/usr/bin/env Rscript
# Test runner for the git hooks.
#
#   Rscript dev/run-tests.R smoke   -> fast subset  (used by pre-commit)
#   Rscript dev/run-tests.R         -> full suite   (used by pre-push)
#
# The smoke subset is the test files that fit NO demand models, so they run in
# seconds. The full suite is enforced at pre-push. Curate the smoke set by
# editing `smoke_files` below; keep heavy model-fitting tests OUT.

mode <- commandArgs(trailingOnly = TRUE)
mode <- if (length(mode) > 0L) mode[[1L]] else "full"

# Fail closed: an unrecognized mode must error rather than silently fall
# through to the full ~41-min suite (e.g. a typo'd "smoke").
if (!mode %in% c("smoke", "full")) {
  stop("run-tests.R: unknown mode '", mode, "'; expected 'smoke' or 'full'.",
       call. = FALSE)
}

# testthat's parallel mode defaults to only 2 workers unless Ncpus is set --
# spread the suite across (most of) the available cores instead.
ncores <- parallel::detectCores()
if (is.na(ncores) || ncores < 2L) ncores <- 2L
options(Ncpus = max(2L, ncores - 1L))

smoke_files <- c(
  "error-handling-classes",
  "alpha_star",
  "tmb-z-matrix-builder",
  "hurdle_simulate",
  "systematic-wrappers",
  "amplitude_persistence",
  "legacy-fitmean",
  "change",
  "pivot-demand-data",
  "issue-audit",
  "summarize",
  "k-parameter",
  "trans",
  "empirical-measures",
  "descriptive-summary",
  "systematic",
  "pmax-omax-engine",
  "equation-audit"
)

# The filter below interpolates these names straight into a regex, so keep them
# to characters that mean themselves. "foo.v2" would overmatch; "foo+bar" would
# fail to match its own file.
unsafe_names <- smoke_files[grepl("[^A-Za-z0-9_-]", smoke_files)]
if (length(unsafe_names) > 0L) {
  stop("run-tests.R: smoke_files names must be [A-Za-z0-9_-] only; got: ",
       paste(unsafe_names, collapse = ", "), call. = FALSE)
}

if (identical(mode, "smoke")) {
  # Guard against drift: every curated entry must map to a real test file.
  # A renamed or deleted file would otherwise just stop matching, silently
  # shrinking smoke coverage while the runner still reported success.
  expected <- file.path("tests", "testthat", paste0("test-", smoke_files, ".R"))
  missing <- smoke_files[!file.exists(expected)]
  if (length(missing) > 0L) {
    stop("run-tests.R: smoke_files entries with no matching test file: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # An empty selection would build the filter "^()$", match nothing, and report
  # success -- the same silent-shrink failure the drift guard exists to prevent.
  if (length(smoke_files) == 0L) {
    stop("run-tests.R: no smoke tests selected.", call. = FALSE)
  }

  # Anchored alternation so e.g. "systematic" does not also match
  # "systematic-wrappers" unless it is listed explicitly.
  filter <- paste0("^(", paste(smoke_files, collapse = "|"), ")$")
  res <- as.data.frame(devtools::test(filter = filter))
} else {
  # Full mode is the local gate for EVERYTHING, including the heavy files that
  # CI R-CMD-check skips (BEEZ_FULL_TESTS gate; see
  # tests/testthat/helper-full-tests.R and .github/workflows/full-tests.yaml).
  Sys.setenv(BEEZ_FULL_TESTS = "true")
  res <- as.data.frame(devtools::test())
}

# `devtools::test()` does not exit non-zero on failure; do it explicitly so the
# calling git hook's `set -e` aborts the commit/push.
if (sum(res$failed) > 0L || any(res$error)) {
  quit(status = 1L)
}
