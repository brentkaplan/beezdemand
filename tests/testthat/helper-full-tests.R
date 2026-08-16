# Heavy-file registry for the BEEZ_FULL_TESTS gate (TICKET-070 / Q14a).
#
# The hosted Linux CI runners kill a long, output-silent `checking tests`
# phase (stochastic lost-communication SIGTERM; see project memory
# ci-oom-diagnosis). With NOT_CRAN=true the full suite crosses that line, so
# the heaviest Monte-Carlo files run only when BEEZ_FULL_TESTS=true:
#   * .github/workflows/full-tests.yaml -- tri-OS, streaming reporter,
#     workflow_dispatch + weekly cron; must be green on every release SHA;
#   * dev/run-tests.R (full mode, the pre-push hook) sets it, so the local
#     gate still runs everything.
# R CMD check (CI R-CMD-check.yaml, CRAN, cran-everything) skips them.
#
# Every file listed here calls .skip_unless_full_tests() at its top; the
# registry test in test-full-tests-registry.R keeps the two in sync.
.beez_full_test_files <- c(
  # 2026-08-16 local timing run (NOT_CRAN=true, 2 workers; total 3548 s):
  "power-demand",   # 2167 s -- Monte Carlo power / find_n (61% of the suite)
  "anova-tmb",      #  201 s -- repeated apt_full TMB refits
  "boot-demand"     #  147 s -- parametric-bootstrap draws
  # everything else <= 120 s/file; remainder ~1030 s => ~9 min at 2 workers
)

.skip_unless_full_tests <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("BEEZ_FULL_TESTS"), "true"),
    "heavy test file: runs only with BEEZ_FULL_TESTS=true (full-tests.yaml / pre-push hook)"
  )
}
