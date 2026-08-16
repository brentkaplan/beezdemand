# Shared fixtures for test-power-demand.R (heavy, BEEZ_FULL_TESTS-gated) and
# test-power-demand-fast.R. testthat sources helper-*.R before every test
# file (and in every parallel worker).

power_test_design <- function(...) {
  utils::modifyList(
    list(prices = c(0.1, 0.5, 1, 2, 5, 10)),
    list(...)
  )
}

fake_batch <- function(hit_rates_by_n) {
  # Returns a run_batch(n, batch_size, sim_offset) closure emitting
  # deterministic all-usable replicate tables with the given per-N hit rate.
  function(n, batch_size, sim_offset) {
    rate <- hit_rates_by_n[[as.character(n)]]
    if (is.null(rate)) {
      rate <- 0
    }
    hits <- round(batch_size * rate)
    hit <- c(rep(TRUE, hits), rep(FALSE, batch_size - hits))
    tibble::tibble(
      sim = seq_len(batch_size) + sim_offset,
      status = "ok",
      converged = TRUE,
      hessian_pd = TRUE,
      estimate = ifelse(hit, 1, 0),
      se = 0.1,
      statistic = ifelse(hit, 10, 0),
      p_value = ifelse(hit, 1e-6, 1),
      ci_lower = ifelse(hit, 0.8, -0.2),
      ci_upper = ifelse(hit, 1.2, 0.2),
      hit_p = hit,
      hit_ci = hit,
      message = NA_character_
    )
  }
}

