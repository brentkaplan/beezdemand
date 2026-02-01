# Only activate renv for interactive sessions (not during R CMD check/build)
if (interactive() && Sys.getenv("R_PACKAGE_BUILD") == "") {
  source("renv/activate.R")
  options(renv.config.pak.enabled = TRUE)
}

# ---- Auto-start MCP session (interactive only) ----
if (interactive() &&
    requireNamespace("btw", quietly = TRUE)) {

  try({
    # mcptools::mcp_session()
    btw::btw_mcp_session()
  }, silent = TRUE)

}

# ---- Graphics device for callr subprocesses ----
# pkgdown builds run examples/vignettes in callr subprocesses. On macOS, the
# default PNG/quartz device can segfault when measuring text (grid/ggplot2).
# Prefer ragg for stable, headless rendering during builds.
if (identical(Sys.getenv("CALLR_IS_RUNNING"), "true") &&
    requireNamespace("ragg", quietly = TRUE)) {
  options(device = ragg::agg_png)
}
