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
# pkgdown builds run examples/vignettes in callr subprocesses. Prefer a
# headless bitmap device to avoid GUI-related rendering issues.
if (identical(Sys.getenv("CALLR_IS_RUNNING"), "true")) {
  options(device = grDevices::png)
}
