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
