# Only activate renv for interactive sessions (not during R CMD check/build)
if (interactive() && Sys.getenv("R_PACKAGE_BUILD") == "") {
  source("renv/activate.R")
  options(renv.config.pak.enabled = TRUE)
}
