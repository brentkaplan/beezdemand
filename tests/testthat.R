# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(beezdemand)

# On CI, cap parallel test workers to bound peak memory. Heavy TMB/NLME fits
# (each carrying a large sdreport covariance) run many-wide under
# `Config/testthat/parallel: true` and OOM the memory-constrained oldrel-1
# runner. testthat::default_num_cpus() consults getOption("Ncpus") BEFORE the
# TESTTHAT_CPUS env var, and r-lib/actions sets Ncpus to the core count, so the
# cap must set Ncpus explicitly (the env var alone would be ignored).
if (nzchar(Sys.getenv("CI"))) {
  options(Ncpus = 2L)
  Sys.setenv(TESTTHAT_CPUS = "2")
}

test_check("beezdemand")
