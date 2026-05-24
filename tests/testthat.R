# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(beezdemand)

# Optionally cap parallel test workers to bound peak memory on constrained CI
# runners. Set via the BEEZDEMAND_TESTTHAT_CPUS env var (the R-CMD-check workflow
# sets it for the memory-tight Linux jobs). testthat::default_num_cpus() reads
# getOption("Ncpus") before the TESTTHAT_CPUS env var, so set both. Unset leaves
# testthat's default (2 in testthat 3.3.2). The resolved cpu inputs are printed
# below so the worker count is visible in tests/testthat.Rout for diagnosis.
.bz_cpus <- Sys.getenv("BEEZDEMAND_TESTTHAT_CPUS", "")
if (nzchar(.bz_cpus)) {
  options(Ncpus = as.integer(.bz_cpus))
  Sys.setenv(TESTTHAT_CPUS = .bz_cpus)
}
message(
  "beezdemand: testthat cpu inputs -> Ncpus=", getOption("Ncpus", "unset"),
  ", TESTTHAT_CPUS='", Sys.getenv("TESTTHAT_CPUS"), "'",
  ", BEEZDEMAND_TESTTHAT_CPUS='", .bz_cpus, "'"
)

test_check("beezdemand")
