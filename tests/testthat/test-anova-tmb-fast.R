# Fast tests split out of the BEEZ_FULL_TESTS-gated test-anova-tmb.R so they
# still run in R CMD check / CI / CRAN.

test_that("anova S3 method is registered", {
  expect_false(is.null(getS3method("anova", "beezdemand_tmb", optional = TRUE)))
})

