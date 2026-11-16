test_that("cannabisCigarettes has sequential integer participant ids", {
  env <- new.env()
  utils::data("cannabisCigarettes", package = "beezdemand", envir = env)
  d <- env$cannabisCigarettes

  expect_identical(
    names(d),
    c("X", "CigPrice", "CanPrice", "variable", "value", "id", "x", "commodity", "y")
  )
  expect_type(d$id, "integer")
  expect_identical(sort(unique(d$id)), seq_len(99))
  expect_true(all(table(d$id) == 50L))
})
