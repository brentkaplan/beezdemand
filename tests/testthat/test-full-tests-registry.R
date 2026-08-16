# Keeps helper-full-tests.R's registry and the files that actually gate
# themselves in sync (same drift guard as dev/run-tests.R's smoke list).

test_that("BEEZ_FULL_TESTS registry matches the files that call the gate", {
  files <- list.files(test_path(), pattern = "^test-.*\\.R$")
  gated <- files[vapply(files, function(f) {
    # the gate call sits at column 0 of a heavy file (this test only mentions
    # it inside a string, indented)
    any(grepl("^\\.skip_unless_full_tests\\(\\)", readLines(test_path(f), warn = FALSE)))
  }, logical(1))]
  gated <- sub("^test-(.*)\\.R$", "\\1", gated)
  expect_setequal(gated, .beez_full_test_files)
  # ... and the gate must be the FIRST executable expression of the file, so
  # no fixture or fit above it runs in ungated CI.
  for (f in .beez_full_test_files) {
    ex <- parse(test_path(paste0("test-", f, ".R")), keep.source = FALSE)
    expect_identical(deparse(ex[[1]]), ".skip_unless_full_tests()",
                     info = paste0("test-", f, ".R"))
  }
  # names are interpolated into a regex by full-tests.yaml
  expect_false(any(grepl("[^A-Za-z0-9_-]", .beez_full_test_files)))
  # every registered file exists
  expect_true(all(file.exists(test_path(paste0("test-", .beez_full_test_files, ".R")))))
})
