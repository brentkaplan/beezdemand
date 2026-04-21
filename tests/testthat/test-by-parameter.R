# Tests for `by` parameter in check_systematic_demand(), get_descriptive_summary(),
# and fit_demand_fixed()

test_that("beezdemand_split_by validates by columns", {
  d <- data.frame(id = 1:3, x = 1:3, y = 1:3)
  expect_error(
    beezdemand:::beezdemand_split_by(d, "nonexistent", identity),
    "not found in data"
  )
})

test_that("beezdemand_split_by splits correctly", {
  d <- data.frame(
    id = 1:6, x = rep(1:3, 2), y = 1:6,
    grp = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )
  out <- beezdemand:::beezdemand_split_by(d, "grp", function(slice, key) {
    list(n = nrow(slice), grp = key$grp)
  })
  expect_equal(nrow(out$group_keys), 2)
  expect_equal(out$results[[1]]$n, 3)
  expect_equal(out$results[[2]]$n, 3)
})

# -- check_systematic_demand(by = ...) -----------------------------------------

test_that("check_systematic_demand works without by (baseline)", {
  data(apt, package = "beezdemand")
  res <- check_systematic_demand(apt)
  expect_s3_class(res, "beezdemand_systematicity")
  expect_null(res$by_var)
  expect_true(nrow(res$results) > 0)
  expect_false("gender" %in% names(res$results))
})

test_that("check_systematic_demand(by = ...) groups results", {
  data(apt_full, package = "beezdemand")
  # Use a small subset for speed
  ids_keep <- unique(apt_full$id)[1:10]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  res <- check_systematic_demand(d, by = "gender")
  expect_s3_class(res, "beezdemand_systematicity")
  expect_equal(res$by_var, "gender")
  expect_true("gender" %in% names(res$results))
  expect_equal(nrow(res$results), length(ids_keep))

  # Print method should mention grouping
  output <- capture.output(print(res))
  expect_true(any(grepl("Grouped by", output)))
})

# -- get_descriptive_summary(by = ...) -----------------------------------------

test_that("get_descriptive_summary works without by (baseline)", {
  data(apt, package = "beezdemand")
  res <- get_descriptive_summary(apt)
  expect_s3_class(res, "beezdemand_descriptive")
  expect_null(res$by_var)
  expect_true(nrow(res$statistics) > 0)
})

test_that("get_descriptive_summary(by = ...) groups results", {
  data(apt_full, package = "beezdemand")
  ids_keep <- unique(apt_full$id)[1:10]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  res <- get_descriptive_summary(d, by = "gender")
  expect_s3_class(res, "beezdemand_descriptive")
  expect_equal(res$by_var, "gender")
  expect_true("gender" %in% names(res$statistics))
  expect_true("gender" %in% names(res$data))

  # More rows than ungrouped (statistics repeated per group)
  res_plain <- get_descriptive_summary(d)
  n_groups <- length(unique(d$gender))
  expect_equal(nrow(res$statistics), nrow(res_plain$statistics) * n_groups)

  # Print method should mention grouping
  output <- capture.output(print(res))
  expect_true(any(grepl("Grouped by", output)))
})

test_that("plot.beezdemand_descriptive facets when grouped", {
  data(apt_full, package = "beezdemand")
  ids_keep <- unique(apt_full$id)[1:6]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  res <- get_descriptive_summary(d, by = "gender")
  p <- plot(res)
  expect_s3_class(p, "ggplot")
  # Should have a facet
  expect_true(!is.null(p$facet) && !inherits(p$facet, "FacetNull"))
})

# -- fit_demand_fixed(by = ...) ------------------------------------------------

test_that("fit_demand_fixed works without by (baseline)", {
  data(apt, package = "beezdemand")
  ids_keep <- unique(apt$id)[1:3]
  d <- apt[apt$id %in% ids_keep, ]

  fit <- fit_demand_fixed(d, equation = "hs", k = 2)
  expect_s3_class(fit, "beezdemand_fixed")
  expect_false(inherits(fit, "beezdemand_fixed_grouped"))
})

test_that("fit_demand_fixed(by = ...) returns grouped object", {
  data(apt_full, package = "beezdemand")
  # Pick 4 subjects spanning 2 genders for speed
  ids_keep <- unique(apt_full$id)[1:4]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  fit <- fit_demand_fixed(d, equation = "hs", k = 2, by = "gender")
  expect_s3_class(fit, "beezdemand_fixed_grouped")
  expect_equal(fit$by_var, "gender")
  expect_true(length(fit$groups) > 0)

  # Each child should be beezdemand_fixed

  for (g in fit$groups) {
    expect_s3_class(g, "beezdemand_fixed")
  }
})

test_that("tidy.beezdemand_fixed_grouped prepends group columns", {
  data(apt_full, package = "beezdemand")
  ids_keep <- unique(apt_full$id)[1:4]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  fit <- fit_demand_fixed(d, equation = "hs", k = 2, by = "gender")
  td <- tidy(fit)
  expect_true("gender" %in% names(td))
  expect_true(nrow(td) > 0)
})

test_that("coef.beezdemand_fixed_grouped prepends group columns", {
  data(apt_full, package = "beezdemand")
  ids_keep <- unique(apt_full$id)[1:4]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  fit <- fit_demand_fixed(d, equation = "hs", k = 2, by = "gender")
  cf <- coef(fit)
  expect_true("gender" %in% names(cf))
  expect_true(nrow(cf) > 0)
})

test_that("glance.beezdemand_fixed_grouped has one row per group", {
  data(apt_full, package = "beezdemand")
  ids_keep <- unique(apt_full$id)[1:4]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  fit <- fit_demand_fixed(d, equation = "hs", k = 2, by = "gender")
  gl <- glance(fit)
  expect_true("gender" %in% names(gl))
  n_groups <- length(unique(d[d$id %in% ids_keep, "gender"]))
  expect_equal(nrow(gl), n_groups)
})

test_that("print.beezdemand_fixed_grouped shows grouping info", {
  data(apt_full, package = "beezdemand")
  ids_keep <- unique(apt_full$id)[1:4]
  d <- apt_full[apt_full$id %in% ids_keep, ]

  fit <- fit_demand_fixed(d, equation = "hs", k = 2, by = "gender")
  output <- capture.output(print(fit))
  expect_true(any(grepl("Grouped by", output)))
  expect_true(any(grepl("gender", output)))
})

test_that("beezdemand_split_by warns and drops NA in by columns", {
  d <- data.frame(
    id = 1:9, x = rep(1:3, 3), y = 1:9,
    grp = c("A", "A", "A", "B", "B", "B", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  expect_warning(
    out <- beezdemand:::beezdemand_split_by(d, "grp", function(slice, key) {
      list(n = nrow(slice), grp = key$grp)
    }),
    "3 row.*NA.*removed"
  )
  # Only two groups remain

  expect_equal(nrow(out$group_keys), 2)
  expect_equal(out$results[[1]]$n, 3)
  expect_equal(out$results[[2]]$n, 3)
})

test_that("by parameter errors on nonexistent columns", {
  data(apt, package = "beezdemand")
  expect_error(
    check_systematic_demand(apt, by = "nonexistent"),
    "not found in data"
  )
  expect_error(
    get_descriptive_summary(apt, by = "nonexistent"),
    "not found in data"
  )
  expect_error(
    fit_demand_fixed(apt, by = "nonexistent"),
    "not found in data"
  )
})
