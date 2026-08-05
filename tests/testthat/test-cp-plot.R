# plot() methods for the cross-price model classes.
#
# These three methods (plot.cp_model_nls, plot.cp_model_lm, plot.cp_model_lmer)
# had no test caller before TICKET-071. They were exercised only by the
# gitignored arg-matrix harness -- which asserted nothing beyond
# inherits(p, "ggplot") -- and by vignettes/cross-price-models.Rmd, which
# aborts the build on error but makes no assertions.
#
# Deliberately no skip_on_cran(): neither CI workflow sets NOT_CRAN, so a skip
# here would put these straight back out of CI (TICKET-070).

cp_plot_linear_data <- function(n_subjects = 20, seed = 42) {
  withr::local_seed(seed)
  ids <- paste0("id", seq_len(n_subjects))
  groups <- c("g1", "g2")
  x_vals <- exp(seq(log(0.5), log(25), length.out = 8))

  subject_df <- data.frame(
    id = ids,
    group = factor(rep(groups, length.out = n_subjects), levels = groups),
    intercept = stats::rnorm(n_subjects, mean = 10, sd = 2),
    slope = stats::rnorm(n_subjects, mean = -0.6, sd = 0.2),
    stringsAsFactors = FALSE
  )

  grid <- expand.grid(id = ids, x = x_vals, stringsAsFactors = FALSE)
  dat <- merge(grid, subject_df, by = "id")
  dat$y <- dat$intercept + dat$slope * dat$x +
    stats::rnorm(nrow(dat), sd = 0.8)

  dat[, c("id", "group", "x", "y")]
}

cp_plot_nls_data <- function(n = 30, seed = 7) {
  withr::local_seed(seed)
  x <- sort(stats::runif(n, min = 0.5, max = 25))
  y <- 10 * 10^(1.5 * exp(-0.05 * x)) *
    exp(stats::rnorm(n, mean = 0, sd = 0.05))
  data.frame(x = x, y = y)
}

# Layer order is NOT stable across these methods -- NLS and LM build
# line-then-point, LMER builds point-then-line -- so always look layers up by
# geom class rather than by index.
expect_point_and_line <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomLine" %in% geoms)

  built <- ggplot2::ggplot_build(p)$data
  point_dat <- built[[which(geoms == "GeomPoint")[1]]]
  line_dat <- built[[which(geoms == "GeomLine")[1]]]

  # Observed points and the prediction curve must both carry finite data --
  # an all-NA layer still renders and would otherwise pass a class-only check.
  expect_gt(nrow(point_dat), 0)
  expect_gt(nrow(line_dat), 0)
  expect_true(all(is.finite(point_dat$x)))
  expect_true(all(is.finite(point_dat$y)))
  expect_true(all(is.finite(line_dat$x)))
  expect_true(all(is.finite(line_dat$y)))
}

test_that("plot.cp_model_nls draws observed points and a prediction curve", {
  skip_if_not_installed("ggplot2")

  fit <- fit_cp_nls(
    cp_plot_nls_data(),
    equation = "exponentiated",
    return_all = TRUE
  )
  p <- plot(fit)

  expect_s3_class(p, "ggplot")
  expect_point_and_line(p)
  expect_identical(p$labels$x, "Price")
  expect_identical(p$labels$y, "Consumption")

  # The curve is drawn at n_points, independent of the number of observations.
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  line_dat <- ggplot2::ggplot_build(p)$data[[which(geoms == "GeomLine")[1]]]
  point_dat <- ggplot2::ggplot_build(p)$data[[which(geoms == "GeomPoint")[1]]]
  expect_equal(nrow(point_dat), 30)
  expect_equal(nrow(line_dat), 100)
})

test_that("plot.cp_model_nls honours n_points, labels and axis transforms", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  fit <- fit_cp_nls(
    cp_plot_nls_data(),
    equation = "exponentiated",
    return_all = TRUE
  )
  p <- plot(
    fit,
    n_points = 25,
    title = "Custom title",
    xlab = "Alt price",
    ylab = "Units",
    x_trans = "log10"
  )

  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  line_dat <- ggplot2::ggplot_build(p)$data[[which(geoms == "GeomLine")[1]]]

  expect_equal(nrow(line_dat), 25)
  expect_identical(p$labels$x, "Alt price")
  expect_identical(p$labels$y, "Units")
  expect_identical(p$labels$title, "Custom title")
  expect_identical(p$scales$get_scales("x")$trans$name, "log-10")
})

test_that("plot.cp_model_lm draws points and a curve for a fixed-effects fit", {
  skip_if_not_installed("ggplot2")

  dat <- cp_plot_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "fixed",
    group_effects = "intercept",
    return_all = TRUE
  )
  p <- plot(fit)

  expect_s3_class(p, "ggplot")
  expect_point_and_line(p)
  expect_identical(p$labels$x, "Price")
  expect_identical(p$labels$y, "Consumption")
})

test_that("plot.cp_model_lmer draws points and a curve for a mixed fit", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("lme4")

  dat <- cp_plot_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "mixed",
    group_effects = "intercept",
    return_all = TRUE
  )
  p <- plot(fit)

  expect_s3_class(p, "ggplot")
  expect_point_and_line(p)
})

test_that("plot.cp_model_lmer supports every pred_type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("lme4")

  dat <- cp_plot_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "mixed",
    group_effects = "intercept",
    return_all = TRUE
  )

  for (pt in c("fixed", "random", "all")) {
    p <- plot(fit, pred_type = pt)
    expect_s3_class(p, "ggplot")
    expect_point_and_line(p)
  }

  # "all" draws both the population curve and the subject curves, so it must
  # carry strictly more line data than "fixed" alone.
  n_line <- function(p) {
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    idx <- which(geoms == "GeomLine")
    sum(vapply(ggplot2::ggplot_build(p)$data[idx], nrow, integer(1)))
  }
  expect_gt(n_line(plot(fit, pred_type = "all")),
            n_line(plot(fit, pred_type = "fixed")))

  # Row counts alone would still pass if "random" drew the population curve
  # repeated once per subject. Pin the semantics: the random layer must contain
  # one group per subject, and those curves must not be identical to each other.
  line_layer <- function(p) {
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    ggplot2::ggplot_build(p)$data[[which(geoms == "GeomLine")[1]]]
  }
  rand <- line_layer(plot(fit, pred_type = "random"))
  expect_true("group" %in% names(rand))
  expect_equal(length(unique(rand$group)), length(unique(dat$id)))

  # Subject curves must actually differ. Compare y at the first shared x.
  first_x <- min(rand$x)
  y_at_first <- rand$y[rand$x == first_x]
  expect_gt(length(y_at_first), 1)
  expect_gt(length(unique(round(y_at_first, 8))), 1)

  # ...and "fixed" must be group-level, not subject-level: one population curve
  # per group_effects level, which is strictly fewer than one per subject.
  fixed_line <- line_layer(plot(fit, pred_type = "fixed"))
  expect_equal(length(unique(fixed_line$group)), length(levels(dat$group)))
  expect_lt(
    length(unique(fixed_line$group)),
    length(unique(rand$group))
  )
})

test_that("plot.cp_model_lmer rejects an unknown pred_type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("lme4")

  dat <- cp_plot_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "mixed",
    group_effects = "intercept",
    return_all = TRUE
  )

  expect_error(plot(fit, pred_type = "nonsense"))
})
