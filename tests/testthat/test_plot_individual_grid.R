test_that("plot.beezdemand_nlme uses observed id × factor combos for individual lines", {
  skip_on_cran()

  # Larger synthetic dataset with a between-subject factor (Gender)
  set.seed(123)
  ids <- 1:30
  Gender <- factor(rep(c("M", "F"), length.out = length(ids)), levels = c("M", "F"))
  x_vals <- c(0, 0.25, 0.5, 1, 2, 4, 8, 12, 16, 24)

  # Generate y on log10 scale for equation_form = "zben"
  Q0_base <- 1.0      # log10(True Max)
  alpha_base <- -3.0  # log10(True Alpha)
  df_list <- lapply(seq_along(ids), function(i) {
    id_i <- ids[i]
    g_i <- Gender[i]
    # Individual-specific parameters with small variability
    Q0_i <- Q0_base + rnorm(1, sd = 0.15)
    alpha_i <- alpha_base + rnorm(1, sd = 0.15)
    y_log <- Q0_i * exp(-(10^alpha_i / Q0_i) * (10^Q0_i) * x_vals) + rnorm(length(x_vals), sd = 0.05)
    data.frame(
      id = factor(id_i, levels = ids),
      x = x_vals,
      y = y_log,          # y is log10 scale for zben
      Gender = g_i
    )
  })
  df <- do.call(rbind, df_list)
  rownames(df) <- NULL

  # Fit mixed model with a between-subject factor on zben form
  fit <- fit_demand_mixed(
    data = df,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("Gender"),
    factor_interaction = FALSE,
    equation_form = "zben",
    random_effects = Q0 + alpha ~ 1,
    covariance_structure = "pdDiag",
    method = "REML"
  )
  expect_s3_class(fit, "beezdemand_nlme")
  expect_false(is.null(fit$model))

  # Plot individual lines faceted by Gender with controlled n_points_pred
  p <- plot(
    fit,
    show_observed_data = FALSE,
    show_pred_lines = "individual",
    facet_formula = ~ Gender,
    n_points_pred = 50
  )
  expect_s3_class(p, "ggplot")

  # The single layer should be the individual prediction lines
  expect_true(length(p$layers) >= 1)
  layer_data <- p$layers[[1]]$data
  expect_true(is.data.frame(layer_data))
  expect_true(all(c("id", "Gender", "x", "predicted_y_plotscale") %in% names(layer_data)))

  # Expected rows = number of unique observed (id × Gender) combos times n_points_pred (50)
  id_gender_obs <- unique(df[, c("id", "Gender")])
  n_ids <- nrow(id_gender_obs)
  expected_rows <- n_ids * 50L
  expect_equal(nrow(layer_data), expected_rows)

  # Ensure each id appears in exactly one Gender level in prediction layer and matches observed
  id_gender_pred <- unique(layer_data[, c("id", "Gender")])
  expect_equal(nrow(id_gender_pred), n_ids)

  # Join and verify mapping matches
  joined <- merge(id_gender_pred, id_gender_obs, by = "id", suffixes = c("_pred", "_obs"))
  expect_equal(nrow(joined), n_ids)
  expect_true(all(as.character(joined$Gender_pred) == as.character(joined$Gender_obs)))

  # Sanity check to ensure we did NOT build full factorial across Gender levels
  full_factorial_rows <- n_ids * length(levels(df$Gender)) * 50L
  expect_true(nrow(layer_data) < full_factorial_rows)
})

test_that("plot.beezdemand_nlme supports within-subject factors for individual lines", {
  skip_on_cran()

  set.seed(456)
  ids <- 1:20
  Condition <- factor(rep(c("Low", "High"), each = length(ids)), levels = c("Low", "High"))
  Condition <- rep(c("Low", "High"), times = length(ids))
  Condition <- factor(Condition, levels = c("Low", "High"))

  x_vals <- c(0, 0.25, 0.5, 1, 2, 4, 8, 12)

  Q0_base <- 1.1
  alpha_base <- -3.2
  df_list <- lapply(ids, function(id_i) {
    # Each id measured at both Condition levels (within-subject)
    do.call(rbind, lapply(levels(Condition), function(cond) {
      # Small condition effect on Q0
      cond_shift <- if (cond == "High") 0.1 else -0.05
      Q0_i <- Q0_base + rnorm(1, sd = 0.1) + cond_shift
      alpha_i <- alpha_base + rnorm(1, sd = 0.1)
      y_log <- Q0_i * exp(-(10^alpha_i / Q0_i) * (10^Q0_i) * x_vals) + rnorm(length(x_vals), sd = 0.04)
      data.frame(
        id = factor(id_i, levels = ids),
        x = x_vals,
        y = y_log,
        Condition = factor(cond, levels = levels(Condition))
      )
    }))
  })
  df <- do.call(rbind, df_list)
  rownames(df) <- NULL

  fit <- fit_demand_mixed(
    data = df,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("Condition"),
    equation_form = "zben",
    random_effects = Q0 + alpha ~ 1,
    covariance_structure = "pdDiag",
    method = "REML"
  )
  expect_s3_class(fit, "beezdemand_nlme")
  expect_false(is.null(fit$model))

  p <- plot(
    fit,
    show_observed_data = FALSE,
    show_pred_lines = "individual",
    facet_formula = ~ Condition,
    n_points_pred = 40
  )
  expect_s3_class(p, "ggplot")

  layer_data <- p$layers[[1]]$data
  expect_true(is.data.frame(layer_data))
  expect_true(all(c("id", "Condition", "x", "predicted_y_plotscale") %in% names(layer_data)))

  # Expected rows = observed unique (id × Condition) combos × n_points_pred (40)
  id_cond_obs <- unique(df[, c("id", "Condition")])
  expected_rows <- nrow(id_cond_obs) * 40L
  expect_equal(nrow(layer_data), expected_rows)

  # For each id, the prediction layer must include exactly the observed Condition levels
  obs_counts <- aggregate(Condition ~ id, data = id_cond_obs, FUN = length)
  pred_counts <- aggregate(Condition ~ id, data = unique(layer_data[, c("id", "Condition")]), FUN = length)
  names(obs_counts)[2] <- "n_levels_obs"
  names(pred_counts)[2] <- "n_levels_pred"
  joined_counts <- merge(obs_counts, pred_counts, by = "id")
  expect_true(all(joined_counts$n_levels_obs == joined_counts$n_levels_pred))
})
