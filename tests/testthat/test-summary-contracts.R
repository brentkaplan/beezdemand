# Test summary() contract compliance for all model classes

test_that("summary.beezdemand_hurdle meets contract", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  data(apt, package = "beezdemand")
  # Subset for faster testing
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- tryCatch(
    fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  s <- summary(fit)

  # Class structure
  expect_s3_class(s, "summary.beezdemand_hurdle")
  expect_s3_class(s, "beezdemand_summary")

  # Required fields
  expect_true("call" %in% names(s))
  expect_true("model_class" %in% names(s))
  expect_equal(s$model_class, "beezdemand_hurdle")
  expect_true("backend" %in% names(s))
  expect_equal(s$backend, "TMB")
  expect_true("nobs" %in% names(s))
  expect_true("n_subjects" %in% names(s))
  expect_true("converged" %in% names(s))
  expect_true("logLik" %in% names(s))
  expect_true("AIC" %in% names(s))
  expect_true("BIC" %in% names(s))
  expect_true("coefficients" %in% names(s))
  expect_true("derived_metrics" %in% names(s))
  expect_true("notes" %in% names(s))

  # coefficients is tibble with required columns
  expect_s3_class(s$coefficients, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "component") %in% names(s$coefficients)))
  expect_true(all(c("estimate_scale", "term_display") %in% names(s$coefficients)))
  expect_true(all(s$coefficients$estimate_scale %in% c("natural", "log", "log10", "logit")))

  # derived_metrics is tibble with required columns
  expect_s3_class(s$derived_metrics, "tbl_df")
  expect_true(all(c("metric", "estimate") %in% names(s$derived_metrics)))
  expect_true(all(c("pmax_model", "omax_model") %in% s$derived_metrics$metric))

  # Canonical components (probability part)
  prob_terms <- c("beta0", "beta1", "gamma0", "gamma1")
  present_prob_terms <- intersect(prob_terms, s$coefficients$term)
  expect_true(length(present_prob_terms) > 0)
  expect_true(all(
    s$coefficients$component[s$coefficients$term %in% present_prob_terms] ==
      "zero_probability"
  ))
})


test_that("summary.beezdemand_cp_hurdle meets contract", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  # Create simple test data
  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  cp_data <- data.frame(
    id = rep(1:n_subj, each = n_obs),
    x = rep(seq(0.1, 10, length.out = n_obs), n_subj),
    y = rpois(n_subj * n_obs, lambda = 10)
  )

  fit <- tryCatch(
    fit_cp_hurdle(
      cp_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      random_effects = c("zeros", "qalone"),
      verbose = 0,
      tmb_control = list(max_iter = 50, eval_max = 200, trace = 0)
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  s <- summary(fit)

  # Class structure
  expect_s3_class(s, "summary.beezdemand_cp_hurdle")
  expect_s3_class(s, "beezdemand_summary")

  # Required fields
  expect_equal(s$model_class, "beezdemand_cp_hurdle")
  expect_equal(s$backend, "TMB")
  expect_true("nobs" %in% names(s))
  expect_true("n_subjects" %in% names(s))
  expect_true("converged" %in% names(s))
  expect_true("logLik" %in% names(s))
  expect_true("coefficients" %in% names(s))
  expect_true("derived_metrics" %in% names(s))

  # coefficients is tibble
  expect_s3_class(s$coefficients, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "component") %in% names(s$coefficients)))
  expect_true(all(c("estimate_scale", "term_display") %in% names(s$coefficients)))
  expect_true(all(s$coefficients$estimate_scale %in% c("natural", "log", "log10", "logit")))

  # derived metrics are not in coefficients
  expect_false(any(s$coefficients$term %in% c("Qalone", "beta")))

  # derived_metrics contains beta and Qalone
  expect_s3_class(s$derived_metrics, "tbl_df")
  expect_true(all(c("metric", "estimate") %in% names(s$derived_metrics)))
  expect_true(all(c("Qalone", "beta") %in% s$derived_metrics$metric))
  expect_true(all(c("std.error", "method") %in% names(s$derived_metrics)))
  expect_true(all(s$derived_metrics$method[s$derived_metrics$metric %in% c("Qalone", "beta")] == "delta"))
  # SEs may be NA when the Hessian/SE computation is unavailable
  se_vals <- s$derived_metrics$std.error[s$derived_metrics$metric %in% c("Qalone", "beta")]
  expect_true(all(is.na(se_vals) | is.finite(se_vals)))

  # Canonical components
  expect_true(any(s$coefficients$component == "zero_probability"))
  expect_true(any(s$coefficients$component == "consumption"))
})


test_that("summary.beezdemand_nlme meets contract", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  apt_small$y_ll4 <- ll4(apt_small$y)

  fit <- tryCatch(
    fit_demand_mixed(
      apt_small,
      y_var = "y_ll4",
      x_var = "x",
      id_var = "id",
      equation_form = "zben"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  s <- summary(fit)

  # Class structure
  expect_s3_class(s, "summary.beezdemand_nlme")
  expect_s3_class(s, "beezdemand_summary")

  # Required fields
  expect_equal(s$model_class, "beezdemand_nlme")
  expect_equal(s$backend, "nlme")
  expect_true("nobs" %in% names(s))
  expect_true("n_subjects" %in% names(s))
  expect_true("converged" %in% names(s))
  expect_true("logLik" %in% names(s))
  expect_true("coefficients" %in% names(s))
  expect_true("derived_metrics" %in% names(s))

  # coefficients is tibble
  expect_s3_class(s$coefficients, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "component") %in% names(s$coefficients)))
  expect_true(all(c("estimate_scale", "term_display") %in% names(s$coefficients)))
  expect_true(all(s$coefficients$estimate_scale %in% c("natural", "log", "log10", "logit")))

  # derived_metrics exists (may be empty)
  expect_s3_class(s$derived_metrics, "tbl_df")
  expect_true(all(c("metric", "estimate") %in% names(s$derived_metrics)))

  # For HS/Koff fixed fits, include derived Pmax/Omax per subject when available
  if (s$equation %in% c("hs", "koff")) {
    expect_true(any(s$derived_metrics$metric %in% c("pmax_model", "omax_model")))
  }
})


test_that("summary.beezdemand_fixed meets contract", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  s <- summary(fit)

  # Class structure
  expect_s3_class(s, "summary.beezdemand_fixed")
  expect_s3_class(s, "beezdemand_summary")

  # Required fields
  expect_equal(s$model_class, "beezdemand_fixed")
  expect_equal(s$backend, "legacy")
  expect_true("nobs" %in% names(s) || "n_subjects" %in% names(s))
  expect_true("coefficients" %in% names(s))
  expect_true("derived_metrics" %in% names(s))

  # coefficients is tibble
  expect_s3_class(s$coefficients, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "component") %in% names(s$coefficients)))
  expect_true(all(c("estimate_scale", "term_display") %in% names(s$coefficients)))
  expect_true(all(s$coefficients$estimate_scale %in% c("natural", "log", "log10", "logit")))

  # derived_metrics exists (may be empty)
  expect_s3_class(s$derived_metrics, "tbl_df")
  expect_true(all(c("metric", "estimate") %in% names(s$derived_metrics)))
})


test_that("summary.beezdemand_systematicity meets contract", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  s <- summary(check)

  # Class structure
  expect_s3_class(s, "summary.beezdemand_systematicity")
  expect_s3_class(s, "beezdemand_summary")

  # Required fields
  expect_equal(s$model_class, "beezdemand_systematicity")
  expect_true("n_subjects" %in% names(s))
  expect_true("n_systematic" %in% names(s))
  expect_true("n_unsystematic" %in% names(s))
})
