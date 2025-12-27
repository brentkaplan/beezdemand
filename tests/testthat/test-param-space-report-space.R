test_that("Spec 2.4: fit objects store param_space + param_space_details (core trio)", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit_fixed <- fit_demand_fixed(apt_small, param_space = "natural")
  expect_true("param_space" %in% names(fit_fixed))
  expect_true("param_space_details" %in% names(fit_fixed))

  skip_on_cran()

  if (requireNamespace("nlme", quietly = TRUE)) {
    apt_small$y_ll4 <- ll4(apt_small$y)
    fit_mixed <- fit_demand_mixed(
      apt_small,
      y_var = "y_ll4",
      x_var = "x",
      id_var = "id",
      param_space = "log10"
    )
    expect_true("param_space" %in% names(fit_mixed))
    expect_true("param_space_details" %in% names(fit_mixed))
  }

  if (requireNamespace("TMB", quietly = TRUE)) {
    fit_hurdle <- fit_demand_hurdle(
      apt_small,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      verbose = 0
    )
    expect_true("param_space" %in% names(fit_hurdle))
    expect_true("param_space_details" %in% names(fit_hurdle))
  }
})


test_that("Spec 2.4: tidy() respects report_space for fixed fits", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "hs", k = 2, param_space = "natural")
  t_nat <- tidy(fit, report_space = "natural")
  t_log <- tidy(fit, report_space = "log10")

  q0_nat <- t_nat[t_nat$term == "Q0", , drop = FALSE]
  q0_log <- t_log[t_log$term == "Q0", , drop = FALSE]
  skip_if(nrow(q0_nat) == 0, "No Q0 term present in tidy output")

  expect_true(all(q0_log$estimate_scale == "log10"))
  expect_true(all(q0_log$term_display == "log10(Q0)"))
  expect_equal(q0_log$estimate, log10(q0_nat$estimate), tolerance = 1e-6)
})


test_that("Spec 2.4: summary()/tidy()/coef() can report hurdle demand params on log10 scale", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- fit_demand_hurdle(
    apt_small,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    verbose = 0,
    tmb_control = list(max_iter = 80, eval_max = 400, trace = 0)
  )
  skip_if(is.null(fit), "Model fitting failed")

  c_int <- coef(fit, report_space = "internal")
  c_log <- coef(fit, report_space = "log10")

  expect_true("log10(Q0)" %in% names(c_log))
  expect_equal(c_log[["log10(Q0)"]], c_int[["logQ0"]] / log(10), tolerance = 1e-6)

  s_log <- summary(fit, report_space = "log10")
  expect_true(any(s_log$coefficients$term == "log10(Q0)"))

  t_log <- tidy(fit, report_space = "log10")
  expect_true(any(t_log$term == "log10(Q0)"))
  expect_true(all(t_log$estimate_scale[t_log$term == "log10(Q0)"] == "log10"))
})


test_that("Spec 2.4: bind_rows() works across tidy outputs (fixed + hurdle)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit_fixed <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  fit_hurdle <- fit_demand_hurdle(
    apt_small,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    verbose = 0,
    tmb_control = list(max_iter = 80, eval_max = 400, trace = 0)
  )
  skip_if(is.null(fit_hurdle), "Model fitting failed")

  tf <- tidy(fit_fixed, report_space = "natural")
  th <- tidy(fit_hurdle, report_space = "natural")

  combined <- dplyr::bind_rows(tf, th)
  expect_s3_class(combined, "tbl_df")
  expect_true("estimate_scale" %in% names(combined))
  expect_true(is.character(combined$estimate_scale) || all(is.na(combined$estimate_scale)))
})

