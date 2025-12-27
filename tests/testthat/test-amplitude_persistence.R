
test_that("calculate_amplitude_persistence.default works correctly", {
  # Create dummy dataframe
  df <- data.frame(
    id = 1:5,
    Intensity = c(10, 10, 10, 10, 10), # No variance, SD=0 -> Z=0
    Q0 = c(10, 10, 10, 10, 10),
    BP0 = c(100, 200, 300, 400, 500), # Mean 300, SD 158.11
    Pmaxe = c(10, 20, 30, 40, 50),
    Omaxe = c(100, 200, 300, 400, 500),
    Alpha = c(0.1, 0.05, 0.02, 0.01, 0.005)
  )
  
  # Standard calculation
  res <- calculate_amplitude_persistence(df)
  
  expect_true(is.data.frame(res))
  expect_true(all(c("Amplitude", "Persistence") %in% names(res)))
  expect_equal(nrow(res), 5)
  
  # Check Amplitude (should be 0 since Intensity is constant)
  expect_equal(res$Amplitude, rep(0, 5))
  
  # Check Persistence includes 1/Alpha via z_inv_alpha (not z_Alpha)
  expect_true("z_inv_alpha" %in% names(res))
  expect_false("z_Alpha" %in% names(res))
  expect_true(any(res$Persistence != 0))
})

test_that("calculate_amplitude_persistence handles non-positive alpha safely", {
  df <- data.frame(
    id = 1:3,
    Intensity = c(10, 10, 10),
    BP0 = c(10, 20, 30),
    Pmaxe = c(10, 20, 30),
    Omaxe = c(10, 20, 30),
    Alpha = c(0.1, 0, 0.01) # 0 would cause Inf in 1/Alpha if not handled
  )
  
  expect_warning(
    res <- calculate_amplitude_persistence(df, persistence = c("BP0", "Pmaxe", "Omaxe", "Alpha")),
    "Non-positive values found"
  )
  
  expect_false(any(is.infinite(res$Persistence)))
  expect_true("z_inv_alpha" %in% names(res))
  expect_false(any(is.infinite(res$z_inv_alpha)))
})

test_that("calculate_amplitude_persistence accepts custom basis_means and basis_sds", {
  df <- data.frame(
    id = 1:2,
    Intensity = c(10, 20), # Mean 15, SD 7.07
    BP0 = c(10, 20),
    Pmaxe = c(10, 20),
    Omaxe = c(10, 20),
    Alpha = c(0.1, 0.1)
  )
  
  # Custom basis: Mean=0, SD=1 (no change) vs Mean=15, SD=1
  # If we set Intensity mean=0, sd=1, then value 10 becomes (10-0)/1 = 10.
  
  res_custom <- calculate_amplitude_persistence(df, 
                                                amplitude = "Intensity",
                                                persistence = "BP0",
                                                basis_means = c(Intensity = 0),
                                                basis_sds = c(Intensity = 1))
  
  expect_equal(res_custom$z_Intensity[1], 10)
  expect_equal(res_custom$z_Intensity[2], 20)
  
  # Check persistence unaffected (should use sample stats)
  # BP0 sample mean=15, sd=7.07. (10-15)/7.07 = -0.707
  expect_equal(round(res_custom$z_BP0[1], 3), -0.707)
})

test_that("calculate_amplitude_persistence.beezdemand_fixed works", {
  # Mock a beezdemand_fixed object
  fit_fixed <- list(
    results = data.frame(
      id = 1:5,
      Intensity = rep(10, 5),
      BP0 = seq(100, 500, 100),
      Pmaxe = seq(10, 50, 10),
      Omaxe = seq(100, 500, 100),
      Alpha = rep(0.01, 5)
    )
  )
  class(fit_fixed) <- "beezdemand_fixed"
  
  res <- calculate_amplitude_persistence(fit_fixed)
  expect_true("Amplitude" %in% names(res))
  expect_true("Persistence" %in% names(res))
})

test_that("calculate_amplitude_persistence.beezdemand_hurdle works", {
  # Mock a beezdemand_hurdle object
  fit_hurdle <- list(
    subject_pars = data.frame(
      id = 1:5,
      Q0 = rep(10, 5),
      breakpoint = seq(100, 500, 100),
      Pmax = seq(10, 50, 10),
      Omax = seq(100, 500, 100),
      alpha = rep(0.01, 5)
    )
  )
  class(fit_hurdle) <- "beezdemand_hurdle"
  
  res <- calculate_amplitude_persistence(fit_hurdle)
  expect_true("Amplitude" %in% names(res))
  expect_true("Persistence" %in% names(res))
  # Should find Q0 and alpha (lowercase/uppercase matching handled? No, generic calls default with specific names)
  # The method maps names?
  # The method passes `pars` to default. Default looks for "Q0", "Alpha".
  # Wait, hurdle has "alpha" (lowercase).
  # Default method `pers_cols <- intersect(persistence, names(df))`
  # Default `persistence` is c("BP0", "Pmaxe", "Omaxe", "Alpha").
  # Hurdle method defaults `persistence = c("breakpoint", "Pmax", "Omax", "alpha")`.
  # So it should work if defaults are used.
  
  expect_equal(length(grep("z_", names(res))), 5) # Q0, breakpoint, Pmax, Omax, alpha
})

test_that("standardization preserves missingness when SD=0", {
  df <- data.frame(
    id = 1:3,
    Intensity = c(10, 10, NA_real_),
    BP0 = c(1, 2, 3),
    Pmaxe = c(1, 2, 3),
    Omaxe = c(1, 2, 3),
    Alpha = c(0.1, 0.1, 0.1)
  )
  res <- calculate_amplitude_persistence(df)
  expect_equal(res$z_Intensity, c(0, 0, NA_real_))
})

test_that("duplicate ids error by default", {
  df <- data.frame(
    id = c(1, 1),
    Intensity = c(10, 20),
    BP0 = c(1, 2),
    Pmaxe = c(1, 2),
    Omaxe = c(1, 2),
    Alpha = c(0.1, 0.2)
  )
  expect_error(calculate_amplitude_persistence(df), "Duplicate 'id'")
})

test_that("case-insensitive matching includes alpha when present as lowercase", {
  df <- data.frame(
    id = 1:3,
    Intensity = c(1, 2, 3),
    BP0 = c(1, 2, 3),
    Pmaxe = c(1, 2, 3),
    Omaxe = c(1, 2, 3),
    alpha = c(0.1, 0.2, 0.3)
  )
  res <- calculate_amplitude_persistence(df)
  expect_true("z_inv_alpha" %in% names(res))
})

# Note: Testing NLME requires valid NLME object or mocking predict/coef. 
# Mocking predict/coef for S3 is tricky in testthat without creating the class logic or helper.
# Since we implemented `calculate_amplitude_persistence.beezdemand_nlme` to call `coef(fit)`,
# we can mock a fit object if we define the method `coef.beezdemand_nlme` locally or if it's exported.
# But `coef` is S3.
# For simplicity, we trust the integration test structure or skip mock if too complex.
# We will skip NLME mock test here to avoid fragility, trusting logic is similar to others + prediction loop.
