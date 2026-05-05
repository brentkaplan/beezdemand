# =============================================================================
# Golden regression baselines for Hurdle Part II template consolidation
# (TICKET-011 Phase 4 prep).
#
# These tests pin the loglik, free coefficients, and select subject_pars
# values for all 6 current template x fit combinations:
#   2RE x {zhao_exponential, exponential, simplified_exponential}
#   3RE x {zhao_exponential, exponential, simplified_exponential}
#
# Captured from the per-variant template files
#   src/HurdleDemand{2,3}RE{,_StdQ0,_SND}.h
# at branch feat/ticket-011-phase-4 (HEAD 87498f5).
#
# When the Phase 4 consolidation rewrites these into a single
# HurdleDemand.h with n_re / equation_type flags, the same fits MUST
# reproduce these reference values to 1e-8 (loglik, coefficients) and
# 1e-6 (subject_pars).
#
# Tolerances rationale:
# - 1e-8 on loglik / coefs: tight enough to catch numerical drift from
#   the parameter-mapping refactor (factor(NA) on logsigma_c etc.) but
#   loose enough to absorb sub-1ulp floating-point noise across runs.
# - 1e-6 on subject_pars: looser because Q0/alpha pass through
#   exp(log_q0 + b_i) where b_i is a per-subject random-effect
#   estimate, so empirical Bayes posterior modes can drift slightly.
# =============================================================================

# Reuse the simulator from test-hurdle_part2_variants.R
source(testthat::test_path("test-hurdle_part2_variants.R"))

.golden_specs <- list(
  list(
    label   = "2RE x zhao_exponential",
    part2   = "zhao_exponential",
    re      = c("zeros", "q0"),
    seed    = 1001,
    sim_part2 = "exponential",  # simulator's "exponential" is HS-stdQ0;
                                # for zhao the data-generating choice is
                                # cosmetic for fitting purposes since the
                                # likelihood is fit-side, not data-side.
    loglik  = -340.5353673593,
    coefs   = c(beta0 = -3.2608299381, beta1 = 2.2457529053,
                log_q0 = 0.0044590720, log_k = 1.3010593371,
                log_alpha = -0.0790836575,
                logsigma_a = 0.0296944803, logsigma_b = -1.5138344846,
                logsigma_e = -1.0427901714, rho_ab_raw = -1.0862209549),
    subj1_Q0 = 0.9083741459, subj1_alpha = 0.9239626248
  ),
  list(
    label   = "2RE x exponential (HS-stdQ0)",
    part2   = "exponential",
    re      = c("zeros", "q0"),
    seed    = 1002,
    sim_part2 = "exponential",
    loglik  = -119.4719033459,
    coefs   = c(beta0 = -3.1777056074, beta1 = 2.0446034545,
                log_q0 = 0.0775476179, log_k = 1.3851701783,
                log_alpha = -0.2211052175,
                logsigma_a = -0.1108615470, logsigma_b = -0.6002558063,
                logsigma_e = -1.8509588725, rho_ab_raw = 0.1551914618),
    subj1_Q0 = 0.6618232021, subj1_alpha = 0.8016323301
  ),
  list(
    label   = "2RE x simplified_exponential (SND)",
    part2   = "simplified_exponential",
    re      = c("zeros", "q0"),
    seed    = 1003,
    sim_part2 = "simplified_exponential",
    loglik  = -149.2237042818,
    coefs   = c(beta0 = -2.4991824041, beta1 = 1.3903243615,
                log_q0 = -0.0520238751, log_alpha = -0.2143593283,
                logsigma_a = -0.4640857339, logsigma_b = -0.5552882129,
                logsigma_e = -1.9050477049, rho_ab_raw = -0.0016253404),
    subj1_Q0 = 0.8970923084, subj1_alpha = 0.8070583340
  ),
  list(
    label   = "3RE x zhao_exponential",
    part2   = "zhao_exponential",
    re      = c("zeros", "q0", "alpha"),
    seed    = 1004,
    sim_part2 = "exponential",
    loglik  = -99.0715423248,
    coefs   = c(beta0 = -3.0184472299, beta1 = 1.9523953209,
                log_q0 = 0.0128191621, log_k = 1.3827678293,
                log_alpha = -0.2059602455,
                logsigma_a = -0.4463446276, logsigma_b = -0.7013461509,
                logsigma_c = -0.6538682336, logsigma_e = -1.9292058614,
                rho_ab_raw = -0.2793099198, rho_ac_raw = -0.2666453020,
                rho_bc_raw = 3.9635691686),
    subj1_Q0 = 0.6381394182, subj1_alpha = 0.5068510927
  ),
  list(
    label   = "3RE x exponential (HS-stdQ0)",
    part2   = "exponential",
    re      = c("zeros", "q0", "alpha"),
    seed    = 1005,
    sim_part2 = "exponential",
    loglik  = -115.6846939936,
    coefs   = c(beta0 = -3.2217280729, beta1 = 2.2706315419,
                log_q0 = 0.0213830706, log_k = 1.3904884299,
                log_alpha = -0.2250562827,
                logsigma_a = -0.7418433401, logsigma_b = -0.2252305145,
                logsigma_c = -3.2523343096, logsigma_e = -1.9537008927,
                rho_ab_raw = -0.7297248441, rho_ac_raw = 1.5598768919,
                rho_bc_raw = 0.5851825360),
    subj1_Q0 = 0.6617956973, subj1_alpha = 0.8446557765
  ),
  list(
    label   = "3RE x simplified_exponential (SND)",
    part2   = "simplified_exponential",
    re      = c("zeros", "q0", "alpha"),
    seed    = 1006,
    sim_part2 = "simplified_exponential",
    loglik  = -702.0182039330,
    coefs   = c(beta0 = -2.8024734479, beta1 = 1.8676003961,
                log_q0 = -0.3112283687, log_alpha = -0.2081516745,
                logsigma_a = -0.4022675493, logsigma_b = 0.1516937916,
                logsigma_c = -1.8772338128, logsigma_e = -0.0524277639,
                rho_ab_raw = 0.1423936878, rho_ac_raw = 0.1232528799,
                rho_bc_raw = -9.5715063483),
    subj1_Q0 = 1.9998625594, subj1_alpha = 0.7058565075
  )
)

for (spec in .golden_specs) {
  local({
    s <- spec  # capture per-iteration

    test_that(sprintf("Hurdle golden baseline: %s", s$label), {
      skip_on_cran()
      skip_if_not_installed("TMB")

      sim_data <- simulate_hurdle_part2_data(
        n_subjects = 40,
        prices = seq(0, 5, by = 0.5),
        part2 = s$sim_part2,
        seed = s$seed
      )

      fit <- suppressWarnings(fit_demand_hurdle(
        sim_data,
        y_var = "y", x_var = "x", id_var = "id",
        random_effects = s$re,
        part2 = s$part2,
        verbose = 0
      ))

      expect_true(isTRUE(fit$converged),
                  info = sprintf("%s convergence", s$label))

      # Loglik: tightest gate. Any drift here means the consolidation
      # changed the likelihood surface.
      expect_equal(as.numeric(logLik(fit)), s$loglik, tolerance = 1e-8,
                   info = sprintf("%s loglik", s$label))

      # Free coefficients: per-template coef vector (parameters that
      # were optimized, NOT mapped to NA). For the consolidated
      # template, MakeADFun(map = list(...)) must produce a $par
      # vector whose elements match these reference values.
      coefs_fit <- fit$model$coefficients
      for (nm in names(s$coefs)) {
        expect_true(nm %in% names(coefs_fit),
                    info = sprintf("%s missing coef %s", s$label, nm))
        expect_equal(unname(coefs_fit[nm]), s$coefs[[nm]], tolerance = 1e-7,
                     info = sprintf("%s coef %s", s$label, nm))
      }

      # subject_pars first-row Q0/alpha: looser tolerance (1e-6) because
      # these pass through exp(log_q0 + b_i) where b_i is an empirical
      # Bayes posterior mode, more sensitive to optimizer trajectory.
      spars1 <- fit$subject_pars[1, ]
      expect_equal(spars1$Q0, s$subj1_Q0, tolerance = 1e-6,
                   info = sprintf("%s subj1$Q0", s$label))
      expect_equal(spars1$alpha, s$subj1_alpha, tolerance = 1e-6,
                   info = sprintf("%s subj1$alpha", s$label))
    })
  })
}
