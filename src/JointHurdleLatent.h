/// @file JointHurdleLatent.h
/// Joint Hurdle Model for Cross-Price Demand - Latent-Trait Variant
/// =============================================================================
///
/// MODEL STRUCTURE:
/// Three streams: alone.target (0), own.target (1), own.alt (2)
///
/// LATENT TRAITS (subject-level):
///   u_buy  - Propensity to buy anything (drives hurdle)
///   u_val  - Baseline consumption intensity
///   u_sens - Price sensitivity
///   u_sub  - Substitution propensity
///
/// These follow: (u_buy, u_val, u_sens, u_sub) ~ MVN(0, Sigma)
///
/// PART I (Shared Hurdle):
///   logit(pi_ij) = gamma0 + gamma_s[stream] + gamma1*log(price_T + epsilon) + u_buy_i
///   - gamma_s[0] = 0 (alone.target is reference)
///   - gamma_s[1] = gamma_own_target
///   - gamma_s[2] = gamma_own_alt
///
/// PART II (Stream-Specific Intensity via Latent Loadings):
///   alone.target:
///     logQ0_AT_i = theta_Q0_AT + u_val_i
///     alpha_AT_i = exp(theta_alpha_AT + u_sens_i)
///     mu = logQ0_AT_i + k*(exp(-alpha_AT_i*price) - 1)
///
///   own.target:
///     logQ0_OT_i = theta_Q0_OT + u_val_i + lambda_sub_q0 * u_sub_i
///     alpha_OT_i = exp(theta_alpha_OT + u_sens_i + lambda_sub_alpha * u_sub_i)
///     mu = logQ0_OT_i + k*(exp(-alpha_OT_i*price) - 1)
///
///   own.alt:
///     logQalone_OA_i = theta_Qalone_OA + u_val_i + lambda_sub_alt * u_sub_i
///     mu = logQalone_OA_i + I*exp(-beta*price)
///
/// KEY DIFFERENCES FROM SATURATED MODEL:
///   - 4 latent traits instead of 4 stream-specific REs
///   - Loadings capture how traits map to parameters
///   - Lower-dimensional, more behaviorally interpretable
///
/// =============================================================================

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type JointHurdleLatent(objective_function<Type>* obj) {
  // =========================================================================
  // DATA
  // =========================================================================
  DATA_VECTOR(price_T);           // Target price (predictor for ALL streams)
  DATA_VECTOR(y);                 // Raw consumption (NOT logged)
  DATA_IVECTOR(stream);           // 0=alone.target, 1=own.target, 2=own.alt
  DATA_IVECTOR(subject_id);       // Subject index (0-based)
  DATA_INTEGER(n_subjects);
  DATA_SCALAR(epsilon);           // Small constant for log(price + epsilon)

  // Stream presence flags
  DATA_INTEGER(has_alone_target);
  DATA_INTEGER(has_own_target);
  DATA_INTEGER(has_own_alt);

  // =========================================================================
  // PART I PARAMETERS (Shared Hurdle)
  // =========================================================================
  PARAMETER(gamma0);              // Intercept (baseline = alone.target)
  PARAMETER(gamma_own_target);    // Offset for own.target stream
  PARAMETER(gamma_own_alt);       // Offset for own.alt stream
  PARAMETER(gamma1);              // Slope on log(price_T + epsilon)

  // =========================================================================
  // PART II FIXED EFFECT PARAMETERS (Population Means)
  // =========================================================================
  // Target demand - alone
  PARAMETER(theta_Q0_AT);         // Population log baseline (alone.target)
  PARAMETER(theta_alpha_AT);      // Population log-alpha (alone.target)

  // Target demand - own
  PARAMETER(theta_Q0_OT);         // Population log baseline (own.target)
  PARAMETER(theta_alpha_OT);      // Population log-alpha (own.target)

  // Cross-price - alt
  PARAMETER(theta_Qalone_OA);     // Population log baseline (own.alt)
  PARAMETER(I);                   // Cross-price interaction intensity
  PARAMETER(log_beta);            // log(beta) to ensure beta > 0

  // Global k
  PARAMETER(k);                   // GLOBAL k (shared across target streams)

  // =========================================================================
  // LATENT TRAIT LOADINGS
  // =========================================================================
  // These capture how u_sub differentially affects own/alt vs alone
  PARAMETER(lambda_sub_q0);       // u_sub loading on own.target Q0
  PARAMETER(lambda_sub_alpha);    // u_sub loading on own.target alpha
  PARAMETER(lambda_sub_alt);      // u_sub loading on own.alt Qalone

  // =========================================================================
  // VARIANCE PARAMETERS (Latent Trait Covariance)
  // =========================================================================
  // Log standard deviations for each latent trait
  PARAMETER(logsigma_buy);
  PARAMETER(logsigma_val);
  PARAMETER(logsigma_sens);
  PARAMETER(logsigma_sub);

  // Correlations among latent traits (6 pairwise correlations)
  PARAMETER(rho_buy_val_raw);
  PARAMETER(rho_buy_sens_raw);
  PARAMETER(rho_buy_sub_raw);
  PARAMETER(rho_val_sens_raw);
  PARAMETER(rho_val_sub_raw);
  PARAMETER(rho_sens_sub_raw);

  // Residual variance
  PARAMETER(logsigma_e);

  // =========================================================================
  // RANDOM EFFECTS (standardized latent traits)
  // =========================================================================
  // u: n_subjects x 4 matrix (standardized)
  // Column 0: u_buy (propensity to buy)
  // Column 1: u_val (baseline intensity)
  // Column 2: u_sens (price sensitivity)
  // Column 3: u_sub (substitution propensity)
  PARAMETER_MATRIX(u);

  // =========================================================================
  // TRANSFORM PARAMETERS
  // =========================================================================
  Type beta_param = exp(log_beta);  // Ensure beta > 0

  Type sigma_buy = exp(logsigma_buy);
  Type sigma_val = exp(logsigma_val);
  Type sigma_sens = exp(logsigma_sens);
  Type sigma_sub = exp(logsigma_sub);
  Type sigma_e = exp(logsigma_e);

  // Transform correlations from (-inf, inf) to (-1, 1)
  Type rho_buy_val = tanh(rho_buy_val_raw);
  Type rho_buy_sens = tanh(rho_buy_sens_raw);
  Type rho_buy_sub = tanh(rho_buy_sub_raw);
  Type rho_val_sens = tanh(rho_val_sens_raw);
  Type rho_val_sub = tanh(rho_val_sub_raw);
  Type rho_sens_sub = tanh(rho_sens_sub_raw);

  // =========================================================================
  // BUILD 4x4 COVARIANCE MATRIX FOR LATENT TRAITS
  // =========================================================================
  matrix<Type> Sigma(4, 4);

  // Diagonal (variances)
  Sigma(0, 0) = sigma_buy * sigma_buy;
  Sigma(1, 1) = sigma_val * sigma_val;
  Sigma(2, 2) = sigma_sens * sigma_sens;
  Sigma(3, 3) = sigma_sub * sigma_sub;

  // Off-diagonal (covariances)
  Sigma(0, 1) = sigma_buy * sigma_val * rho_buy_val;
  Sigma(1, 0) = Sigma(0, 1);

  Sigma(0, 2) = sigma_buy * sigma_sens * rho_buy_sens;
  Sigma(2, 0) = Sigma(0, 2);

  Sigma(0, 3) = sigma_buy * sigma_sub * rho_buy_sub;
  Sigma(3, 0) = Sigma(0, 3);

  Sigma(1, 2) = sigma_val * sigma_sens * rho_val_sens;
  Sigma(2, 1) = Sigma(1, 2);

  Sigma(1, 3) = sigma_val * sigma_sub * rho_val_sub;
  Sigma(3, 1) = Sigma(1, 3);

  Sigma(2, 3) = sigma_sens * sigma_sub * rho_sens_sub;
  Sigma(3, 2) = Sigma(2, 3);

  // Cholesky decomposition
  matrix<Type> L = Sigma.llt().matrixL();

  // =========================================================================
  // TRANSFORM LATENT TRAITS TO ACTUAL SCALE
  // =========================================================================
  matrix<Type> traits(n_subjects, 4);
  for (int i = 0; i < n_subjects; i++) {
    vector<Type> u_std(4);
    u_std(0) = u(i, 0);
    u_std(1) = u(i, 1);
    u_std(2) = u(i, 2);
    u_std(3) = u(i, 3);
    vector<Type> trait_i = L * u_std;
    traits(i, 0) = trait_i(0);  // u_buy
    traits(i, 1) = trait_i(1);  // u_val
    traits(i, 2) = trait_i(2);  // u_sens
    traits(i, 3) = trait_i(3);  // u_sub
  }

  // =========================================================================
  // NEGATIVE LOG-LIKELIHOOD
  // =========================================================================
  Type nll = Type(0.0);
  int n = price_T.size();
  Type log2pi = log(Type(2.0) * M_PI);

  // Prior on random effects (standard normal)
  for (int i = 0; i < n_subjects; i++) {
    for (int j = 0; j < 4; j++) {
      nll -= dnorm(u(i, j), Type(0.0), Type(1.0), true);
    }
  }

  // =========================================================================
  // DATA LIKELIHOOD
  // =========================================================================
  for (int i = 0; i < n; i++) {
    int subj = subject_id(i);
    int s = stream(i);
    Type y_i = y(i);
    Type p_i = price_T(i);

    // Extract latent traits for this subject
    Type u_buy_i = traits(subj, 0);
    Type u_val_i = traits(subj, 1);
    Type u_sens_i = traits(subj, 2);
    Type u_sub_i = traits(subj, 3);

    // -----------------------------------------------------------------
    // PART I: Hurdle (probability of zero)
    // -----------------------------------------------------------------
    // Stream-specific intercept offset
    Type gamma_s;
    if (s == 0) {
      gamma_s = Type(0.0);  // alone.target is reference
    } else if (s == 1) {
      gamma_s = gamma_own_target;
    } else {
      gamma_s = gamma_own_alt;
    }

    // Hurdle linear predictor: u_buy drives propensity to buy
    Type eta = gamma0 + gamma_s + gamma1 * log(p_i + epsilon) + u_buy_i;
    Type exp_eta = exp(eta);
    Type pi_ij = exp_eta / (Type(1.0) + exp_eta);  // P(y = 0)

    // -----------------------------------------------------------------
    // Check if y is zero
    // -----------------------------------------------------------------
    if (y_i == Type(0.0)) {
      // Zero consumption: contributes to Part I only
      nll -= log(pi_ij);
    } else {
      // Positive consumption: contributes to both parts
      nll -= log(Type(1.0) - pi_ij);

      // Compute log(y) only for positive values
      Type logQ_i = log(y_i);

      // -----------------------------------------------------------------
      // PART II: Stream-specific intensity via latent traits
      // -----------------------------------------------------------------
      Type mu_ij;

      if (s == 0) {
        // alone.target: baseline demand
        // u_val affects intensity, u_sens affects price sensitivity
        Type logQ0_AT_i = theta_Q0_AT + u_val_i;
        Type alpha_AT_i = exp(theta_alpha_AT + u_sens_i);

        // EXACT mean function from HurdleDemand2RE.h
        mu_ij = logQ0_AT_i + k * (exp(-alpha_AT_i * p_i) - Type(1.0));

      } else if (s == 1) {
        // own.target: demand with alternative present
        // u_sub captures substitution effect on baseline and sensitivity
        Type logQ0_OT_i = theta_Q0_OT + u_val_i + lambda_sub_q0 * u_sub_i;
        Type alpha_OT_i = exp(theta_alpha_OT + u_sens_i + lambda_sub_alpha * u_sub_i);

        // EXACT mean function from HurdleDemand2RE.h
        mu_ij = logQ0_OT_i + k * (exp(-alpha_OT_i * p_i) - Type(1.0));

      } else {
        // own.alt: Cross-price model
        // u_sub captures how substitution affects alternative consumption
        Type logQalone_OA_i = theta_Qalone_OA + u_val_i + lambda_sub_alt * u_sub_i;

        // EXACT mean function from HurdleCrossPrice2RE.h
        mu_ij = logQalone_OA_i + I * exp(-beta_param * p_i);
      }

      // Gaussian likelihood for log(y)
      Type resid = (logQ_i - mu_ij) / sigma_e;
      nll -= -logsigma_e - Type(0.5) * log2pi - Type(0.5) * resid * resid;
    }
  }

  // =========================================================================
  // ADREPORT: Derived quantities
  // =========================================================================
  ADREPORT(beta_param);

  // Latent trait SDs
  ADREPORT(sigma_buy);
  ADREPORT(sigma_val);
  ADREPORT(sigma_sens);
  ADREPORT(sigma_sub);
  ADREPORT(sigma_e);

  // Latent trait correlations
  ADREPORT(rho_buy_val);
  ADREPORT(rho_buy_sens);
  ADREPORT(rho_buy_sub);
  ADREPORT(rho_val_sens);
  ADREPORT(rho_val_sub);
  ADREPORT(rho_sens_sub);

  // Latent traits (for subject-level predictions)
  ADREPORT(traits);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
