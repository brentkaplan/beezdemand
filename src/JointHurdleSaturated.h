/// @file JointHurdleSaturated.h
/// Joint Hurdle Model for Cross-Price Demand - Saturated Variant
/// =============================================================================
///
/// MODEL STRUCTURE:
/// Three streams: alone.target (0), own.target (1), own.alt (2)
///
/// PART I (Shared Hurdle):
///   logit(pi_ij) = gamma0 + gamma_s[stream] + gamma1*log(price_T + epsilon) + a_i
///   - gamma_s[0] = 0 (alone.target is reference)
///   - gamma_s[1] = gamma_own_target
///   - gamma_s[2] = gamma_own_alt
///   - Single shared RE a_i across all streams
///
/// PART II (Stream-Specific Intensity):
///   alone.target: log(Q) = (logQ0_AT + b_AT_i) + k*(exp(-alpha_AT*price_T) - 1) + e
///   own.target:   log(Q) = (logQ0_OT + b_OT_i) + k*(exp(-alpha_OT*price_T) - 1) + e
///   own.alt:      log(Q) = (logQalone_OA + b_OA_i) + I*exp(-beta*price_T) + e
///
/// RANDOM EFFECTS:
///   Block-diagonal covariance:
///   - a_i: zeros RE (independent)
///   - (b_AT_i, b_OT_i, b_OA_i): intensity REs (correlated 3x3 block)
///
/// =============================================================================

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type JointHurdleSaturated(objective_function<Type>* obj) {
  // =========================================================================
  // DATA
  // =========================================================================
  DATA_VECTOR(price_T);           // Target price (predictor for ALL streams)
  DATA_VECTOR(y);                 // Raw consumption (NOT logged)
  DATA_IVECTOR(stream);           // 0=alone.target, 1=own.target, 2=own.alt
  DATA_IVECTOR(subject_id);       // Subject index (0-based)
  DATA_INTEGER(n_subjects);
  DATA_SCALAR(epsilon);           // Small constant for log(price + epsilon)

  // Stream presence flags (1 if stream has any observations, 0 otherwise)
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
  // PART II PARAMETERS - Target Demand (alone.target, own.target)
  // =========================================================================
  PARAMETER(logQ0_AT);            // alone.target: log baseline consumption
  PARAMETER(alpha_AT);            // alone.target: price sensitivity
  PARAMETER(logQ0_OT);            // own.target: log baseline consumption
  PARAMETER(alpha_OT);            // own.target: price sensitivity
  PARAMETER(k);                   // GLOBAL k (shared across target streams)

  // =========================================================================
  // PART II PARAMETERS - Cross-Price (own.alt)
  // =========================================================================
  PARAMETER(logQalone_OA);        // own.alt: log baseline when target absent
  PARAMETER(I);                   // Cross-price interaction intensity
  PARAMETER(log_beta);            // log(beta) to ensure beta > 0

  // =========================================================================
  // VARIANCE PARAMETERS
  // =========================================================================
  // Zeros RE variance
  PARAMETER(logsigma_a);

  // Intensity RE variances
  PARAMETER(logsigma_b_AT);
  PARAMETER(logsigma_b_OT);
  PARAMETER(logsigma_b_OA);

  // Intensity RE correlations (within 3x3 block)
  PARAMETER(rho_AT_OT_raw);
  PARAMETER(rho_AT_OA_raw);
  PARAMETER(rho_OT_OA_raw);

  // Residual variance
  PARAMETER(logsigma_e);

  // =========================================================================
  // RANDOM EFFECTS (standardized)
  // =========================================================================
  // u: n_subjects x 4 matrix
  // Column 0: a_i (zeros)
  // Column 1: b_AT_i (alone.target intensity)
  // Column 2: b_OT_i (own.target intensity)
  // Column 3: b_OA_i (own.alt intensity)
  PARAMETER_MATRIX(u);

  // =========================================================================
  // TRANSFORM PARAMETERS
  // =========================================================================
  Type beta_param = exp(log_beta);  // Ensure beta > 0

  Type sigma_a = exp(logsigma_a);
  Type sigma_b_AT = exp(logsigma_b_AT);
  Type sigma_b_OT = exp(logsigma_b_OT);
  Type sigma_b_OA = exp(logsigma_b_OA);
  Type sigma_e = exp(logsigma_e);

  // Transform correlations from (-inf, inf) to (-1, 1)
  Type rho_AT_OT = tanh(rho_AT_OT_raw);
  Type rho_AT_OA = tanh(rho_AT_OA_raw);
  Type rho_OT_OA = tanh(rho_OT_OA_raw);

  // =========================================================================
  // BUILD COVARIANCE MATRICES
  // =========================================================================
  // Zeros RE: scalar variance (independent)
  Type var_a = sigma_a * sigma_a;

  // Intensity REs: 3x3 correlation matrix
  matrix<Type> Sigma_b(3, 3);
  Sigma_b(0, 0) = sigma_b_AT * sigma_b_AT;
  Sigma_b(1, 1) = sigma_b_OT * sigma_b_OT;
  Sigma_b(2, 2) = sigma_b_OA * sigma_b_OA;
  Sigma_b(0, 1) = sigma_b_AT * sigma_b_OT * rho_AT_OT;
  Sigma_b(1, 0) = Sigma_b(0, 1);
  Sigma_b(0, 2) = sigma_b_AT * sigma_b_OA * rho_AT_OA;
  Sigma_b(2, 0) = Sigma_b(0, 2);
  Sigma_b(1, 2) = sigma_b_OT * sigma_b_OA * rho_OT_OA;
  Sigma_b(2, 1) = Sigma_b(1, 2);

  // Cholesky decomposition for intensity REs
  matrix<Type> L_b = Sigma_b.llt().matrixL();

  // =========================================================================
  // TRANSFORM RANDOM EFFECTS
  // =========================================================================
  // a_i: simple scaling
  vector<Type> a(n_subjects);
  for (int i = 0; i < n_subjects; i++) {
    a(i) = sigma_a * u(i, 0);
  }

  // b_i: transform through Cholesky
  matrix<Type> b(n_subjects, 3);
  for (int i = 0; i < n_subjects; i++) {
    vector<Type> u_b(3);
    u_b(0) = u(i, 1);
    u_b(1) = u(i, 2);
    u_b(2) = u(i, 3);
    vector<Type> b_i = L_b * u_b;
    b(i, 0) = b_i(0);  // b_AT_i
    b(i, 1) = b_i(1);  // b_OT_i
    b(i, 2) = b_i(2);  // b_OA_i
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

    // Shared random effect for zeros
    Type a_i = a(subj);

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

    Type eta = gamma0 + gamma_s + gamma1 * log(p_i + epsilon) + a_i;
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
      // PART II: Stream-specific intensity model
      // -----------------------------------------------------------------
      Type mu_ij;

      if (s == 0) {
        // alone.target: Zhao demand model
        // EXACT form from HurdleDemand2RE.h
        Type b_AT_i = b(subj, 0);
        mu_ij = (logQ0_AT + b_AT_i) + k * (exp(-alpha_AT * p_i) - Type(1.0));

      } else if (s == 1) {
        // own.target: Zhao demand model (different params)
        // EXACT form from HurdleDemand2RE.h
        Type b_OT_i = b(subj, 1);
        mu_ij = (logQ0_OT + b_OT_i) + k * (exp(-alpha_OT * p_i) - Type(1.0));

      } else {
        // own.alt: Cross-price model
        // EXACT form from HurdleCrossPrice2RE.h
        Type b_OA_i = b(subj, 2);
        mu_ij = (logQalone_OA + b_OA_i) + I * exp(-beta_param * p_i);
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

  // Variances
  ADREPORT(var_a);
  Type var_b_AT = sigma_b_AT * sigma_b_AT;
  Type var_b_OT = sigma_b_OT * sigma_b_OT;
  Type var_b_OA = sigma_b_OA * sigma_b_OA;
  Type var_e = sigma_e * sigma_e;
  ADREPORT(var_b_AT);
  ADREPORT(var_b_OT);
  ADREPORT(var_b_OA);
  ADREPORT(var_e);

  // Correlations
  ADREPORT(rho_AT_OT);
  ADREPORT(rho_AT_OA);
  ADREPORT(rho_OT_OA);

  // Standard deviations
  ADREPORT(sigma_a);
  ADREPORT(sigma_b_AT);
  ADREPORT(sigma_b_OT);
  ADREPORT(sigma_b_OA);
  ADREPORT(sigma_e);

  // Random effects (for subject-level predictions)
  ADREPORT(a);
  ADREPORT(b);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
