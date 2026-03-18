/// @file MixedDemand.h
/// Continuous-Only Mixed Effects Demand Model via TMB
/// =============================================================================
///
/// MODEL STRUCTURE:
/// A single continuous-response model with design-matrix covariates on Q0 and
/// alpha, plus random effects on Q0 and optionally alpha.
///
/// Supports four equation types (runtime branching via eqn_type):
///   0 = exponential (HS):    Gaussian on log(Q)
///   1 = exponentiated (Koff): Gaussian on raw Q
///   2 = simplified:           Gaussian on raw Q (no k)
///   3 = zben:                 Gaussian on LL4(Q) (no k)
///
/// Random effects: n_re = 1 (Q0 only) or n_re = 2 (Q0 + alpha)
/// Covariates: design matrices X_q0, X_alpha for fixed effects
///
/// =============================================================================

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type MixedDemand(objective_function<Type>* obj) {
  // DATA
  DATA_VECTOR(y);            // Response (log(Q), raw Q, or LL4(Q) depending on eqn_type)
  DATA_VECTOR(price);        // Price vector
  DATA_IVECTOR(subject_id);  // 0-indexed subject IDs
  DATA_INTEGER(n_subjects);  // Number of subjects
  DATA_MATRIX(X_q0);         // Design matrix for Q0 (n_obs x p_q0)
  DATA_MATRIX(X_alpha);      // Design matrix for alpha (n_obs x p_alpha)
  DATA_INTEGER(eqn_type);    // 0=exponential, 1=exponentiated, 2=simplified, 3=zben
  DATA_INTEGER(n_re);        // 1 or 2

  // FIXED EFFECT PARAMETERS
  PARAMETER_VECTOR(beta_q0);    // Q0 coefficients (p_q0)
  PARAMETER_VECTOR(beta_alpha); // alpha coefficients (p_alpha)
  PARAMETER(log_k);             // k scaling parameter (mapped out for simplified/zben)

  // VARIANCE PARAMETERS (log-scale)
  PARAMETER(logsigma_b);    // Q0 RE SD
  PARAMETER(logsigma_c);    // alpha RE SD (mapped out if n_re==1)
  PARAMETER(logsigma_e);    // Residual SD

  // Correlation parameter (mapped out if n_re==1)
  PARAMETER(rho_bc_raw);

  // RANDOM EFFECTS (standardized)
  PARAMETER_MATRIX(u);      // n_subjects x n_re

  // TRANSFORM PARAMETERS
  Type k = exp(log_k);
  Type sigma_b = exp(logsigma_b);
  Type sigma_e = exp(logsigma_e);

  // BUILD COVARIANCE MATRIX AND TRANSFORM RANDOM EFFECTS
  matrix<Type> re(n_subjects, n_re);

  if (n_re == 2) {
    Type sigma_c = exp(logsigma_c);
    Type rho_bc = tanh(rho_bc_raw);

    Type var_b = sigma_b * sigma_b;
    Type var_c = sigma_c * sigma_c;
    Type cov_bc = sigma_b * sigma_c * rho_bc;

    // Build 2x2 covariance matrix
    matrix<Type> Sigma(2, 2);
    Sigma(0, 0) = var_b;
    Sigma(0, 1) = cov_bc;
    Sigma(1, 0) = cov_bc;
    Sigma(1, 1) = var_c;

    matrix<Type> L = Sigma.llt().matrixL();

    for (int i = 0; i < n_subjects; i++) {
      vector<Type> u_i = u.row(i);
      vector<Type> re_i = L * u_i;
      re(i, 0) = re_i(0);
      re(i, 1) = re_i(1);
    }

    // ADREPORT variance components
    ADREPORT(sigma_c);
    ADREPORT(var_b);
    ADREPORT(var_c);
    ADREPORT(cov_bc);
    ADREPORT(rho_bc);
  } else {
    // n_re == 1: only Q0 random effect
    for (int i = 0; i < n_subjects; i++) {
      re(i, 0) = sigma_b * u(i, 0);
    }

    Type var_b = sigma_b * sigma_b;
    ADREPORT(var_b);
  }

  // NEGATIVE LOG-LIKELIHOOD
  Type nll = Type(0.0);
  int n = y.size();
  Type log2pi = log(Type(2.0) * M_PI);

  // Prior on standardized random effects
  for (int i = 0; i < n_subjects; i++) {
    for (int j = 0; j < n_re; j++) {
      nll -= dnorm(u(i, j), Type(0.0), Type(1.0), true);
    }
  }

  // Data likelihood
  for (int i = 0; i < n; i++) {
    int subj = subject_id(i);

    // Subject-specific parameters via design matrices + random effects
    // Use explicit dot product: cast row to vector for TMB/Eigen compatibility
    vector<Type> xq_i = X_q0.row(i);
    Type log_q0_i = (xq_i * beta_q0).sum() + re(subj, 0);
    Type Q0_i = exp(log_q0_i);

    vector<Type> xa_i = X_alpha.row(i);
    Type log_alpha_i = (xa_i * beta_alpha).sum();
    if (n_re == 2) {
      log_alpha_i += re(subj, 1);
    }
    Type alpha_i = exp(log_alpha_i);

    Type resid;

    if (eqn_type == 0) {
      // Exponential (HS): Gaussian on log(Q)
      // HS equation: log10(Q) = log10(Q0) + k*(exp(-α*Q0*C) - 1)
      // In natural log: ln(Q) = ln(Q0) + k*ln(10)*(exp(-α*Q0*C) - 1)
      Type mu = log_q0_i + k * log(Type(10.0)) * (exp(-alpha_i * Q0_i * price(i)) - Type(1.0));
      resid = (y(i) - mu) / sigma_e;

    } else if (eqn_type == 1) {
      // Exponentiated (Koffarnus): Gaussian on raw Q
      // log(Q_pred) = log(Q0) + k * ln(10) * (exp(-alpha * Q0 * price) - 1)
      // Q_pred = exp(log(Q_pred))
      Type log_Q_pred = log_q0_i + k * log(Type(10.0)) * (exp(-alpha_i * Q0_i * price(i)) - Type(1.0));
      Type Q_pred = exp(log_Q_pred);
      resid = (y(i) - Q_pred) / sigma_e;

    } else if (eqn_type == 2) {
      // Simplified: Gaussian on raw Q
      // Q_pred = Q0 * exp(-alpha * Q0 * price)
      Type Q_pred = Q0_i * exp(-alpha_i * Q0_i * price(i));
      resid = (y(i) - Q_pred) / sigma_e;

    } else {
      // zben (eqn_type == 3): Gaussian on LL4(Q)
      // Q0_log10 = log(Q0) / log(10)
      // rate = (alpha / Q0_log10) * Q0
      // y_pred = Q0_log10 * exp(-rate * price)
      Type Q0_log10 = log_q0_i / log(Type(10.0));
      // Clamp to avoid division by zero when Q0 ≈ 1 (log10(Q0) ≈ 0)
      Q0_log10 = CppAD::CondExpGt(CppAD::abs(Q0_log10), Type(1e-6), Q0_log10, Type(1e-6));
      Type rate = (alpha_i / Q0_log10) * Q0_i;
      Type y_pred = Q0_log10 * exp(-rate * price(i));
      resid = (y(i) - y_pred) / sigma_e;
    }

    // Gaussian NLL contribution
    nll -= -logsigma_e - Type(0.5) * log2pi - Type(0.5) * resid * resid;
  }

  // ADREPORT derived quantities
  ADREPORT(k);
  ADREPORT(sigma_b);
  ADREPORT(sigma_e);
  ADREPORT(re);
  ADREPORT(beta_q0);
  ADREPORT(beta_alpha);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
