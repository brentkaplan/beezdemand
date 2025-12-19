/// @file HurdleCrossPrice3RE.h
/// Two-Part Mixed Effects Hurdle Cross-Price Demand Model - THREE RANDOM EFFECTS
/// =============================================================================
///
/// MODEL STRUCTURE:
/// Part I (Binary):  logit(pi_ij) = beta0 + beta1*log(x+epsilon) + a_i
/// Part II (Continuous): log(Q_ij) = (logQalone + b_i) + (I + c_i)*exp(-beta*x) + e_ij
/// Random Effects: (a_i, b_i, c_i) ~ MVN(0, Sigma_3x3)
///
/// Parameters:
///   - beta0, beta1: Part I logistic regression coefficients
///   - logQalone: log of baseline consumption when alternative absent
///   - I: cross-price interaction intensity (sign indicates sub/complement)
///   - beta: rate of cross-price influence decay (constrained positive)
///   - c_i: individual-level random effect on I
///
/// =============================================================================

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type HurdleCrossPrice3RE(objective_function<Type>* obj) {
  // DATA
  DATA_VECTOR(x);           // Alternative product price
  DATA_VECTOR(logQ);        // log(consumption) for positive values, 0 otherwise
  DATA_IVECTOR(delta);      // 1 if consumption == 0, 0 if consumption > 0
  DATA_IVECTOR(subject_id);
  DATA_INTEGER(n_subjects);
  DATA_SCALAR(epsilon);     // Small constant for log(x + epsilon)

  // FIXED EFFECT PARAMETERS
  PARAMETER(beta0);         // Part I intercept
  PARAMETER(beta1);         // Part I slope on log(x + epsilon)
  PARAMETER(logQalone);     // log of Q_alone (baseline consumption)
  PARAMETER(I);             // Cross-price interaction intensity
  PARAMETER(log_beta);      // log(beta) to ensure beta > 0

  // VARIANCE PARAMETERS (log-scale)
  PARAMETER(logsigma_a);    // RE variance for Part I
  PARAMETER(logsigma_b);    // RE variance for logQalone
  PARAMETER(logsigma_c);    // RE variance for I
  PARAMETER(logsigma_e);    // Residual variance

  // Correlation parameters (unbounded, transformed via tanh)
  PARAMETER(rho_ab_raw);
  PARAMETER(rho_ac_raw);
  PARAMETER(rho_bc_raw);

  // RANDOM EFFECTS (standardized)
  PARAMETER_MATRIX(u);

  // TRANSFORM PARAMETERS
  Type beta_param = exp(log_beta);  // Ensure beta > 0
  Type sigma_a = exp(logsigma_a);
  Type sigma_b = exp(logsigma_b);
  Type sigma_c = exp(logsigma_c);
  Type sigma_e = exp(logsigma_e);

  Type rho_ab = tanh(rho_ab_raw);
  Type rho_ac = tanh(rho_ac_raw);
  Type rho_bc = tanh(rho_bc_raw);

  Type var_a = sigma_a * sigma_a;
  Type var_b = sigma_b * sigma_b;
  Type var_c = sigma_c * sigma_c;
  Type cov_ab = sigma_a * sigma_b * rho_ab;
  Type cov_ac = sigma_a * sigma_c * rho_ac;
  Type cov_bc = sigma_b * sigma_c * rho_bc;
  Type var_e = sigma_e * sigma_e;

  // BUILD 3x3 COVARIANCE MATRIX
  matrix<Type> Sigma(3, 3);
  Sigma(0, 0) = var_a;
  Sigma(0, 1) = cov_ab;
  Sigma(0, 2) = cov_ac;
  Sigma(1, 0) = cov_ab;
  Sigma(1, 1) = var_b;
  Sigma(1, 2) = cov_bc;
  Sigma(2, 0) = cov_ac;
  Sigma(2, 1) = cov_bc;
  Sigma(2, 2) = var_c;

  matrix<Type> L = Sigma.llt().matrixL();

  // Transform random effects
  matrix<Type> re(n_subjects, 3);
  for (int i = 0; i < n_subjects; i++) {
    vector<Type> u_i = u.row(i);
    vector<Type> re_i = L * u_i;
    re(i, 0) = re_i(0);
    re(i, 1) = re_i(1);
    re(i, 2) = re_i(2);
  }

  // NEGATIVE LOG-LIKELIHOOD
  Type nll = Type(0.0);
  int n = x.size();
  Type log2pi = log(Type(2.0) * M_PI);

  // Prior on random effects
  for (int i = 0; i < n_subjects; i++) {
    for (int j = 0; j < 3; j++) {
      nll -= dnorm(u(i, j), Type(0.0), Type(1.0), true);
    }
  }

  // Data likelihood
  for (int i = 0; i < n; i++) {
    int subj = subject_id(i);
    Type a_i = re(subj, 0);
    Type b_i = re(subj, 1);
    Type c_i = re(subj, 2);

    // Part I: Probability of zero consumption
    Type eta = beta0 + beta1 * log(x(i) + epsilon) + a_i;
    Type exp_eta = exp(eta);
    Type pi_ij = exp_eta / (Type(1.0) + exp_eta);

    // Part II: Expected log consumption (cross-price model)
    // log(Q) = (logQalone + b_i) + (I + c_i) * exp(-beta * x)
    Type mu_ij = (logQalone + b_i) + (I + c_i) * exp(-beta_param * x(i));

    if (delta(i) == 1) {
      // Zero consumption: contributes to Part I only
      nll -= log(pi_ij);
    } else {
      // Positive consumption: contributes to both parts
      nll -= log(Type(1.0) - pi_ij);
      Type resid = (logQ(i) - mu_ij) / sigma_e;
      nll -= -logsigma_e - Type(0.5) * log2pi - Type(0.5) * resid * resid;
    }
  }

  // ADREPORT derived quantities
  ADREPORT(beta_param);
  ADREPORT(var_a);
  ADREPORT(var_b);
  ADREPORT(var_c);
  ADREPORT(cov_ab);
  ADREPORT(cov_ac);
  ADREPORT(cov_bc);
  ADREPORT(var_e);
  ADREPORT(rho_ab);
  ADREPORT(rho_ac);
  ADREPORT(rho_bc);
  ADREPORT(sigma_a);
  ADREPORT(sigma_b);
  ADREPORT(sigma_c);
  ADREPORT(sigma_e);
  ADREPORT(re);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
