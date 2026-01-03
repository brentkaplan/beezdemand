/// @file HurdleDemand2RE.hpp
/// Two-Part Mixed Effects Hurdle Demand Model - TWO RANDOM EFFECTS
/// =============================================================================
///
/// MODEL STRUCTURE:
/// Part I (Binary):  logit(pi_ij) = beta0 + beta1*log(price+epsilon) + a_i
/// Part II (Continuous): log(Q_ij) = (logQ0 + b_i) + k*(exp(-alpha*price) - 1) + e_ij
///   NOTE: alpha is FIXED (no c_i random effect), parameterized as log_alpha
///   to ensure positivity: alpha = exp(log_alpha)
///   NOTE: k is parameterized as log_k to ensure positivity: k = exp(log_k)
/// Random Effects: (a_i, b_i) ~ MVN(0, Sigma_2x2)
///
/// =============================================================================

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type HurdleDemand2RE(objective_function<Type>* obj) {
  // DATA
  DATA_VECTOR(price);
  DATA_VECTOR(logQ);
  DATA_IVECTOR(delta);
  DATA_IVECTOR(subject_id);
  DATA_INTEGER(n_subjects);
  DATA_SCALAR(epsilon);  // Small constant for log(price + epsilon)

  // FIXED EFFECT PARAMETERS
  PARAMETER(beta0);
  PARAMETER(beta1);
  PARAMETER(logQ0);
  PARAMETER(log_k);      // Log-space k to ensure positivity
  PARAMETER(log_alpha);  // Log-space alpha to ensure positivity

  // VARIANCE PARAMETERS (log-scale)
  PARAMETER(logsigma_a);
  PARAMETER(logsigma_b);
  PARAMETER(logsigma_e);

  // Correlation parameter
  PARAMETER(rho_ab_raw);

  // RANDOM EFFECTS (standardized)
  PARAMETER_MATRIX(u);

  // TRANSFORM PARAMETERS
  Type k = exp(log_k);        // Ensure k > 0
  Type alpha = exp(log_alpha);  // Ensure alpha > 0
  Type sigma_a = exp(logsigma_a);
  Type sigma_b = exp(logsigma_b);
  Type sigma_e = exp(logsigma_e);

  Type rho_ab = tanh(rho_ab_raw);

  Type var_a = sigma_a * sigma_a;
  Type var_b = sigma_b * sigma_b;
  Type cov_ab = sigma_a * sigma_b * rho_ab;
  Type var_e = sigma_e * sigma_e;

  // BUILD 2x2 COVARIANCE MATRIX
  matrix<Type> Sigma(2, 2);
  Sigma(0, 0) = var_a;
  Sigma(0, 1) = cov_ab;
  Sigma(1, 0) = cov_ab;
  Sigma(1, 1) = var_b;

  matrix<Type> L = Sigma.llt().matrixL();

  // Transform random effects
  matrix<Type> re(n_subjects, 2);
  for (int i = 0; i < n_subjects; i++) {
    vector<Type> u_i = u.row(i);
    vector<Type> re_i = L * u_i;
    re(i, 0) = re_i(0);
    re(i, 1) = re_i(1);
  }

  // NEGATIVE LOG-LIKELIHOOD
  Type nll = Type(0.0);
  int n = price.size();
  Type log2pi = log(Type(2.0) * M_PI);

  // Prior on random effects
  for (int i = 0; i < n_subjects; i++) {
    for (int j = 0; j < 2; j++) {
      nll -= dnorm(u(i, j), Type(0.0), Type(1.0), true);
    }
  }

  // Data likelihood (alpha is FIXED, no c_i)
  for (int i = 0; i < n; i++) {
    int subj = subject_id(i);
    Type a_i = re(subj, 0);
    Type b_i = re(subj, 1);

    Type eta = beta0 + beta1 * log(price(i) + epsilon) + a_i;
    Type exp_eta = exp(eta);
    Type pi_ij = exp_eta / (Type(1.0) + exp_eta);
    Type mu_ij = (logQ0 + b_i) + k * (exp(-alpha * price(i)) - Type(1.0));

    if (delta(i) == 1) {
      nll -= log(pi_ij);
    } else {
      nll -= log(Type(1.0) - pi_ij);
      Type resid = (logQ(i) - mu_ij) / sigma_e;
      nll -= -logsigma_e - Type(0.5) * log2pi - Type(0.5) * resid * resid;
    }
  }

  // ADREPORT derived quantities
  ADREPORT(k);      // Natural-scale k for reporting
  ADREPORT(alpha);  // Natural-scale alpha for reporting
  ADREPORT(var_a);
  ADREPORT(var_b);
  ADREPORT(cov_ab);
  ADREPORT(var_e);
  ADREPORT(rho_ab);
  ADREPORT(sigma_a);
  ADREPORT(sigma_b);
  ADREPORT(sigma_e);
  ADREPORT(re);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
