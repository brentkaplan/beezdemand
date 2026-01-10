/// @file HurdleDemand3RE_StdQ0.h
/// Two-Part Mixed Effects Hurdle Demand Model - THREE RANDOM EFFECTS
/// HS-standardized Part II (Q0 inside exponent)
///
/// Part I:  logit(pi_ij) = beta0 + beta1*log(price+epsilon) + a_i
/// Part II: log(Q_ij) = (log_q0 + b_i) + k*(exp(-alpha_i * Q0_i * price) - 1) + e_ij
///   where Q0_i = exp(log_q0 + b_i)
///   and alpha_i = exp(log_alpha + c_i), k = exp(log_k)
/// Random Effects: (a_i, b_i, c_i) ~ MVN(0, Sigma_3x3)

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type HurdleDemand3RE_StdQ0(objective_function<Type>* obj) {
  // DATA
  DATA_VECTOR(price);
  DATA_VECTOR(logQ);
  DATA_IVECTOR(delta);
  DATA_IVECTOR(subject_id);
  DATA_INTEGER(n_subjects);
  DATA_SCALAR(epsilon);

  // FIXED EFFECT PARAMETERS
  PARAMETER(beta0);
  PARAMETER(beta1);
  PARAMETER(log_q0);
  PARAMETER(log_k);
  PARAMETER(log_alpha);

  // VARIANCE PARAMETERS (log-scale)
  PARAMETER(logsigma_a);
  PARAMETER(logsigma_b);
  PARAMETER(logsigma_c);
  PARAMETER(logsigma_e);

  // Correlation parameters (unbounded, transformed via tanh)
  PARAMETER(rho_ab_raw);
  PARAMETER(rho_ac_raw);
  PARAMETER(rho_bc_raw); // partial correlation raw

  // RANDOM EFFECTS (standardized)
  PARAMETER_MATRIX(u);

  // TRANSFORM PARAMETERS
  Type k = exp(log_k);
  Type alpha = exp(log_alpha);
  Type sigma_a = exp(logsigma_a);
  Type sigma_b = exp(logsigma_b);
  Type sigma_c = exp(logsigma_c);
  Type sigma_e = exp(logsigma_e);

  // PD correlation mapping (partial correlation for rho_bc)
  Type rho_ab = tanh(rho_ab_raw);
  Type rho_ac = tanh(rho_ac_raw);
  Type rho_bc_partial = tanh(rho_bc_raw);
  Type rho_bc = rho_ab * rho_ac +
    rho_bc_partial * sqrt((Type(1.0) - rho_ab * rho_ab) * (Type(1.0) - rho_ac * rho_ac));

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
  int n = price.size();
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

    Type Q0_i = exp(log_q0 + b_i);
    Type alpha_i = exp(log_alpha + c_i);

    Type eta = beta0 + beta1 * log(price(i) + epsilon) + a_i;
    Type mu_ij = (log_q0 + b_i) + k * (exp(-alpha_i * Q0_i * price(i)) - Type(1.0));

    if (delta(i) == 1) {
      nll += logspace_add(Type(0.0), -eta);
    } else {
      nll += logspace_add(Type(0.0), eta);
      Type resid = (logQ(i) - mu_ij) / sigma_e;
      nll -= -logsigma_e - Type(0.5) * log2pi - Type(0.5) * resid * resid;
    }
  }

  // ADREPORT
  ADREPORT(k);
  ADREPORT(alpha);
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

