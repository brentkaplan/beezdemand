/// @file MixedDemand.h
/// Continuous Mixed-Effects Demand Model via TMB (TICKET-011 Phase 2)
/// =============================================================================
///
/// MODEL STRUCTURE:
/// A continuous-response demand model with design-matrix fixed effects on Q0
/// and alpha plus block-structured random effects parameterized by per-block
/// pdDiag or pdSymm covariances.
///
/// Supports four equation types (runtime branching via eqn_type):
///   0 = exponential (HS):     Gaussian on log(Q)
///   1 = exponentiated (Koff): Gaussian on raw Q
///   2 = simplified:           Gaussian on raw Q (no k)
///   3 = zben:                 Gaussian on LL4(Q) (no k)
///
/// RANDOM EFFECTS:
///   * Z_q0 / Z_alpha: per-observation RE design matrices, columns ordered
///     [block1_q0_terms, block2_q0_terms, ...] (same for alpha).
///   * u: standardized REs, shape n_subjects x (re_dim_q0 + re_dim_alpha).
///     Columns ordered [block1_q0_terms, block1_alpha_terms,
///                      block2_q0_terms, block2_alpha_terms, ...].
///   * Per block b of size d_b = block_q0_dim[b] + block_alpha_dim[b]:
///       - logsigma slice (length d_b) -> diagonal SDs
///       - rho_raw slice (length d_b*(d_b-1)/2 if pdSymm, 0 if pdDiag)
///         -> partial correlations via LKJ-Cholesky construction
///       - L_b = diag(sigma_b) * L_corr_b (lower-triangular Cholesky of Sigma_b)
///       - re_block = L_b * u_block
///
/// BACKWARD-COMPAT (TICKET-011 Phase 2): a single intercept-only block with
/// pdSymm and d=2 reduces L_corr to [[1,0],[tanh(rho_raw[0]), sech(rho_raw[0])]],
/// matching the pre-Phase-2 2x2 tanh(rho_bc_raw) parameterization bit-for-bit.
///
/// =============================================================================

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type MixedDemand(objective_function<Type>* obj) {
  // ===========================================================================
  // DATA
  // ===========================================================================
  DATA_VECTOR(y);
  DATA_VECTOR(price);
  DATA_IVECTOR(subject_id);
  DATA_INTEGER(n_subjects);
  DATA_MATRIX(X_q0);
  DATA_MATRIX(X_alpha);
  DATA_MATRIX(Z_q0);              // n_obs x re_dim_q0 (may be 0-col)
  DATA_MATRIX(Z_alpha);           // n_obs x re_dim_alpha (may be 0-col)
  DATA_INTEGER(eqn_type);

  DATA_INTEGER(n_blocks);
  DATA_IVECTOR(block_q0_dim);
  DATA_IVECTOR(block_alpha_dim);
  DATA_IVECTOR(block_types);      // 0 = pdDiag, 1 = pdSymm
  DATA_IVECTOR(block_q0_offset);  // 0-indexed col offsets into Z_q0
  DATA_IVECTOR(block_alpha_offset);

  // ===========================================================================
  // PARAMETERS
  // ===========================================================================
  PARAMETER_VECTOR(beta_q0);
  PARAMETER_VECTOR(beta_alpha);
  PARAMETER(log_k);

  PARAMETER_VECTOR(logsigma);     // length sum(block_q0_dim + block_alpha_dim)
  PARAMETER(logsigma_e);
  PARAMETER_VECTOR(rho_raw);      // length sum(over pdSymm: d*(d-1)/2)

  PARAMETER_MATRIX(u);            // n_subjects x (re_dim_q0 + re_dim_alpha)

  // ===========================================================================
  // TRANSFORM SCALARS
  // ===========================================================================
  Type k = exp(log_k);
  Type sigma_e = exp(logsigma_e);

  int re_dim_q0 = Z_q0.cols();
  int re_dim_alpha = Z_alpha.cols();

  // ===========================================================================
  // BUILD PER-BLOCK CHOLESKY FACTORS AND SCALED RANDOM EFFECTS
  //
  // Layout: for each block b, walk through its sigma slice and rho slice,
  // construct L_corr via LKJ-Cholesky, scale by diag(sigma) to get L_b, then
  // transform u_block into re_block.
  // ===========================================================================
  matrix<Type> re_q0_mat(n_subjects, re_dim_q0);
  matrix<Type> re_alpha_mat(n_subjects, re_dim_alpha);
  re_q0_mat.setZero();
  re_alpha_mat.setZero();

  int sigma_offset = 0;
  int rho_offset = 0;
  int u_offset = 0;

  for (int b = 0; b < n_blocks; b++) {
    int d_q0 = block_q0_dim(b);
    int d_alpha = block_alpha_dim(b);
    int d = d_q0 + d_alpha;
    if (d == 0) continue;  // empty block (shouldn't happen but safe)

    // Extract sigma slice and build diag(sigma) factor.
    vector<Type> sigma_b(d);
    for (int i = 0; i < d; i++) {
      sigma_b(i) = exp(logsigma(sigma_offset + i));
    }

    // Build L_b = diag(sigma_b) * L_corr_b via LKJ-Cholesky construction.
    // For pdDiag (block_types(b) == 0), L_corr_b = identity.
    // For pdSymm with d == 1, L_corr_b = [[1]] (no off-diagonals).
    matrix<Type> L_b(d, d);
    L_b.setZero();
    L_b(0, 0) = sigma_b(0);

    if (block_types(b) == 1 && d > 1) {
      // pdSymm with d >= 2: build correlation Cholesky via LKJ from rho_raw.
      // Iterate j = 1..d-1; for each j, walk k = 0..j-1 consuming partial
      // correlations and computing L_corr(j, k). Diagonal L_corr(j, j) is
      // determined by the unit-row constraint.
      matrix<Type> L_corr(d, d);
      L_corr.setZero();
      L_corr(0, 0) = Type(1.0);

      for (int j = 1; j < d; j++) {
        Type sum_squares = Type(0.0);
        for (int k = 0; k < j; k++) {
          Type r = tanh(rho_raw(rho_offset));
          rho_offset++;
          if (k == 0) {
            L_corr(j, k) = r;
          } else {
            L_corr(j, k) = r * sqrt(Type(1.0) - sum_squares);
          }
          sum_squares += L_corr(j, k) * L_corr(j, k);
        }
        L_corr(j, j) = sqrt(Type(1.0) - sum_squares);
      }

      // L_b = diag(sigma_b) * L_corr
      for (int j = 0; j < d; j++) {
        for (int k = 0; k <= j; k++) {
          L_b(j, k) = sigma_b(j) * L_corr(j, k);
        }
      }
    } else {
      // pdDiag, or pdSymm with d == 1: L_b = diag(sigma_b)
      for (int j = 1; j < d; j++) {
        L_b(j, j) = sigma_b(j);
      }
    }

    // Transform u_block to re_block per subject.
    // u_block columns [u_offset, u_offset + d - 1] within u.
    for (int i = 0; i < n_subjects; i++) {
      vector<Type> u_block(d);
      for (int p = 0; p < d; p++) {
        u_block(p) = u(i, u_offset + p);
      }
      vector<Type> re_block = L_b * u_block;
      // Distribute: first d_q0 entries -> Z_q0 cols at block_q0_offset,
      //             next d_alpha entries -> Z_alpha cols at block_alpha_offset.
      for (int p = 0; p < d_q0; p++) {
        re_q0_mat(i, block_q0_offset(b) + p) = re_block(p);
      }
      for (int p = 0; p < d_alpha; p++) {
        re_alpha_mat(i, block_alpha_offset(b) + p) = re_block(d_q0 + p);
      }
    }

    sigma_offset += d;
    u_offset += d;
  }

  // ===========================================================================
  // NEGATIVE LOG-LIKELIHOOD
  // ===========================================================================
  Type nll = Type(0.0);
  int n = y.size();
  Type log2pi = log(Type(2.0) * M_PI);

  // Standard-normal prior on the standardized REs (u).
  for (int i = 0; i < n_subjects; i++) {
    for (int j = 0; j < (re_dim_q0 + re_dim_alpha); j++) {
      nll -= dnorm(u(i, j), Type(0.0), Type(1.0), true);
    }
  }

  // Data likelihood.
  for (int i = 0; i < n; i++) {
    int subj = subject_id(i);

    // Fixed-effect contributions.
    vector<Type> xq_i = X_q0.row(i);
    Type log_q0_i = (xq_i * beta_q0).sum();
    vector<Type> xa_i = X_alpha.row(i);
    Type log_alpha_i = (xa_i * beta_alpha).sum();

    // RE contributions via Z dot product.
    if (re_dim_q0 > 0) {
      vector<Type> zq_i = Z_q0.row(i);
      vector<Type> re_q0_subj = re_q0_mat.row(subj);
      log_q0_i += (zq_i * re_q0_subj).sum();
    }
    if (re_dim_alpha > 0) {
      vector<Type> za_i = Z_alpha.row(i);
      vector<Type> re_alpha_subj = re_alpha_mat.row(subj);
      log_alpha_i += (za_i * re_alpha_subj).sum();
    }

    Type Q0_i = exp(log_q0_i);
    Type alpha_i = exp(log_alpha_i);
    Type resid;

    if (eqn_type == 0) {
      // Exponential (HS): Gaussian on log(Q)
      Type mu = log_q0_i + k * log(Type(10.0)) * (exp(-alpha_i * Q0_i * price(i)) - Type(1.0));
      resid = (y(i) - mu) / sigma_e;

    } else if (eqn_type == 1) {
      // Exponentiated (Koffarnus): Gaussian on raw Q
      Type log_Q_pred = log_q0_i + k * log(Type(10.0)) * (exp(-alpha_i * Q0_i * price(i)) - Type(1.0));
      Type Q_pred = exp(log_Q_pred);
      resid = (y(i) - Q_pred) / sigma_e;

    } else if (eqn_type == 2) {
      // Simplified: Gaussian on raw Q
      Type Q_pred = Q0_i * exp(-alpha_i * Q0_i * price(i));
      resid = (y(i) - Q_pred) / sigma_e;

    } else {
      // zben (eqn_type == 3): Gaussian on LL4(Q)
      Type Q0_log10 = log_q0_i / log(Type(10.0));
      Q0_log10 = CppAD::CondExpGt(Q0_log10, Type(1e-3), Q0_log10, Type(1e-3));
      Type rate = (alpha_i / Q0_log10) * Q0_i;
      Type y_pred = Q0_log10 * exp(-rate * price(i));
      resid = (y(i) - y_pred) / sigma_e;
    }

    nll -= -logsigma_e - Type(0.5) * log2pi - Type(0.5) * resid * resid;
  }

  // ===========================================================================
  // ADREPORT
  // ===========================================================================
  ADREPORT(k);
  ADREPORT(sigma_e);
  ADREPORT(beta_q0);
  ADREPORT(beta_alpha);
  // Per-RE-column scaled REs for downstream subject_pars / ranef methods.
  ADREPORT(re_q0_mat);
  ADREPORT(re_alpha_mat);

  return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
