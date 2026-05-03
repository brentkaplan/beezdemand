# Simulate within-subject demand data for TICKET-011 Phase 2 parity tests

Generates long-format demand data where each subject is observed at
every price under every level of an in-subject `condition` factor. Used
by the Phase 2 parity tests to confirm that
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
matches
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
on factor-expanded random-effects specifications
(`pdDiag(Q0+alpha~condition)`, `pdSymm(Q0+alpha~condition)` etc.).

## Usage

``` r
.simulate_within_subject_demand(
  n_subjects = 30,
  n_conditions = 3,
  prices = c(0.1, 0.5, 1, 2, 5, 10, 20),
  log_q0_pop = log(20),
  log_alpha_pop = log(0.005),
  delta_q0 = NULL,
  delta_alpha = NULL,
  sigma_b = 0.3,
  sigma_d = 0.3,
  rho_bd = 0,
  sigma_e = 0.1,
  seed = NULL
)
```

## Arguments

- n_subjects:

  Integer; number of subjects.

- n_conditions:

  Integer; number of within-subject condition levels (named `"C1"`,
  `"C2"`, ...).

- prices:

  Numeric vector of prices each subject sees at every condition.

- log_q0_pop:

  Numeric; population log-Q0.

- log_alpha_pop:

  Numeric; population log-alpha.

- delta_q0:

  Numeric vector of length `n_conditions`; per-condition shifts on
  log-Q0. Defaults to 0 for all conditions.

- delta_alpha:

  Numeric vector of length `n_conditions`; per-condition shifts on
  log-alpha. Defaults to 0 for all conditions.

- sigma_b:

  Numeric; SD of per-(subject, condition) Q0 random deviation.

- sigma_d:

  Numeric; SD of per-(subject, condition) alpha random deviation.

- rho_bd:

  Numeric; correlation between b and d within (subject, condition).

- sigma_e:

  Numeric; residual SD on log-y.

- seed:

  Optional integer seed for reproducibility.

## Value

A tibble with columns `id` (factor), `condition` (factor), `x` (price),
and `y` (consumption). Long-format, one row per (subject, condition,
price).

## Details

Data-generating process: each subject `i` at condition `c` and price `p`
has consumption \$\$y\_{i,c,p} = Q\_{0,i,c} \cdot \exp(-\alpha\_{i,c}
\cdot Q\_{0,i,c} \cdot p) \cdot \exp(\epsilon)\$\$ where \$\$\log
Q\_{0,i,c} = \log Q\_{0,\text{pop}} + \delta^{Q_0}\_c + b\_{i,c}\$\$
\$\$\log \alpha\_{i,c} = \log \alpha\_{\text{pop}} +
\delta^{\alpha}\_c + d\_{i,c}\$\$ with per-condition shifts
`delta_q0[c]`, `delta_alpha[c]` and per-subject per-condition random
deviations `(b_{i,c}, d_{i,c}) ~ N(0, Sigma)`.
