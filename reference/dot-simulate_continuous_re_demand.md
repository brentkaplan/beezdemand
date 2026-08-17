# Simulate dose-response demand with a continuous within-subject random slope

Sibling of
[`.simulate_within_subject_demand()`](https://brentkaplan.github.io/beezdemand/reference/dot-simulate_within_subject_demand.md)
for TICKET-051: instead of a within-subject *factor*, each subject is
observed at several values of a *continuous* within-subject covariate
(e.g. centered `log10` drug dose), and both intensity and elasticity
change with that covariate at a subject-specific rate (a continuous
random slope).

## Usage

``` r
.simulate_continuous_re_demand(
  n_subjects = 60,
  doses = c(-2, -1, 0, 1, 2),
  prices = c(0, 1, 2, 4, 8, 16, 24, 36),
  log_q0_pop = log(20),
  log_alpha_pop = log(0.006),
  b1_q0 = 0.1,
  b1_alpha = -0.15,
  sd_u_q0 = 0.3,
  sd_w_q0 = 0.1,
  sd_u_alpha = 0.3,
  sd_w_alpha = 0.1,
  rho_q0 = 0.3,
  rho_alpha = 0.3,
  sigma_e = 0.05,
  covariate_name = "dose_c",
  seed = NULL
)
```

## Arguments

- n_subjects:

  Integer; number of subjects.

- doses:

  Numeric vector of within-subject covariate values seen by every
  subject. Assumed centered (mean ~ 0); for dose ladders typically
  `log10`-spaced then centered.

- prices:

  Numeric vector of prices each subject sees at every dose.

- log_q0_pop, log_alpha_pop:

  Numeric; population log-Q0 / log-alpha at the reference covariate
  value (`dose_c = 0`).

- b1_q0, b1_alpha:

  Numeric; fixed dose slopes on log-Q0 / log-alpha.

- sd_u_q0, sd_w_q0:

  Numeric; SDs of the Q0 random intercept / slope.

- sd_u_alpha, sd_w_alpha:

  Numeric; SDs of the alpha random intercept / slope.

- rho_q0, rho_alpha:

  Numeric in (-1, 1); intercept-slope correlation on the Q0 and alpha
  sides respectively.

- sigma_e:

  Numeric; SD of the lognormal observation noise on log-y.

- covariate_name:

  Character; name of the emitted covariate column (default `"dose_c"`).

- seed:

  Optional integer seed for reproducibility.

## Value

A tibble with columns `id` (factor), the covariate (`dose_c` by default,
numeric), `x` (price), and `y` (consumption). Long-format, one row per
(subject, dose, price). The true generating parameters are attached as
`attr(., "truth")`.

## Details

Data-generating process: for subject `i` at covariate value `d` (column
`dose_c`, assumed already centered) and price `p`, \$\$\log Q\_{0,i}(d)
= \log Q\_{0,\text{pop}} + \beta^{Q_0}\_1 d + u^{Q_0}\_i + w^{Q_0}\_i
d\$\$ \$\$\log \alpha_i(d) = \log \alpha\_{\text{pop}} +
\beta^{\alpha}\_1 d + u^{\alpha}\_i + w^{\alpha}\_i d\$\$ with
per-subject random intercepts/slopes \\(u^{Q_0}\_i, w^{Q_0}\_i,
u^{\alpha}\_i, w^{\alpha}\_i) \sim \mathcal{N}(0, \Sigma)\\, and SND
mean consumption \\\mu = Q\_{0,i}(d)\exp(-\alpha_i(d)\\Q\_{0,i}(d)\\p)\\
with multiplicative lognormal observation noise \\y = \mu \cdot
\exp(\epsilon)\\, \\\epsilon \sim \mathcal{N}(0, \sigma_e^2)\\.

\\\Sigma\\ is block-diagonal across the Q0 and alpha parameter sides:
the Q0 intercept/slope pair correlates at `rho_q0`, the alpha pair at
`rho_alpha`, and the two sides are independent.

**Noise caveat (TICKET-051 landmine \#5).** The noise here is lognormal
for convenience (mirroring `test-tmb-recovery.R`), whereas the
`simplified` / `exponentiated` TMB likelihoods are Gaussian on raw Q.
The fitted residual SD therefore will *not* match `sigma_e`; only
**mean-structure** recovery (fixed slopes, RE intercept/slope SDs, and
correlations) is a valid check.
