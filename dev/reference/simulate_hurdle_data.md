# Simulate Data from Two-Part Mixed Effects Hurdle Demand Model

Generates simulated demand data from the two-part hurdle model. Useful
for Monte Carlo simulation studies, power analyses, and model
validation.

## Usage

``` r
simulate_hurdle_data(
  n_subjects = 100,
  prices = seq(0, 11, by = 0.5),
  beta0 = -2,
  beta1 = 1,
  log_q0 = log(10),
  logQ0 = deprecated(),
  k = 2,
  alpha = 0.5,
  sigma_a = 1,
  sigma_b = 0.5,
  sigma_c = 0.1,
  rho_ab = 0.3,
  rho_ac = 0,
  rho_bc = 0,
  sigma_e = 0.3,
  epsilon = 0.001,
  n_random_effects = 2,
  stop_at_zero = TRUE,
  seed = NULL,
  part2 = c("koff", "snd"),
  rho_ab_raw = NULL,
  rho_ac_raw = NULL,
  rho_bc_raw = NULL
)
```

## Arguments

- n_subjects:

  Number of subjects to simulate. Default is 100.

- prices:

  Numeric vector of prices at which to simulate consumption. Default is
  `seq(0, 11, by = 0.5)`.

- beta0:

  Intercept for Part I (logistic). Default is -2.

- beta1:

  Slope for Part I (on log(price + epsilon)). Default is 1.

- log_q0:

  Log of intensity parameter (Q0). Default is log(10), meaning Q0 = 10.
  To specify Q0 directly, use `log_q0 = log(your_Q0)`.

- logQ0:

  **\[deprecated\]** Use `log_q0` instead.

- k:

  Scaling parameter for demand decay. Default is 2.

- alpha:

  Elasticity parameter controlling rate of demand decay. Default is 0.5.

- sigma_a:

  Standard deviation of random intercept for Part I. Default is 1.

- sigma_b:

  Standard deviation of random intercept for Part II. Default is 0.5.

- sigma_c:

  Standard deviation of random slope for alpha (only used if
  `n_random_effects = 3`). Default is 0.1.

- rho_ab:

  Correlation between a_i and b_i. Default is 0.3.

- rho_ac:

  Correlation between a_i and c_i. Default is 0.

- rho_bc:

  Correlation between b_i and c_i. Default is 0.

- sigma_e:

  Residual standard deviation. Default is 0.3.

- epsilon:

  Small constant for log(price + epsilon). Default is 0.001.

- n_random_effects:

  Number of random effects (2 or 3). Default is 2.

- stop_at_zero:

  Logical; if TRUE, stop generating observations for a subject once zero
  consumption is observed. This means subjects will have varying numbers
  of observations. Set to FALSE to generate all prices for all subjects.
  Default is TRUE.

- seed:

  Optional random seed for reproducibility.

- part2:

  Character. Positive-part (Part II) generator: `"koff"` (the original
  Zhao et al. (2016) / Koffarnus-style generator; default, backward
  compatible) or `"snd"` (matches `src/HurdleDemand3RE_SND.h` /
  `src/HurdleDemand2RE_SND.h` exactly; see Details).

- rho_ab_raw:

  Only used when `part2 = "snd"`.
  Pre-[`tanh()`](https://rdrr.io/r/base/Hyperbolic.html) raw value for
  the a/b random-effect correlation, mirroring the TMB model's own
  `rho_ab_raw` coefficient (actual correlation is `tanh(rho_ab_raw)`).
  Default `NULL` uses `atanh(0.3)` (actual correlation 0.3, matching the
  `"koff"` generator's default `rho_ab`).

- rho_ac_raw:

  Only used when `part2 = "snd"` and `n_random_effects = 3`.
  Pre-[`tanh()`](https://rdrr.io/r/base/Hyperbolic.html) raw value for
  the a/c random-effect correlation. Default `NULL` uses `0` (actual
  correlation 0).

- rho_bc_raw:

  Only used when `part2 = "snd"` and `n_random_effects = 3`. Raw value
  for the b/c *partial* correlation (see Details); the actual b/c
  correlation is not `tanh(rho_bc_raw)` directly. Default `NULL` uses
  `0` (actual correlation 0).

  Note: `part2` and `rho_*_raw` were added after `seed` in the argument
  list so that pre-existing positional calls (whose 19th and last
  positional argument was `seed`) continue to bind correctly instead of
  landing on `part2` (where
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) would error).
  Always pass `part2`/`rho_*_raw` by name.

## Value

A data frame with columns:

- id:

  Subject identifier

- x:

  Price value

- y:

  Simulated consumption (may include zeros)

- delta:

  Indicator for zero consumption (1 = zero, 0 = positive)

- a_i:

  Subject-specific random effect for Part I

- b_i:

  Subject-specific random effect for Part II

- c_i:

  Subject-specific random effect for alpha (if n_random_effects = 3)

## Details

**Part I (Zero vs Positive), shared by both `part2` generators:**
\$\$logit(P(Y=0)) = \beta_0 + \beta_1 \cdot \log(price + \epsilon) +
a_i\$\$

**Part II, `part2 = "koff"` (Zhao et al., 2016):** \$\$\log(Y \| Y \> 0)
= (\log Q_0 + b_i) + k \cdot (\exp(-(\alpha + c_i) \cdot price) - 1) +
\epsilon\$\$

**Part II, `part2 = "snd"` (exactly mirrors `src/HurdleDemand3RE_SND.h`
/ `src/HurdleDemand2RE_SND.h`, i.e. a log-linear/SND mean with lognormal
errors and no `k`):** \$\$Q\_{0,i} = \exp(\log Q_0 + b_i), \quad
\alpha_i = \exp(\log \alpha + c_i)\$\$ \$\$\log(Y \| Y \> 0) = (\log
Q_0 + b_i) - \alpha_i Q\_{0,i} \cdot price + \epsilon\$\$ with
\\\epsilon \sim N(0, \sigma_e^2)\\ (residual on the log-consumption
scale) in both generators. When `n_random_effects = 2`, `c_i = 0` for
every subject in both generators, so `alpha_i` reduces to the fixed
population `alpha`.

Random effects \\(a_i, b_i)\\ or \\(a_i, b_i, c_i)\\ are drawn from a
multivariate normal distribution with mean zero and covariance built
from `sigma_a`, `sigma_b`, `sigma_c` (SDs, natural scale) and the
specified correlations. For `part2 = "koff"` (unchanged from previous
releases), `rho_ab`, `rho_ac`, `rho_bc` are the actual (final)
correlations, checked for a jointly positive-definite covariance matrix.
For `part2 = "snd"`, correlations instead mirror the TMB model's own
raw-parameter -\> actual correlation mapping exactly (any raw values are
guaranteed to give a valid PD matrix, so no PD check is needed):
`rho_ab = tanh(rho_ab_raw)`, `rho_ac = tanh(rho_ac_raw)`, and `rho_bc`
via the LKJ-Cholesky partial-correlation transform \$\$\rho\_{bc} =
\rho\_{ab}\rho\_{ac} + \tanh(\rho\_{bc,raw}) \sqrt{(1 -
\rho\_{ab}^2)(1 - \rho\_{ac}^2)}.\$\$ This lets a user plug a fitted
`part2 = "snd"` model's own `rho_ab_raw`/`rho_ac_raw`/`rho_bc_raw`
coefficients directly into the simulator for a parametric bootstrap or
recovery study.

## See also

[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md),
[`run_hurdle_monte_carlo`](https://brentkaplan.github.io/beezdemand/reference/run_hurdle_monte_carlo.md)

## Examples

``` r
# Simulate with default parameters (2 RE model)
sim_data <- simulate_hurdle_data(n_subjects = 100, seed = 123)
head(sim_data)
#>   id   x        y delta        a_i        b_i
#> 1  1 0.0 4.718989     0 -0.5604756 -0.4229137
#> 2  1 0.5 3.887357     0 -0.5604756 -0.4229137
#> 3  1 1.0 1.966397     0 -0.5604756 -0.4229137
#> 4  1 1.5 1.976901     0 -0.5604756 -0.4229137
#> 5  1 2.0 1.847601     0 -0.5604756 -0.4229137
#> 6  1 2.5 2.580423     0 -0.5604756 -0.4229137

# Simulate with custom prices
apt_prices <- c(0, 0.25, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8, 9, 10)
sim_apt <- simulate_hurdle_data(n_subjects = 100, prices = apt_prices, seed = 123)

# Simulate with custom parameters (Q0 = 15, alpha = 0.1)
sim_custom <- simulate_hurdle_data(
  n_subjects = 100,
  log_q0 = log(15),
  alpha = 0.1,
  seed = 123
)

# Simulate 3 RE model
sim_3re <- simulate_hurdle_data(
  n_subjects = 100,
  n_random_effects = 3,
  sigma_c = 0.1,
  seed = 456
)
```
