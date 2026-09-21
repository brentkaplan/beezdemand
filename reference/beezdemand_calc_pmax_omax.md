# Calculate Pmax and Omax with Method Reporting and Parameter-Space Safety

Unified internal engine for pmax/omax computation. Supports analytic
solutions (Lambert W for HS/hurdle, closed-form for SND), numerical
fallback, and observed (row-wise) metrics. Parameter-scale conversions
are handled internally.

## Usage

``` r
beezdemand_calc_pmax_omax(
  model_type = NULL,
  params = NULL,
  param_scales = NULL,
  expenditure_fn = NULL,
  demand_fn = NULL,
  p_zero_fn = NULL,
  price_obs = NULL,
  consumption_obs = NULL,
  tol = 0.1,
  compute_observed = NULL
)
```

## Arguments

- model_type:

  Character: "hs", "koff", "hurdle", "hurdle_hs_stdq0", "snd",
  "simplified", "zben", or NULL

- params:

  Named list of parameters. Names depend on model_type:

  - hs/koff: alpha, q0, k

  - hurdle: alpha, q0, k (note: hurdle uses different formula)

  - hurdle_hs_stdq0: alpha, q0, k (Q0 appears inside exponent)

  - snd/simplified: alpha, q0

  - zben: alpha, q0 (TMB-tier zero-bounded exponential; numerical
    fallback only, which requires `price_obs` / `price_range` for the
    numerical search domain; see
    [`.pmax_numerical()`](https://brentkaplan.github.io/beezdemand/reference/dot-pmax_numerical.md))

- param_scales:

  Named list mapping parameter names to their input scales: "natural",
  "log", or "log10". Default assumes all natural.

- expenditure_fn:

  Optional function E(p) for numerical fallback. If NULL and model_type
  is provided, will be constructed from params.

- demand_fn:

  Optional function Q(p) for elasticity calculation.

- price_obs:

  Numeric vector of observed prices (required for observed metrics and
  domain constraints).

- consumption_obs:

  Numeric vector of observed consumption (for observed metrics).

- tol:

  Tolerance for unit elasticity check (default 0.1).

- compute_observed:

  Logical; compute observed metrics? Default TRUE if price_obs and
  consumption_obs are provided.

## Value

A list with snake_case fields:

- pmax_model:

  Model-based pmax

- omax_model:

  Model-based omax

- q_at_pmax_model:

  Quantity at pmax

- method_model:

  Method used: "analytic_lambert_w", "analytic_snd",
  "numerical_optimize_observed_domain"

- domain_model:

  Price domain used for computation

- is_boundary_model:

  Logical; is pmax at domain boundary?

- elasticity_at_pmax_model:

  Elasticity evaluated at pmax

- unit_elasticity_pass_model:

  Logical; is elasticity near -1?

- note_model:

  Any notes about model computation

- pmax_obs:

  Observed pmax

- omax_obs:

  Observed omax

- method_obs:

  Method for observed: "row_wise_max"

- tie_break_obs:

  Tie-break rule: "min_price"

- n_obs_rows:

  Number of observation rows

- n_unique_prices:

  Number of unique prices

- has_duplicate_prices:

  Logical; duplicate prices detected?

- n_max_ties:

  Number of rows achieving omax

- note_obs:

  Notes about observed computation

- alpha_scale_in:

  Input scale for alpha

- q0_scale_in:

  Input scale for Q0

- k_scale_in:

  Input scale for k (if applicable)

- note_param_space:

  Notes about parameter conversions

## Multiple expenditure maxima

Two model families can have more than one local maximum of expenditure,
and the reported Pmax is then a convention rather than the unique
maximiser:

- **zben**: on the back-transformed scale the expenditure curve can have
  two peaks, one on each side of the LL4 knee. With `F(t) = t * exp(-t)`
  scaled by `q = log10(Q0)`, the two-peak regime is the set of
  parameters for which the knee falls between the two stationary points
  of the transformed curve (in the engine's notation
  `F(b) < 4 ln Q0 < F(a)`). The engine searches the analytic domain that
  contains every candidate, refines each grid-local maximum, and reports
  the global one; a small change in `alpha` or `Q0` can therefore move
  Pmax discontinuously from one peak to the other. Counts of fits on
  which the old single-start optimiser missed the global peak measure
  optimiser failure; they say nothing about how common the two-peak
  regime is in data.

- **HS / hurdle exponential with `k > e`**: the exponential expenditure
  curve is unbounded as price grows, so the analytic Lambert-W solution
  returns the *first* local maximum (the peak before expenditure starts
  rising again), which is the quantity the demand literature reports as
  Pmax. It is not the supremum of expenditure.

## Examples

``` r
# \donttest{
result <- beezdemand_calc_pmax_omax(
  model_type = "hs",
  params = list(alpha = 0.001, q0 = 10, k = 3),
  price_obs = c(0, 0.5, 1, 2, 4, 8, 16),
  consumption_obs = c(10, 9, 8, 6, 3, 1, 0)
)
result$pmax_model
#> [1] 17.19203
result$omax_model
#> [1] 57.73857
# }
```
