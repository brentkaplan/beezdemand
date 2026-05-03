# Build the conditioned reference grid for TMB EMMs and comparisons

Shared helper that constructs `level_combos` (the factor-level grid,
optionally filtered by `at`) and `ref_X` (the corresponding design
matrix) for a TMB demand fit. Both
[`get_demand_param_emms.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.beezdemand_tmb.md)
and
[`get_demand_comparisons.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.beezdemand_tmb.md)
consume this helper so they cannot drift apart on which cells the user
requested.

## Usage

``` r
.tmb_build_emm_ref_grid(
  fit_obj,
  param = c("Q0", "alpha"),
  at = NULL,
  factors_in_emm = NULL
)
```

## Arguments

- fit_obj:

  A `beezdemand_tmb` object.

- param:

  Character. `"Q0"` or `"alpha"`.

- at:

  Named list of factor-level filters or covariate-value overrides.

- factors_in_emm:

  Character subset of fitted factors to include.

## Value

A list with components:

- level_combos:

  Filtered grid as a data.frame.

- ref_X:

  Filtered design matrix.

- use_factors:

  Character vector of factors driving the grid.

- cov_names:

  Character vector of continuous covariates.

- is_intercept_only:

  Logical; `TRUE` when the fit has neither factors nor covariates.

## Details

Continuous covariates are held at the training mean unless overridden
via `at`. Factor levels are filtered down to the requested values when
`at` names a factor.
