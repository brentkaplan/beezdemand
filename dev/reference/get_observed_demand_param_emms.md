# Get Estimated Marginal Means for Observed Factor Combinations

This function is a wrapper around \`get_demand_param_emms\`. It first
calls \`get_demand_param_emms\` to calculate Estimated Marginal Means
(EMMs) for Q0 and alpha parameters over all combinations of the
specified factor levels. It then filters these results to return EMMs
only for the combinations of factor levels that were actually present in
the original dataset used to fit the \`beezdemand_nlme\` model.

## Usage

``` r
get_observed_demand_param_emms(
  fit_obj,
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  include_ev = FALSE,
  ...
)
```

## Arguments

- fit_obj:

  A \`beezdemand_nlme\` object returned by \`fit_demand_mixed()\`.

- factors_in_emm:

  Character vector of factor names to compute EMMs over. Defaults to all
  factors present in the \`fit_obj\`. These factors define the grid over
  which EMMs are initially calculated and then filtered.

- at:

  Optional named list specifying levels of conditioning variables for
  \`emmeans::ref_grid()\`. Passed to \`get_demand_param_emms\`.

- ci_level:

  Confidence level for the EMMs (default 0.95). Passed to
  \`get_demand_param_emms\`.

- include_ev:

  Logical. If TRUE, calculates and includes Essential Value (EV) derived
  from alpha. Passed to \`get_demand_param_emms\`. Default \`FALSE\`.

- ...:

  Additional arguments passed to \`get_demand_param_emms\` and
  subsequently to \`emmeans::emmeans()\`.

## Value

A tibble similar to the output of \`get_demand_param_emms\`, but
filtered to include only rows corresponding to factor level combinations
that were observed in the original \`fit_obj\$data\`. Contains:

- Factor levels:

  Columns for each factor in \`factors_in_emm\`.

- Q0_param_log10, alpha_param_log10:

  EMMs for model parameters (log10 scale) and CIs.

- Q0_natural, alpha_natural:

  EMMs back-transformed to natural scale and CIs.

- EV, LCL_EV, UCL_EV:

  (If \`include_ev=TRUE\`) Essential Value and its CI.

## See also

[`get_demand_param_emms`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.md)
