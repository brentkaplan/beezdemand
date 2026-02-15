# Get Estimated Marginal Means for Demand Parameters

Calculates Estimated Marginal Means (EMMs) for Q0 and alpha parameters
from a \`beezdemand_nlme\` model for all combinations of specified
factor levels. Reports parameters on both their estimation scale (log10)
and their natural, back-transformed scale. Optionally includes Essential
Value (EV).

## Usage

``` r
get_demand_param_emms(
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

  A \`beezdemand_nlme\` object.

- factors_in_emm:

  Character vector of factor names to compute EMMs over. Defaults to all
  factors present in the \`fit_obj\`.

- at:

  Optional named list specifying levels of conditioning variables for
  \`emmeans::ref_grid()\`.

- ci_level:

  Confidence level for the EMMs (default 0.95).

- include_ev:

  Logical. If TRUE, calculates and includes Essential Value (EV) derived
  from alpha, along with its confidence interval (calculated by
  back-transforming the CI of alpha_param_log10). Default \`FALSE\`.

- ...:

  Additional arguments passed to \`emmeans::emmeans()\`.

## Value

A tibble containing:

- Factor levels:

  Columns for each factor in \`factors_in_emm\`.

- Q0_param_log10, alpha_param_log10:

  EMMs for the model parameters (log10 scale) with their respective
  confidence intervals (LCL_Q0_param, UCL_Q0_param, etc.).

- Q0_natural, alpha_natural:

  EMMs back-transformed to the natural scale (10^param) with their
  respective confidence intervals (LCL_Q0_natural, UCL_Q0_natural,
  etc.).

- EV, LCL_EV, UCL_EV:

  (If \`include_ev=TRUE\`) Essential Value and its CI.
