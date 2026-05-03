# Get Estimated Marginal Means for Demand Parameters

Calculates Estimated Marginal Means (EMMs) for Q0 and alpha parameters
from a `beezdemand_nlme` model for all combinations of specified factor
levels. Reports parameters on both their estimation scale (log10) and
their natural, back-transformed scale. Optionally includes Essential
Value (EV).

## Usage

``` r
get_demand_param_emms(fit_obj, ...)

# Default S3 method
get_demand_param_emms(fit_obj, ...)

# S3 method for class 'beezdemand_nlme'
get_demand_param_emms(
  fit_obj,
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  include_ev = FALSE,
  param = c("both", "Q0", "alpha"),
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_nlme` object.

- ...:

  Additional arguments passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

- factors_in_emm:

  Character vector of factor names to compute EMMs over. Defaults to all
  factors present in the `fit_obj`.

- at:

  Optional named list specifying levels of conditioning variables for
  [`emmeans::ref_grid()`](https://rvlenth.github.io/emmeans/reference/ref_grid.html).

- ci_level:

  Confidence level for the EMMs (default 0.95).

- include_ev:

  Logical. If TRUE, calculates and includes Essential Value (EV) derived
  from alpha, along with its confidence interval (calculated by
  back-transforming the CI of alpha_param_log10). Default `FALSE`.

- param:

  Character, one of `"both"` (default), `"Q0"`, or `"alpha"`. Controls
  which demand parameter's EMM columns are returned. `"both"` preserves
  the historical four-column-block structure (Q0 and alpha together).
  `"Q0"` returns only Q0 columns (and drops EV, since EV is a function
  of alpha); a warning is emitted if `include_ev = TRUE` is requested
  alongside `param = "Q0"`. `"alpha"` returns only alpha columns plus
  the EV block when `include_ev = TRUE`. Mirrors the `param` argument on
  the `beezdemand_tmb` method.

## Value

A tibble containing:

- Factor levels:

  Columns for each factor in `factors_in_emm`.

- Q0_param_log10, alpha_param_log10:

  EMMs for the model parameters (log10 scale) with their respective
  confidence intervals (LCL_Q0_param, UCL_Q0_param, etc.).

- Q0_natural, alpha_natural:

  EMMs back-transformed to the natural scale (10^param) with their
  respective confidence intervals (LCL_Q0_natural, UCL_Q0_natural,
  etc.).

- EV, LCL_EV, UCL_EV:

  (If `include_ev=TRUE`) Essential Value and its CI.

When `param = "Q0"` or `param = "alpha"`, only the columns associated
with the requested parameter (plus factor columns and, for `"alpha"`,
the EV block) are returned.

## Examples

``` r
# \donttest{
data(ko, package = "beezdemand")
ko$y_ll4 <- ll4(ko$y, lambda = 4)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
  id_var = "monkey", factors = "dose", equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 10 (Q0: 5, alpha: 5)
get_demand_param_emms(fit)
#> # A tibble: 5 × 13
#>   dose  Q0_param_log10 LCL_Q0_param_log10 UCL_Q0_param_log10 Q0_natural
#>   <fct>          <dbl>              <dbl>              <dbl>      <dbl>
#> 1 3e-05           2.58               2.35               2.80      377. 
#> 2 1e-04           2.38               2.23               2.52      238. 
#> 3 3e-04           2.21               2.10               2.32      163. 
#> 4 0.001           1.91               1.78               2.03       80.5
#> 5 0.003           1.90               1.73               2.07       79.7
#> # ℹ 8 more variables: LCL_Q0_natural <dbl>, UCL_Q0_natural <dbl>,
#> #   alpha_param_log10 <dbl>, LCL_alpha_param_log10 <dbl>,
#> #   UCL_alpha_param_log10 <dbl>, alpha_natural <dbl>, LCL_alpha_natural <dbl>,
#> #   UCL_alpha_natural <dbl>

# Request only Q0 columns — convenient for pivoting and plotting
get_demand_param_emms(fit, param = "Q0")
#> # A tibble: 5 × 7
#>   dose  Q0_param_log10 LCL_Q0_param_log10 UCL_Q0_param_log10 Q0_natural
#>   <fct>          <dbl>              <dbl>              <dbl>      <dbl>
#> 1 3e-05           2.58               2.35               2.80      377. 
#> 2 1e-04           2.38               2.23               2.52      238. 
#> 3 3e-04           2.21               2.10               2.32      163. 
#> 4 0.001           1.91               1.78               2.03       80.5
#> 5 0.003           1.90               1.73               2.07       79.7
#> # ℹ 2 more variables: LCL_Q0_natural <dbl>, UCL_Q0_natural <dbl>
# }
```
