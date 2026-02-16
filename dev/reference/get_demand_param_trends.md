# Get Trends (Slopes) of Demand Parameters with respect to Continuous Covariates

Computes the trend (slope) of `Q0` and/or `alpha` with respect to one or
more continuous covariates using
[`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html)
on a fitted `beezdemand_nlme` model. Trends are computed on the
parameter estimation scale (log10), consistent with how parameters are
modeled.

## Usage

``` r
get_demand_param_trends(
  fit_obj,
  params = c("Q0", "alpha"),
  covariates,
  specs = ~1,
  at = NULL,
  ci_level = 0.95,
  ...
)
```

## Arguments

- fit_obj:

  A `beezdemand_nlme` object from
  [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md).

- params:

  Character vector of parameters to analyze: any of "Q0", "alpha".
  Default `c("Q0", "alpha")`.

- covariates:

  Character vector of continuous covariate names for which to compute
  trends.

- specs:

  A formula specifying the factors over which to produce trends (e.g.,
  `~ drug` for trends by drug; `~ 1` for overall). Default `~ 1`.

- at:

  Optional named list to condition variables (factors or continuous)
  when computing trends (passed through to
  [`emmeans::ref_grid`](https://rvlenth.github.io/emmeans/reference/ref_grid.html)).

- ci_level:

  Confidence level for intervals. Default 0.95.

- ...:

  Additional args passed to
  [`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html).

## Value

A tibble combining trends for each requested parameter and covariate,
including columns for grouping factors (from `specs`), `parameter`,
`covariate`, `trend` (slope on log10 scale), and its CI (`lower.CL`,
`upper.CL`).

## Examples

``` r
if (FALSE) { # \dontrun{
trends <- get_demand_param_trends(
  fit_obj = my_fit,
  params = c("Q0", "alpha"),
  covariates = c("age", "dose_num"),
  specs = ~ drug,
  at = list(age = 21, dose_num = 0.5)
)
} # }
```
