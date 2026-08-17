# Calculate Pmax/Omax for Multiple Subjects

Calculate Pmax/Omax for Multiple Subjects

## Usage

``` r
beezdemand_calc_pmax_omax_vec(
  params_df,
  model_type,
  param_scales = NULL,
  price_list = NULL,
  consumption_list = NULL,
  p_zero_fn_list = NULL,
  ...
)
```

## Arguments

- params_df:

  Data frame with one row per subject, containing parameter columns

- model_type:

  Character: model type (same for all subjects)

- param_scales:

  Named list of parameter scales

- price_list:

  Optional list of price vectors (one per subject)

- consumption_list:

  Optional list of consumption vectors (one per subject)

- ...:

  Additional arguments passed to beezdemand_calc_pmax_omax

## Value

Data frame with pmax/omax results for each subject

## Examples

``` r
# \donttest{
params_df <- data.frame(
  alpha = c(0.001, 0.002),
  q0 = c(10, 15),
  k = c(3, 3)
)
beezdemand_calc_pmax_omax_vec(params_df, model_type = "hs")
#>   pmax_model omax_model q_at_pmax_model       method_model is_boundary_model
#> 1  17.192027   57.73857        3.358450 analytic_lambert_w             FALSE
#> 2   5.730676   28.86928        5.037675 analytic_lambert_w             FALSE
#>   elasticity_at_pmax_model unit_elasticity_pass_model pmax_unconditional
#> 1                       -1                       TRUE                 NA
#> 2                       -1                       TRUE                 NA
#>   omax_unconditional q_at_pmax_unconditional p_zero_at_pmax
#> 1                 NA                      NA             NA
#> 2                 NA                      NA             NA
#>   method_unconditional is_boundary_unconditional pmax_obs omax_obs
#> 1                 <NA>                        NA       NA       NA
#> 2                 <NA>                        NA       NA       NA
#>   has_duplicate_prices n_max_ties
#> 1                   NA         NA
#> 2                   NA         NA
# }
```
