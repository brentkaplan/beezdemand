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
