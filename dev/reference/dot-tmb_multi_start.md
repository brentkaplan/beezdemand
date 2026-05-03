# Multi-Start TMB Optimization

Multi-Start TMB Optimization

## Usage

``` r
.tmb_multi_start(
  tmb_data,
  start_values,
  map,
  tmb_control,
  user_specified,
  verbose,
  prepared = NULL
)
```

## Arguments

- tmb_data:

  TMB data list.

- start_values:

  Default starting values list.

- map:

  TMB map list.

- tmb_control:

  Control parameters.

- user_specified:

  Character vector of user-specified tmb_control fields.

- verbose:

  Integer verbosity level.

- prepared:

  Output from .tmb_prepare_data() for data-driven start offsets.

## Value

List with obj, opt, start_used.
