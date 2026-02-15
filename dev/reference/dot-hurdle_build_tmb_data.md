# Build TMB Data List for Hurdle Model

Internal function to construct the TMB data list for hurdle demand
models.

## Usage

``` r
.hurdle_build_tmb_data(prepared_data, model_name, epsilon)
```

## Arguments

- prepared_data:

  List from
  [`.hurdle_prepare_data()`](https://brentkaplan.github.io/beezdemand/reference/dot-hurdle_prepare_data.md).

- model_name:

  Character string specifying the TMB model.

- epsilon:

  Small constant for log(price + epsilon) in Part I.

## Value

A list suitable for `TMB::MakeADFun(data = ...)`.
