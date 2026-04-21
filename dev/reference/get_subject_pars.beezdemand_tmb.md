# Get Subject-Specific Parameters from TMB Model

Get Subject-Specific Parameters from TMB Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
get_subject_pars(object, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Additional arguments (currently unused).

## Value

A data frame with columns: id, b_i, c_i (if 2 RE), Q0, alpha, Pmax,
Omax.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(get_subject_pars(fit))
#>    id        b_i        c_i        Q0       alpha      Pmax     Omax
#> 1  19  0.4347393 -0.6136353 10.058257 0.001637265 13.439090 44.13565
#> 2  30 -0.8308985  0.4420231  2.837025 0.004705282 16.579171 15.35759
#> 3  38 -0.3588363  0.1280380  4.548595 0.003437349 14.155029 21.02253
#> 4  60  0.3938840  0.1112283  9.655605 0.003380052  6.781236 21.37890
#> 5  68  0.4514138 -0.3174823 10.227379 0.002201591  9.829030 32.82252
#> 6 106 -0.1487063  0.4857014  5.612231 0.004915356  8.022712 14.70123
# }
```
