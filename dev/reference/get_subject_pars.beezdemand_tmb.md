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
#> 1  19  0.4347399 -0.6136355 10.058257 0.001637264 13.439089 44.13566
#> 2  30 -0.8308979  0.4420229  2.837025 0.004705279 16.579168 15.35759
#> 3  38 -0.3588356  0.1280378  4.548595 0.003437347 14.155026 21.02253
#> 4  60  0.3938846  0.1112281  9.655604 0.003380049  6.781236 21.37890
#> 5  68  0.4514144 -0.3174826 10.227378 0.002201589  9.829030 32.82253
#> 6 106 -0.1487057  0.4857013  5.612230 0.004915352  8.022711 14.70123
# }
```
