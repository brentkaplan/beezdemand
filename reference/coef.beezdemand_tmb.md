# Extract Coefficients from TMB Model

Extract coefficients from a fitted `beezdemand_tmb` model. The `type`
argument selects the return shape. The default, `"internal"`, is
unchanged: a named numeric vector of the optimizer's flat
parameterization (entries include `beta_q0`, `beta_alpha`, `logsigma*`,
and any covariance hyperparameters; intercepts are on the log scale
because the optimizer works in unconstrained space). This is the
numeric-vector escape hatch consumed by tooling such as
[`car::deltaMethod`](https://rdrr.io/pkg/car/man/deltaMethod.html) and
`multcomp::glht`.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
coef(object, type = c("internal", "subject", "combined", "fixed"), ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- type:

  One of `"internal"` (default; raw optimizer vector), `"subject"` or
  its alias `"combined"` (per-subject parameter tibble), or `"fixed"`
  (one-row tibble of fixed-effect coefficients).

- ...:

  Additional arguments (currently unused; supplying `report_space` is an
  error).

## Value

For `type = "internal"`, a named numeric vector. For
`type = "subject"`/`"combined"`, a tibble with one row per subject (or
one row per subject-by-within-id-factor-level cell when the fit has
within-id factor variation). For `type = "fixed"`, a one-row tibble of
fixed-effect coefficients.

## Details

`type = "subject"` (alias `"combined"`) returns the per-subject
parameter tibble from
[`get_subject_pars`](https://brentkaplan.github.io/beezdemand/reference/get_subject_pars.md)
(with `expanded = NULL`, so within-id factor expansion is
auto-detected). This is concept-parity with
`coef.beezdemand_nlme(type = "combined")` but not column-identical: it
returns resolved per-subject parameters (`Q0`, `alpha`, ...), not a
per-design-term coefficient matrix. `type = "fixed"` returns a one-row
tibble of the fixed-effect coefficients only (the `beta_q0` /
`beta_alpha` block on the internal parameterization), excluding `log_k`,
`logsigma*`, and `rho*`.

Scale conversion is not performed here: supplying `report_space` through
`...` is an error. Use
[`get_subject_pars`](https://brentkaplan.github.io/beezdemand/reference/get_subject_pars.md)
or
[`predict.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_tmb.md)
for natural-scale parameters.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
coef(fit)                    # raw optimizer vector (default, "internal")
#>    beta_q0 beta_alpha      log_k   logsigma   logsigma logsigma_e    rho_raw 
#>  1.8736539 -5.8010932  0.8954500 -0.9527944 -0.7797945 -1.9498223 -0.4674928 
coef(fit, type = "subject")  # per-subject parameter tibble
#> # A tibble: 10 × 8
#>    id        b_i    c_i    Q0   alpha  Pmax  Omax pmax_at_bound
#>    <chr>   <dbl>  <dbl> <dbl>   <dbl> <dbl> <dbl> <lgl>        
#>  1 19     0.435  -0.614 10.1  0.00164 13.4   44.1 FALSE        
#>  2 30    -0.831   0.442  2.84 0.00471 16.6   15.4 FALSE        
#>  3 38    -0.359   0.128  4.55 0.00344 14.2   21.0 FALSE        
#>  4 60     0.394   0.111  9.66 0.00338  6.78  21.4 FALSE        
#>  5 68     0.451  -0.317 10.2  0.00220  9.83  32.8 FALSE        
#>  6 106   -0.149   0.486  5.61 0.00492  8.02  14.7 FALSE        
#>  7 113   -0.0358 -0.596  6.28 0.00167 21.1   43.4 FALSE        
#>  8 142   -0.0509 -0.450  6.19 0.00193 18.6   37.5 FALSE        
#>  9 156    0.227   0.203  8.17 0.00371  7.31  19.5 FALSE        
#> 10 188   -0.0470  0.710  6.21 0.00615  5.79  11.7 FALSE        
coef(fit, type = "fixed")    # fixed-effect coefficients
#> # A tibble: 1 × 2
#>   `Q0:(Intercept)` `alpha:(Intercept)`
#>              <dbl>               <dbl>
#> 1             1.87               -5.80
# }
```
