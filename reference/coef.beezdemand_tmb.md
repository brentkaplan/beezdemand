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
#>    beta_q0 beta_alpha   logsigma   logsigma logsigma_e    rho_raw 
#>  1.8799653 -5.5720123 -0.9506009 -0.7771868 -1.9469323 -0.4593234 
coef(fit, type = "subject")  # per-subject parameter tibble
#> # A tibble: 10 × 8
#>    id        b_i    c_i    Q0   alpha  Pmax  Omax pmax_at_bound
#>    <chr>   <dbl>  <dbl> <dbl>   <dbl> <dbl> <dbl> <lgl>        
#>  1 19     0.435  -0.612 10.1  0.00206 13.9   44.1 FALSE        
#>  2 30    -0.830   0.444  2.86 0.00593 17.1   15.3 FALSE        
#>  3 38    -0.363   0.117  4.56 0.00428 14.9   21.3 FALSE        
#>  4 60     0.396   0.116  9.74 0.00427  6.98  21.3 FALSE        
#>  5 68     0.450  -0.322 10.3  0.00276 10.2   33.0 FALSE        
#>  6 106   -0.153   0.477  5.63 0.00613  8.42  14.8 FALSE        
#>  7 113   -0.0392 -0.604  6.30 0.00208 22.2   43.8 FALSE        
#>  8 142   -0.0472 -0.444  6.25 0.00244 19.0   37.3 FALSE        
#>  9 156    0.231   0.212  8.26 0.00470  7.48  19.4 FALSE        
#> 10 188   -0.0475  0.716  6.25 0.00778  5.97  11.7 FALSE        
coef(fit, type = "fixed")    # fixed-effect coefficients
#> # A tibble: 1 × 2
#>   `Q0:(Intercept)` `alpha:(Intercept)`
#>              <dbl>               <dbl>
#> 1             1.88               -5.57
# }
```
