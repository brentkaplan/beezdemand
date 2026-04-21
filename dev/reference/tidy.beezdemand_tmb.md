# Tidy a beezdemand_tmb Model

Tidy a beezdemand_tmb Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
tidy(x, report_space = c("natural", "log10", "internal"), ...)
```

## Arguments

- x:

  A `beezdemand_tmb` object.

- report_space:

  Character. One of `"natural"`, `"log10"`, `"internal"`.

- ...:

  Additional arguments.

## Value

A tibble of model coefficients.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
tidy(fit)
#> # A tibble: 7 × 9
#>   term           estimate std.error statistic   p.value component estimate_scale
#>   <chr>             <dbl>     <dbl>     <dbl>     <dbl> <chr>     <chr>         
#> 1 Q0:(Intercept)  6.51      0.810        8.04 8.80e- 16 consumpt… natural       
#> 2 alpha:(Interc…  0.00302   0.00169      1.79 7.41e-  2 consumpt… natural       
#> 3 log_k           0.895     0.484        1.85 6.42e-  2 consumpt… log           
#> 4 logsigma_b     -0.953     0.229       -4.16 3.23e-  5 variance  natural       
#> 5 logsigma_c     -0.780     0.230       -3.39 7.04e-  4 variance  natural       
#> 6 logsigma_e     -1.95      0.0631     -30.9  6.77e-210 variance  natural       
#> 7 rho_bc_raw     -0.467     0.329       -1.42 1.56e-  1 variance  natural       
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
tidy(fit, report_space = "log10")
#> # A tibble: 7 × 9
#>   term           estimate std.error statistic   p.value component estimate_scale
#>   <chr>             <dbl>     <dbl>     <dbl>     <dbl> <chr>     <chr>         
#> 1 Q0:(Intercept)    0.814    0.0540     15.1  2.59e- 51 consumpt… log10         
#> 2 alpha:(Interc…   -2.52     0.243     -10.4  3.74e- 25 consumpt… log10         
#> 3 log_k             0.895    0.484       1.85 6.42e-  2 consumpt… log           
#> 4 logsigma_b       -0.953    0.229      -4.16 3.23e-  5 variance  natural       
#> 5 logsigma_c       -0.780    0.230      -3.39 7.04e-  4 variance  natural       
#> 6 logsigma_e       -1.95     0.0631    -30.9  6.77e-210 variance  natural       
#> 7 rho_bc_raw       -0.467    0.329      -1.42 1.56e-  1 variance  natural       
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
# }
```
