# Predict from TMB Mixed-Effects Demand Model

Predict from TMB Mixed-Effects Demand Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
predict(
  object,
  newdata = NULL,
  type = c("response", "parameters", "demand"),
  prices = NULL,
  scale = c("model", "natural"),
  correction = TRUE,
  ...
)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- newdata:

  Optional data frame. If NULL, predicts for original data.

- type:

  Character. One of `"response"` (fitted values on response scale),
  `"parameters"` (subject-specific parameters), or `"demand"`
  (population demand curve).

- prices:

  Optional numeric vector of prices for population prediction.

- scale:

  Character. Output scale for predictions: `"model"` returns values on
  the model's native scale (e.g., LL4-transformed for zben, log for
  exponential), while `"natural"` automatically back-transforms to the
  natural consumption scale. Default is `"model"` for backward
  compatibility.

  When `scale = "natural"` and `equation = "exponential"`, the lognormal
  retransformation correction `exp(sigma_e^2/2)` is applied by default
  to produce the conditional mean (not median). Set `correction = FALSE`
  to obtain the median (geometric mean) instead. For `"exponentiated"`
  and `"simplified"` equations, predictions are already on the natural
  scale and no correction is needed. For `"zben"`,
  [`ll4_inv()`](https://brentkaplan.github.io/beezdemand/reference/ll4_inv.md)
  is applied; because ll4_inv is nonlinear, this gives the value
  corresponding to the conditional mean on the LL4 scale (approximately
  the median on the natural scale).

- correction:

  Logical. If `TRUE` (default), applies the lognormal retransformation
  correction when `scale = "natural"`. Set to `FALSE` to obtain the
  median prediction. Only affects the `"exponential"` equation.

- ...:

  Additional arguments.

## Value

Depends on `type`:

- `"response"`: tibble with .fitted column

- `"parameters"`: tibble of subject-specific parameters

- `"demand"`: tibble with price and .fitted columns

## Note

Population-averaged (marginal) predictions integrating over the random
effects distribution are not yet implemented for this model tier. The
`type = "demand"` prediction uses RE = 0 (population fixed effects
only). For marginal integration accounting for Jensen's inequality, use
[`predict.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/predict.beezdemand_hurdle.md)
with `marginal = TRUE`.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).

# Fitted values
head(predict(fit, type = "response"))
#> # A tibble: 6 × 4
#>   id        x     y .fitted
#>   <fct> <dbl> <dbl>   <dbl>
#> 1 19      0      10    2.31
#> 2 19      0.5    10    2.26
#> 3 19      1      10    2.22
#> 4 19      1.5     8    2.17
#> 5 19      2       8    2.13
#> 6 19      2.5     8    2.08

# Population demand curve at specific prices
predict(fit, type = "demand", prices = c(0, 1, 5, 10, 20))
#> # A tibble: 5 × 2
#>   price .fitted
#>   <dbl>   <dbl>
#> 1     0  1.87  
#> 2     1  1.76  
#> 3     5  1.34  
#> 4    10  0.866 
#> 5    20  0.0382

# Subject-level parameters
head(predict(fit, type = "parameters"))
#> # A tibble: 6 × 7
#>   id       b_i    c_i    Q0   alpha  Pmax  Omax
#>   <chr>  <dbl>  <dbl> <dbl>   <dbl> <dbl> <dbl>
#> 1 19     0.435 -0.614 10.1  0.00164 13.4   44.1
#> 2 30    -0.831  0.442  2.84 0.00471 16.6   15.4
#> 3 38    -0.359  0.128  4.55 0.00344 14.2   21.0
#> 4 60     0.394  0.111  9.66 0.00338  6.78  21.4
#> 5 68     0.451 -0.317 10.2  0.00220  9.83  32.8
#> 6 106   -0.149  0.486  5.61 0.00492  8.02  14.7
# }
```
