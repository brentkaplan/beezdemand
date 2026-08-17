# Design matrices for a beezdemand_tmb fit

By default returns a named list of all four design matrices the TMB
template consumed: `X_q0`, `X_alpha`, `Z_q0`, `Z_alpha`. Use `what` to
select a single matrix. `X_q0` and `X_alpha` are zero-copy references to
`fit$formula_details`; `Z_q0` and `Z_alpha` are recomputed via the
internal builder.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
model.matrix(object, what = NULL, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- what:

  `NULL` (default) returns the full named list. Otherwise one of
  `"X_q0"`, `"X_alpha"`, `"Z_q0"`, `"Z_alpha"`.

- ...:

  Unused.

## Value

Named list of numeric matrices, or a single numeric matrix when `what`
is set. `NULL` (with a message) when a degenerate Z is requested.

## Details

Returning a named list (vs the single matrix `lm`/`lme4` return) is
intentional: the TMB tier has two fixed-effect linear predictors (one
per nonlinear parameter), not one.

## See also

[`formula.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/formula.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
str(model.matrix(fit))
#> List of 4
#>  $ X_q0   : num [1:146, 1] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:146] "1" "2" "3" "4" ...
#>   .. ..$ : chr "(Intercept)"
#>   ..- attr(*, "assign")= int 0
#>  $ X_alpha: num [1:146, 1] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:146] "1" "2" "3" "4" ...
#>   .. ..$ : chr "(Intercept)"
#>   ..- attr(*, "assign")= int 0
#>  $ Z_q0   : num [1:146, 1] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:146] "1" "2" "3" "4" ...
#>   .. ..$ : chr "(Intercept)"
#>  $ Z_alpha: num [1:146, 1] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:146] "1" "2" "3" "4" ...
#>   .. ..$ : chr "(Intercept)"
head(model.matrix(fit, what = "X_q0"))
#>   (Intercept)
#> 1           1
#> 2           1
#> 3           1
#> 4           1
#> 5           1
#> 6           1
# }
```
