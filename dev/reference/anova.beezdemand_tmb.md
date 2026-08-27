# Joint Wald and likelihood-ratio tests for a TMB demand fit

For a single fit, computes joint Wald-chi-square tests on grouped
fixed-effect coefficients. For multiple fits passed via `...`, performs
sequential likelihood-ratio tests on nested models.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
anova(
  object,
  ...,
  test = c("Wald", "LRT", "AIC"),
  terms = NULL,
  group_by = c("auto", "parameter", "term")
)
```

## Arguments

- object:

  A `beezdemand_tmb` fit.

- ...:

  Additional `beezdemand_tmb` fits for nested comparison.

- test:

  One of `"Wald"`, `"LRT"`, `"AIC"`. Default: `"Wald"` for a single fit,
  `"LRT"` when extra fits are supplied.

- terms:

  `NULL` (all fixed effects), a character vector of term names, or a
  named list mapping group labels to term-name vectors. Term names match
  display names (`Q0:genderMale`) or raw names.

- group_by:

  One of `"auto"` (group non-intercept terms by parameter x
  factor/covariate), `"parameter"` (one group per Q0 / alpha), or
  `"term"` (one row per coefficient).

## Value

For a single fit, a tibble with `Group`, `Chisq`, `df`, `p.value`. For
multiple fits, a tibble with `Model`, `df`, `AIC`, `Chisq`,
`` `Pr(>Chisq)` ``.

## Details

The Wald statistic for a coefficient block is \\W = \beta_g'
\Sigma\_{gg}^{-1} \beta_g\\, asymptotically \\\chi^2\\ on
`length(beta_g)` df. An exactly rank-deficient (perfectly collinear)
block has a singular \\\Sigma\_{gg}\\ and triggers an explicit error. A
near-collinear block is not detected: \\\Sigma\_{gg}\\ stays invertible
and \\W\\ becomes large and unstable, so such a value should be
interpreted with caution. For multiple fits, the likelihood-ratio test
screens for detectable non-nesting (equal or decreasing degrees of
freedom, or a larger model with lower log-likelihood) but cannot prove
nesting from log-likelihood and df alone. Pass genuinely nested models.
Rows of the multiple-fit table are ordered by ascending degrees of
freedom, and the `Model` column labels them `Model1`, `Model2`, ... in
that order.

## See also

[`anova.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/anova.beezdemand_nlme.md),
[`confint.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/confint.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt_full)
# 40 subjects per gender keep the example fast; use the full data in practice
ids <- unique(apt_full[c("id", "gender")])
ids <- ids[ids$gender %in% c("Male", "Female"), ]
keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
dat <- apt_full[apt_full$id %in% keep, ]
fit <- fit_demand_tmb(dat, equation = "exponential",
                      factors = "gender", verbose = 0)
#>   equation='exponential': Dropped 501 zero-consumption observations (859 remaining).
anova(fit)
#> # A tibble: 2 × 4
#>   Group           Chisq    df  p.value
#>   <chr>           <dbl> <int>    <dbl>
#> 1 Q0 ~ gender    11.9       1 0.000569
#> 2 alpha ~ gender  0.178     1 0.673   
anova(fit, group_by = "parameter")
#> # A tibble: 2 × 4
#>   Group Chisq    df   p.value
#>   <chr> <dbl> <int>     <dbl>
#> 1 Q0     521.     2 9.15e-114
#> 2 alpha 1305.     2 3.47e-284
# }
```
