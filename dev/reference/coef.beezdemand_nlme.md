# Extract Coefficients from a beezdemand_nlme Model

Provides methods to extract fixed effects, random effects, or
subject-specific (combined fixed + random) coefficients from a
`beezdemand_nlme` object. This is an S3 method for the generic `coef`
function.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
coef(
  object,
  type = "combined",
  report_space = c("internal", "natural", "log10"),
  ...
)
```

## Arguments

- object:

  A `beezdemand_nlme` object.

- type:

  Character, type of coefficients to extract. One of:

  - `"fixed"`: Returns only fixed effects (equivalent to
    `fixef(object)`).

  - `"random"`: Returns only random effects (equivalent to
    `ranef(object)`).

  - `"combined"` (default): Returns subject-specific coefficients, where
    each subject's coefficient is the sum of the corresponding fixed
    effect and that subject's random effect deviation. This is
    equivalent to what
    [`stats::coef()`](https://rdrr.io/r/stats/coef.html) on an `nlme`
    object returns.

- report_space:

  Character. One of `"internal"` (default), `"natural"`, or `"log10"`.

- ...:

  Additional arguments passed to the underlying `nlme` coefficient
  extraction functions
  ([`nlme::fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html),
  [`nlme::ranef()`](https://rdrr.io/pkg/nlme/man/random.effects.html),
  or `stats::coef.nlme()`).

## Value

Depending on `type`:

- `type="fixed"`: A named numeric vector of fixed-effect coefficients.

- `type="random"`: A data frame (or list of data frames if multiple
  levels of grouping) of random effects, as returned by `ranef.nlme()`.

- `type="combined"`: A data frame where rows are subjects (from
  `id_var`) and columns are the Q0 and alpha parameters, representing
  subject-specific estimates (on the log10 scale).

## See also

[`fixef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/fixef.beezdemand_nlme.md),
[`ranef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/ranef.beezdemand_nlme.md)

## Examples

``` r
# \donttest{
data(ko)
fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
                        id_var = "monkey", equation_form = "zben")
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=2.27, alpha_int=-3
#> Number of fixed parameters: 2 (Q0: 1, alpha: 1)
coef(fit, type = "fixed")
#>        Q0     alpha 
#>  2.158507 -4.586304 
coef(fit, type = "random")
#>              Q0         alpha
#> A -8.206090e-12 -1.491344e-09
#> B -1.086347e-10 -2.434156e-11
#> C  1.168408e-10  1.515685e-09
coef(fit, type = "combined")
#>         Q0     alpha
#> A 2.158507 -4.586304
#> B 2.158507 -4.586304
#> C 2.158507 -4.586304
# }
```
