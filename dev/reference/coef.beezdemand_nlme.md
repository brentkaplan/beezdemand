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
if (FALSE) { # \dontrun{
# Assuming 'fit_one_factor' is a successfully fitted beezdemand_nlme object
if (!is.null(fit_one_factor$model)) {
  # Get fixed effects
  fixed_coeffs <- coef(fit_one_factor, type = "fixed")
  print(fixed_coeffs)

  # Get random effects
  random_effects_summary <- coef(fit_one_factor, type = "random")
  print(random_effects_summary)

  # Get subject-specific coefficients (default)
  subject_coeffs <- coef(fit_one_factor) # or type = "combined"
  print(subject_coeffs)
}
} # }
```
