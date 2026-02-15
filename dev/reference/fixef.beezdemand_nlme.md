# Extract Fixed Effects from a beezdemand_nlme Model

S3 method for \`fixef\` for objects of class \`beezdemand_nlme\`.
Extracts the fixed-effect coefficients from the fitted \`nlme\` model.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
fixef(object, ...)
```

## Arguments

- object:

  A \`beezdemand_nlme\` object.

- ...:

  Additional arguments passed to \`nlme::fixef()\`.

## Value

A named numeric vector of fixed-effect coefficients.

## See also

[`coef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/coef.beezdemand_nlme.md),
[`ranef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/ranef.beezdemand_nlme.md)
