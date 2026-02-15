# Extract Random Effects from a beezdemand_nlme Model

S3 method for \`ranef\` for objects of class \`beezdemand_nlme\`.
Extracts the random effects from the fitted \`nlme\` model.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
ranef(object, ...)
```

## Arguments

- object:

  A \`beezdemand_nlme\` object.

- ...:

  Additional arguments passed to \`nlme::ranef()\`.

## Value

A data frame (or list of data frames if multiple levels of grouping) of
random effects, as returned by \`ranef.nlme()\`.

## See also

[`coef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/coef.beezdemand_nlme.md),
[`fixef.beezdemand_nlme`](https://brentkaplan.github.io/beezdemand/reference/fixef.beezdemand_nlme.md)
