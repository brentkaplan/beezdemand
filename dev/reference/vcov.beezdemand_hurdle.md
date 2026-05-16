# Variance-covariance matrix for a beezdemand_hurdle fit

Returns the joint fixed-effect VCOV across Part I (logit-link
participation) and Part II (log-link demand) components, with row/col
names prefixed by component (`zero_probability.<term>`,
`consumption.<term>`, `variance.<term>`). Same component classification
as
[`confint.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/confint.beezdemand_hurdle.md).

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
vcov(object, ...)
```

## Arguments

- object:

  A `beezdemand_hurdle` object.

- ...:

  Unused.

## Value

Numeric symmetric matrix with component-prefixed dim names.

## See also

[`coef.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/coef.beezdemand_hurdle.md),
[`confint.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/confint.beezdemand_hurdle.md).
