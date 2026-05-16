# Formula for a beezdemand_hurdle fit

Returns the (currently intercept-only) component formulas plus the
random-effect specification preserved at fit time. Hurdle does not yet
support factor or covariate effects on its Part I (logit participation)
or Part II (log-consumption intensity) components; both `binary` and
`consumption` are `~ 1`. The API is intentionally aligned with
[`formula.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/formula.beezdemand_tmb.md)
so the shape extends without breaking when hurdle components gain
factor/covariate support.

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
formula(x, ...)
```

## Arguments

- x:

  A `beezdemand_hurdle` object.

- ...:

  Unused.

## Value

Named list `list(binary, consumption, random)`.

## See also

[`model.matrix.beezdemand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/model.matrix.beezdemand_hurdle.md).
