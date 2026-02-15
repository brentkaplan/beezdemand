# Extract Coefficients from Hurdle Demand Model

Extract Coefficients from Hurdle Demand Model

## Usage

``` r
# S3 method for class 'beezdemand_hurdle'
coef(object, report_space = c("natural", "log10", "internal"), ...)
```

## Arguments

- object:

  An object of class `beezdemand_hurdle`.

- report_space:

  Character. One of \`"natural"\` (default), \`"log10"\`, or
  \`"internal"\`. Default is \`"natural"\` for consistency with
  \`tidy()\`.

- ...:

  Additional arguments (currently unused).

## Value

Named numeric vector of fixed effect coefficients.
