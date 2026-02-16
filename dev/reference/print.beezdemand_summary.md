# Print Method for beezdemand Summary Objects

Fallback print method for summary objects inheriting from
`beezdemand_summary`. Specific summary classes should implement their
own `print.summary.*` methods for detailed output; this provides a
minimal fallback.

## Usage

``` r
# S3 method for class 'beezdemand_summary'
print(x, ...)
```

## Arguments

- x:

  A summary object with class including `beezdemand_summary`.

- ...:

  Additional arguments (unused).

## Value

Invisibly returns `x`.
