# Log-Logistic Transformation (LL4-like)

Applies a log-logistic like transformation, specifically
\`log_base(x^lambda + 1) / lambda\`. This transformation is useful for
compressing data that spans several orders of magnitude while handling
zero values gracefully (as \`x=0\` yields \`0\`). It's a variation
related to the Box-Cox transformation or a generalized logarithm.

## Usage

``` r
ll4(x, lambda = 4, base = 10)
```

## Arguments

- x:

  A numeric vector or scalar of non-negative values to be transformed.

- lambda:

  A positive numeric scalar, the lambda parameter of the transformation.
  Controls the curvature. Default is \`4\`.

- base:

  A positive numeric scalar, the base of the logarithm. Default is
  \`10\`.

## Value

A numeric vector or scalar of the transformed values. Returns \`NaN\`
for \`x \< 0\` if \`lambda\` results in non-real numbers (e.g., even
root of negative). However, the intended domain is \`x \>= 0\`.

## Examples

``` r
ll4(0)
#> [1] 0
ll4(1)
#> [1] 0.0752575
ll4(10)
#> [1] 1.000011
ll4(100)
#> [1] 2
ll4(c(0, 1, 10, 100, 1000))
#> [1] 0.0000000 0.0752575 1.0000109 2.0000000 3.0000000

# Using a different lambda or base
ll4(10, lambda = 2)
#> [1] 1.002161
ll4(10, base = exp(1)) # Natural log base
#> [1] 2.30261
```
