# Inverse Log-Logistic Transformation (Inverse LL4-like)

Applies the inverse of the `ll4` transformation. Given `y = ll4(x)`,
this function calculates `x = (base^(y * lambda) - 1)^(1/lambda)`.

## Usage

``` r
ll4_inv(y, lambda = 4, base = 10)
```

## Arguments

- y:

  A numeric vector or scalar of transformed values (output from `ll4`).

- lambda:

  A positive numeric scalar, the lambda parameter used in the original
  `ll4` transformation. Must match the one used for the forward
  transform. Default is `4`.

- base:

  A positive numeric scalar, the base of the logarithm used in the
  original `ll4` transformation. Must match. Default is `10`.

## Value

A numeric vector or scalar of the original, untransformed values.
Returns `0` when the intermediate quantity `base^(y * lambda) - 1` is
negative (i.e., when `y < 0` for `base = 10` and even `lambda`), since
consumption cannot be negative.

## Details

**Domain and boundary behavior.** The inverse LL4 transformation is
defined for `y >= 0` (when `base = 10` and `lambda = 4`). For `y < 0`,
the intermediate quantity `base^(y * lambda) - 1` becomes negative, and
raising a negative number to the fractional power `1/lambda` is
undefined in real arithmetic. In this case, the function returns `0`
(consumption cannot be negative).

This boundary condition arises in practice when a model predicts fitted
values below zero on the LL4 scale — typically for extrapolation to very
high prices. The mapping to zero is the natural floor because
`ll4(0) = 0` and the LL4 transformation is monotonically increasing on
`[0, Inf)`.

## Examples

``` r
original_values <- c(0, 1, 10, 100, 1000)
transformed_values <- ll4(original_values)
back_transformed_values <- ll4_inv(transformed_values)
print(data.frame(original_values, transformed_values, back_transformed_values))
#>   original_values transformed_values back_transformed_values
#> 1               0          0.0000000                       0
#> 2               1          0.0752575                       1
#> 3              10          1.0000109                      10
#> 4             100          2.0000000                     100
#> 5            1000          3.0000000                    1000
all.equal(original_values, back_transformed_values) # Should be TRUE or very close
#> [1] TRUE

# Negative y values are mapped to 0 (consumption floor)
ll4_inv(-0.5, lambda = 4, base = 10) # Returns 0
#> [1] 0
```
