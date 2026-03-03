# Create a Pseudo-Log LL4 Transformation Object for ggplot2

Generates a
[`scales::trans`](https://scales.r-lib.org/reference/new_transform.html)
object using the `ll4` transformation. This transformation object can be
passed to the `trans` argument of
[`ggplot2::scale_x_continuous`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
or
[`ggplot2::scale_y_continuous`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).
It's designed for non-negative data and handles zero values gracefully.
The "pseudo" aspect is conceptual, similar to `pseudo_log_trans` in that
it handles a range including zero, but the transformation is `ll4`.

## Usage

``` r
pseudo_ll4_trans(lambda = 4)
```

## Arguments

- lambda:

  A positive numeric scalar, the lambda parameter for the `ll4`
  transformation. Default is `4`.

## Value

A `trans` object (from the `scales` package).

## See also

[`ll4`](https://brentkaplan.github.io/beezdemand/reference/ll4.md),
[`ll4_inv`](https://brentkaplan.github.io/beezdemand/reference/ll4_inv.md),
[`trans_new`](https://scales.r-lib.org/reference/new_transform.html)

## Examples

``` r
# \donttest{
if (require(ggplot2) && require(scales)) {
  set.seed(123)
  df <- data.frame(
    x_vals = c(0, 0.01, 0.1, 1, 10, 100, 1000, NA), # Include 0 and NA
    y_vals = c(0, 10, 50, 100, 500, 1000, 2000, 50)
  )

  # Using pseudo_ll4_trans for the y-axis
  ggplot(df, aes(x = x_vals, y = y_vals)) +
    geom_point() +
    scale_y_continuous(trans = pseudo_ll4_trans(lambda = 4),
                       name = "Y-Values (Pseudo-LL4 Scale)") +
    ggtitle("Y-Axis with Pseudo-LL4 Transformation")

  # Using pseudo_ll4_trans for the x-axis
  ggplot(df, aes(x = x_vals, y = y_vals)) +
    geom_point() +
    scale_x_continuous(trans = pseudo_ll4_trans(lambda = 2), # Different lambda
                       name = "X-Values (Pseudo-LL4 Scale)") +
    ggtitle("X-Axis with Pseudo-LL4 Transformation")
}
#> Loading required package: scales
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).

# }
```
