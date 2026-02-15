# beezdemand Color Scale (Discrete)

beezdemand Color Scale (Discrete)

## Usage

``` r
scale_color_beezdemand(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::scale_color_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

## Value

A ggplot2 discrete color scale.

## Examples

``` r
# \donttest{
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_beezdemand()

# }
```
