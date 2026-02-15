# beezdemand Plot Theme

beezdemand Plot Theme

## Usage

``` r
theme_beezdemand(
  style = c("modern", "apa"),
  base_size = 11,
  base_family = "sans"
)
```

## Arguments

- style:

  Character. One of `"modern"` or `"apa"`.

- base_size:

  Base font size (default: 11).

- base_family:

  Base font family (default: "sans").

## Value

A ggplot2 theme object.

## Examples

``` r
# \donttest{
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  theme_beezdemand()

# }
```
