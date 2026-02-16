# APA Theme

APA theme for ggplot

## Usage

``` r
theme_apa(plot.box = FALSE)
```

## Arguments

- plot.box:

  Boolean for a box around the plot

## Value

ggplot theme

## Details

Theme for ggplot graphics that closely align with APA formatting

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
# \donttest{
p <- ggplot2::ggplot(apt, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point() 
p + theme_apa()

# }
```
