# annotation_logticks2

Creates annotation layer

## Usage

``` r
annotation_logticks2(
  base = 10,
  sides = "bl",
  scaled = TRUE,
  short = unit(0.1, "cm"),
  mid = unit(0.2, "cm"),
  long = unit(0.3, "cm"),
  colour = "black",
  size = 0.5,
  linetype = 1,
  alpha = 1,
  data = data.frame(x = NA),
  color = NULL,
  ...
)
```

## Arguments

- base:

  base for drawing in scale

- sides:

  sides to draw, by default bottom and left

- scaled:

  true by default

- short:

  short tick settings

- mid:

  mid tick settings

- long:

  long tick settings

- colour:

  default to black colour

- size:

  size for labels

- linetype:

  default linetype

- alpha:

  default alpha level

- data:

  data to include

- color:

  colors to include

- ...:

  additional arguments

## Value

ggplot2 layer

## Details

Inherit and extend layer for use in ggplot draw

## Author

Shawn Gilroy <shawn.gilroy@temple.edu>
