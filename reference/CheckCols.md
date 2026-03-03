# Check Column Names

Checks to ensure column names are specified

## Usage

``` r
CheckCols(dat, xcol, ycol, idcol, groupcol = NULL)
```

## Arguments

- dat:

  Dataframe

- xcol:

  Name of x column

- ycol:

  Name of y column

- idcol:

  Name of id column

- groupcol:

  Name of group column

## Value

Dataframe

## Details

Check column names

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
dat <- data.frame(price = 1:5, quantity = c(10, 8, 5, 2, 0), subj = rep(1, 5))
CheckCols(dat, xcol = "price", ycol = "quantity", idcol = "subj")
#>   x  y id
#> 1 1 10  1
#> 2 2  8  1
#> 3 3  5  1
#> 4 4  2  1
#> 5 5  0  1
```
