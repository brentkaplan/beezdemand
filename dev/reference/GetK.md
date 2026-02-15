# Get K

Calculates a k value by looking for the max/min consumption across
entire dataset and adds .5 to that range

## Usage

``` r
GetK(dat, mnrange = TRUE)
```

## Arguments

- dat:

  Dataframe (long form)

- mnrange:

  Boolean for whether k should be calculated based on the mean range +
  .5

## Value

Numeric

## Details

\`r lifecycle::badge("superseded")\`

\`GetK()\` has been superseded by \[get_k()\], which provides explicit
parameters for the adjustment value and optional verbose output for
better transparency. \`GetK()\` will continue to work but is no longer
recommended for new code.

Will look for maximum/minimum greater zero

## See also

\[get_k()\] for the modern interface

## Author

Brent Kaplan \<bkaplan.ku@gmail.com\>

## Examples

``` r
GetK(apt)
#> Warning: `GetK()` was deprecated in beezdemand 0.3.0.
#> ℹ Please use `get_k()` instead.
#> [1] 1.429419
```
