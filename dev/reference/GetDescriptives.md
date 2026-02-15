# Get Purchase Task Descriptive Summary

Calculates descriptive statistics from purchase task data.

## Usage

``` r
GetDescriptives(
  dat,
  bwplot = FALSE,
  outdir = "../plots/",
  device = "png",
  filename = "bwplot"
)
```

## Arguments

- dat:

  Dataframe (long form)

- bwplot:

  Boolean. If TRUE, a ggplot2 box and whisker plot is saved. Default is
  FALSE.

- outdir:

  Character. Directory where plot will be saved. Be sure to include
  trailing '/'. Default location is one level up in "../plots/".

- device:

  Character. Type of file. Default is "png". Can be "pdf".

- filename:

  Character. Specify filename. Defualt is "bwplot".

## Value

Dataframe with descriptive statistics

## Details

\`r lifecycle::badge("superseded")\`

\`GetDescriptives()\` has been superseded by
\[get_descriptive_summary()\], which provides a modern S3 interface with
standardized methods (\`print()\`, \`summary()\`, \`plot()\`).
\`GetDescriptives()\` will continue to work but is no longer recommended
for new code.

Provides the following descriptive statistics from purchase task data at
each price: mean consumption, median consumption, standard deviation of
consumption, proportion of 0 values, number of NAs, minimum consumption,
and maximum consumption.

## See also

\[get_descriptive_summary()\] for the modern interface

## Author

Brent Kaplan \<bkaplan.ku@gmail.com\>

## Examples

``` r
GetDescriptives(apt)
#> Warning: `GetDescriptives()` was deprecated in beezdemand 0.3.0.
#> ℹ Please use `get_descriptive_summary()` instead.
#>    Price Mean Median   SD PropZeros NAs Min Max
#> 1      0  6.8    6.5 2.62       0.0   0   3  10
#> 2    0.5  6.8    6.5 2.62       0.0   0   3  10
#> 3      1  6.5    6.5 2.27       0.0   0   3  10
#> 4    1.5  6.1    6.0 1.91       0.0   0   3   9
#> 5      2  5.3    5.5 1.89       0.0   0   2   8
#> 6    2.5  5.2    5.0 1.87       0.0   0   2   8
#> 7      3  4.8    5.0 1.48       0.0   0   2   7
#> 8      4  4.3    4.5 1.57       0.0   0   2   7
#> 9      5  3.9    3.5 1.45       0.0   0   2   7
#> 10     6  3.5    3.0 1.43       0.0   0   2   6
#> 11     7  3.3    3.0 1.34       0.0   0   2   6
#> 12     8  2.6    2.5 1.51       0.1   0   0   5
#> 13     9  2.4    2.0 1.58       0.1   0   0   5
#> 14    10  2.2    2.0 1.32       0.1   0   0   4
#> 15    15  1.1    0.5 1.37       0.5   0   0   3
#> 16    20  0.8    0.0 1.14       0.6   0   0   3
```
