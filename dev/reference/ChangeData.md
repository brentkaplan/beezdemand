# ChangeData

Changes demand data

## Usage

``` r
ChangeData(
  dat,
  nrepl = 1,
  replnum = 0.01,
  rem0 = FALSE,
  remq0e = FALSE,
  replfree = NULL,
  xcol = "x",
  ycol = "y",
  idcol = "id"
)
```

## Arguments

- dat:

  A long form dataframe

- nrepl:

  Number of zeros to replace with replacement value (replnum). Can
  accept either a number or "all" if all zeros should be replaced.
  Default is to replace the first zero only

- replnum:

  Value to replace zeros. Default is .01

- rem0:

  If TRUE, removes all 0s in consumption data prior to analysis. Default
  value is FALSE

- remq0e:

  If TRUE, removes consumption and price where price == 0. Default value
  is FALSE

- replfree:

  Optionally replaces price == 0 with specified value.

- xcol:

  Column name in dataframe that signifies x values (usually price or the
  IV)

- ycol:

  Column name in dataframe that signifies y values (usually consumption
  or the DV)

- idcol:

  Column name in dataframe that signifies identifying id grouping

## Value

Long form dataframe resembling the originally provided dataframe

## Details

Change demand data in various ways. Ways include replacing any number of
0 values with a replacement number (or remove them completely), removing
price and consumption at free, replacing free with some number. This
will soon replace ReplaceZeros and certain arguments in FitCurves.

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## Change just the first instance of 0 within each unique value of id with .1
ChangeData(apt, nrepl = 1, replnum = .1)
#>      id    x    y
#> 1    19  0.0 10.0
#> 2    19  0.5 10.0
#> 3    19  1.0 10.0
#> 4    19  1.5  8.0
#> 5    19  2.0  8.0
#> 6    19  2.5  8.0
#> 7    19  3.0  7.0
#> 8    19  4.0  7.0
#> 9    19  5.0  7.0
#> 10   19  6.0  6.0
#> 11   19  7.0  6.0
#> 12   19  8.0  5.0
#> 13   19  9.0  5.0
#> 14   19 10.0  4.0
#> 15   19 15.0  3.0
#> 16   19 20.0  2.0
#> 17   30  0.0  3.0
#> 18   30  0.5  3.0
#> 19   30  1.0  3.0
#> 20   30  1.5  3.0
#> 21   30  2.0  2.0
#> 22   30  2.5  2.0
#> 23   30  3.0  2.0
#> 24   30  4.0  2.0
#> 25   30  5.0  2.0
#> 26   30  6.0  2.0
#> 27   30  7.0  2.0
#> 28   30  8.0  2.0
#> 29   30  9.0  1.0
#> 30   30 10.0  1.0
#> 31   30 15.0  1.0
#> 32   30 20.0  1.0
#> 33   38  0.0  4.0
#> 34   38  0.5  4.0
#> 35   38  1.0  4.0
#> 36   38  1.5  4.0
#> 37   38  2.0  4.0
#> 38   38  2.5  4.0
#> 39   38  3.0  4.0
#> 40   38  4.0  3.0
#> 41   38  5.0  3.0
#> 42   38  6.0  3.0
#> 43   38  7.0  3.0
#> 44   38  8.0  2.0
#> 45   38  9.0  2.0
#> 46   38 10.0  2.0
#> 47   38 15.0  0.1
#> 48   38 20.0  0.0
#> 49   60  0.0 10.0
#> 50   60  0.5 10.0
#> 51   60  1.0  8.0
#> 52   60  1.5  8.0
#> 53   60  2.0  6.0
#> 54   60  2.5  6.0
#> 55   60  3.0  5.0
#> 56   60  4.0  5.0
#> 57   60  5.0  4.0
#> 58   60  6.0  4.0
#> 59   60  7.0  3.0
#> 60   60  8.0  3.0
#> 61   60  9.0  2.0
#> 62   60 10.0  2.0
#> 63   60 15.0  0.1
#> 64   60 20.0  0.0
#> 65   68  0.0 10.0
#> 66   68  0.5 10.0
#> 67   68  1.0  9.0
#> 68   68  1.5  9.0
#> 69   68  2.0  8.0
#> 70   68  2.5  8.0
#> 71   68  3.0  7.0
#> 72   68  4.0  6.0
#> 73   68  5.0  5.0
#> 74   68  6.0  5.0
#> 75   68  7.0  5.0
#> 76   68  8.0  4.0
#> 77   68  9.0  4.0
#> 78   68 10.0  3.0
#> 79   68 15.0  0.1
#> 80   68 20.0  0.0
#> 81  106  0.0  5.0
#> 82  106  0.5  5.0
#> 83  106  1.0  5.0
#> 84  106  1.5  5.0
#> 85  106  2.0  4.0
#> 86  106  2.5  4.0
#> 87  106  3.0  4.0
#> 88  106  4.0  3.0
#> 89  106  5.0  3.0
#> 90  106  6.0  2.0
#> 91  106  7.0  2.0
#> 92  106  8.0  0.1
#> 93  106  9.0  0.0
#> 94  106 10.0  0.0
#> 95  106 15.0  0.0
#> 96  106 20.0  0.0
#> 97  113  0.0  6.0
#> 98  113  0.5  6.0
#> 99  113  1.0  6.0
#> 100 113  1.5  6.0
#> 101 113  2.0  5.0
#> 102 113  2.5  5.0
#> 103 113  3.0  5.0
#> 104 113  4.0  5.0
#> 105 113  5.0  5.0
#> 106 113  6.0  5.0
#> 107 113  7.0  4.0
#> 108 113  8.0  4.0
#> 109 113  9.0  4.0
#> 110 113 10.0  4.0
#> 111 113 15.0  3.0
#> 112 113 20.0  2.0
#> 113 142  0.0  8.0
#> 114 142  0.5  8.0
#> 115 142  1.0  8.0
#> 116 142  1.5  6.0
#> 117 142  2.0  6.0
#> 118 142  2.5  5.0
#> 119 142  3.0  5.0
#> 120 142  4.0  4.0
#> 121 142  5.0  3.0
#> 122 142  6.0  3.0
#> 123 142  7.0  3.0
#> 124 142  8.0  3.0
#> 125 142  9.0  3.0
#> 126 142 10.0  3.0
#> 127 142 15.0  3.0
#> 128 142 20.0  3.0
#> 129 156  0.0  7.0
#> 130 156  0.5  7.0
#> 131 156  1.0  7.0
#> 132 156  1.5  7.0
#> 133 156  2.0  6.0
#> 134 156  2.5  6.0
#> 135 156  3.0  5.0
#> 136 156  4.0  5.0
#> 137 156  5.0  4.0
#> 138 156  6.0  3.0
#> 139 156  7.0  3.0
#> 140 156  8.0  2.0
#> 141 156  9.0  2.0
#> 142 156 10.0  2.0
#> 143 156 15.0  1.0
#> 144 156 20.0  0.1
#> 145 188  0.0  5.0
#> 146 188  0.5  5.0
#> 147 188  1.0  5.0
#> 148 188  1.5  5.0
#> 149 188  2.0  4.0
#> 150 188  2.5  4.0
#> 151 188  3.0  4.0
#> 152 188  4.0  3.0
#> 153 188  5.0  3.0
#> 154 188  6.0  2.0
#> 155 188  7.0  2.0
#> 156 188  8.0  1.0
#> 157 188  9.0  1.0
#> 158 188 10.0  1.0
#> 159 188 15.0  0.1
#> 160 188 20.0  0.0
```
