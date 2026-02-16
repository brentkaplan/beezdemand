# ExtraF

Extra Sum of Squares F-test

## Usage

``` r
ExtraF(
  dat,
  equation = "hs",
  groups = NULL,
  verbose = FALSE,
  k,
  compare = "alpha",
  idcol = "id",
  xcol = "x",
  ycol = "y",
  groupcol = NULL,
  start_alpha = 0.001
)
```

## Arguments

- dat:

  Long form data frame

- equation:

  "hs"

- groups:

  NULL for all. Character vector matching groups in groupcol

- verbose:

  If TRUE, prints all output including models

- k:

  User-defined k value; if missing will attempt to find shared k and
  then mean emprirical range (in log units)

- compare:

  Specify whether to compare alpha or Q0. Default is alpha

- idcol:

  The column name that should be treated as dataset identifier

- xcol:

  The column name that should be treated as "x" data

- ycol:

  The column name that should be treated as "y" data

- groupcol:

  The column name that should be treated as the groups

- start_alpha:

  Optional numeric to inform starting value for alpha

## Value

List of results and models

## Details

One alpha better than individual alphas?

## Author

Brent Kaplan <bkaplan.ku@gmail.com>

## Examples

``` r
## Compare two groups using equation by Koffarnus et al., 2015 and a fixed k of 2
# \donttest{
apt$group <- NA
apt[apt$id %in% sample(unique(apt$id), length(unique(apt$id))/2), "group"] <- "a"
apt$group[is.na(apt$group)] <- "b"
ExtraF(apt, "koff", k = 2, groupcol = "group")
#> [1] "Null hypothesis: alpha same for all data sets"
#> [1] "Alternative hypothesis: alpha different for each data set"
#> [1] "Conclusion: reject the null hypothesis"
#> [1] "F(1,156) = 7.4511, p = 0.0071"
# }
```
