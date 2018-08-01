# Behavioral Economic (be) Easy (ez) Demand
An R package containing commonly used functions for analyzing behavioral economic demand curve data.

### Installation
----------------

To install this stable version (0.1.0) of `beezdemand` from CRAN, along with dependent packages:
```r
install.packages("beezdemand", dependencies = TRUE)

library(beezdemand)
```

To install a stable release directly from [GitHub](http://github.com/brentkaplan/beezdemand), first install and load the devtools package. Then, use `install_github` to install the package and associated vignette.

```r
install.packages("devtools")

devtools::install_github("brentkaplan/beezdemand", build_vignettes = TRUE)

library(beezdemand)
```

To install the development version of the package, specify the development branch in `install_github`:

```r
devtools::install_github("brentkaplan/beezdemand@develop")
```

#### Note About Use
Currently, this version (0.1.0) is the first minor release and is stable. I encourage you to use it,
but be aware that there might be (unknown) bugs present. I've tried hard to make this version usable while including the core functionality (described more below). However, if you find issues or would like to contribute, please open an issue on this GitHub page or contact me at <bkaplan.ku@gmail.com>.

### Sample Implementation
-------------------------

Some of the following information can also be found in the package's vignette. I try to keep both up to date.

#### Example dataset provided
An example dataset of responses on an Alcohol Purchase Task is provided. These data are a subset of from the paper by [Kaplan & Reed (2018)](http://psycnet.apa.org/record/2018-02774-001). Participants (id)
reported the number of alcoholic drinks (y) they would be willing to
purchase and consume at various prices (x; USD). Note the
long format.

```r
>apt[c(1:8, 17:24), ]
id   x  y
1  19 0.0 10
2  19 0.5 10
3  19 1.0 10
4  19 1.5  8
5  19 2.0  8
6  19 2.5  8
7  19 3.0  7
8  19 4.0  7
17 30 0.0  3
18 30 0.5  3
19 30 1.0  3
20 30 1.5  3
21 30 2.0  2
22 30 2.5  2
23 30 3.0  2
24 30 4.0  2
```

#### Converting from Wide to Long and vice versa

Some datasets read into `R` will be in a "wide" format, where column names indicate dataset identifiers:
```r
> wide <- tidyr::spread(apt, id, y)
> wide
      x 19 30 38 60 68 106 113 142 156 188
1   0.0 10  3  4 10 10   5   6   8   7   5
2   0.5 10  3  4 10 10   5   6   8   7   5
3   1.0 10  3  4  8  9   5   6   8   7   5
4   1.5  8  3  4  8  9   5   6   6   7   5
..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..
16 20.0  2  1  0  0  0   0   2   3   0   0
```

The functions in this package primarily deal with data that are in long format, for example the provided dataset `apt`. In order to convert from wide to long formats and vice versa, I recommend using the following commands.

__Wide to Long__
```r
> long <- tidyr::gather(wide, id, y, -x)
> long
       x  id  y
1    0.0  19 10
2    0.5  19 10
3    1.0  19 10
4    1.5  19  8
5    2.0  19  8
6    2.5  19  8
7    3.0  19  7
8    4.0  19  7
9    5.0  19  7
10   6.0  19  6
11   7.0  19  6
12   8.0  19  5
13   9.0  19  5
14  10.0  19  4
15  15.0  19  3
16  20.0  19  2
```

__Long to Wide__
```r
> wide <- tidyr::spread(long, id, y)
> wide
      x 106 113 142 156 188 19 30 38 60 68
1   0.0   5   6   8   7   5 10  3  4 10 10
2   0.5   5   6   8   7   5 10  3  4 10 10
3   1.0   5   6   8   7   5 10  3  4  8  9
4   1.5   5   6   6   7   5  8  3  4  8  9
5   2.0   4   5   6   6   4  8  2  4  6  8
6   2.5   4   5   5   6   4  8  2  4  6  8
7   3.0   4   5   5   5   4  7  2  4  5  7
8   4.0   3   5   4   5   3  7  2  3  5  6
9   5.0   3   5   3   4   3  7  2  3  4  5
10  6.0   2   5   3   3   2  6  2  3  4  5
11  7.0   2   4   3   3   2  6  2  3  3  5
12  8.0   0   4   3   2   1  5  2  2  3  4
13  9.0   0   4   3   2   1  5  1  2  2  4
14 10.0   0   4   3   2   1  4  1  2  2  3
15 15.0   0   3   3   1   0  3  1  0  0  0
16 20.0   0   2   3   0   0  2  1  0  0  0
```

#### Obtain descriptive summary
Descriptive values of responses at each price. Includes mean, standard
deviation, proportion of zeros, numer of NAs, minimum, and maximum.

```r
> GetDescriptives(apt)
   Price Mean Median   SD PropZeros NAs Min Max
1      0  6.8    6.5 2.62       0.0   0   3  10
2    0.5  6.8    6.5 2.62       0.0   0   3  10
3      1  6.5    6.5 2.27       0.0   0   3  10
4    1.5  6.1    6.0 1.91       0.0   0   3   9
5      2  5.3    5.5 1.89       0.0   0   2   8
6    2.5  5.2    5.0 1.87       0.0   0   2   8
7      3  4.8    5.0 1.48       0.0   0   2   7
8      4  4.3    4.5 1.57       0.0   0   2   7
9      5  3.9    3.5 1.45       0.0   0   2   7
10     6  3.5    3.0 1.43       0.0   0   2   6
11     7  3.3    3.0 1.34       0.0   0   2   6
12     8  2.6    2.5 1.51       0.1   0   0   5
13     9  2.4    2.0 1.58       0.1   0   0   5
14    10  2.2    2.0 1.32       0.1   0   0   4
15    15  1.1    0.5 1.37       0.5   0   0   3
16    20  0.8    0.0 1.14       0.6   0   0   3
```

#### Apply algorithm for identifying unsystematic responses
Examine consistency of demand data using Stein et al.'s (2015) alogrithm for identifying unsystematic
responses. Default values shown.


```r
> head(CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.1, reversals = 0, ncons0 = 2), 5)
  id TotalPass DeltaQ DeltaQPass Bounce BouncePass Reversals ReversalsPass
1 19         3 0.2112       Pass      0       Pass         0          Pass
2 30         3 0.1437       Pass      0       Pass         0          Pass
3 38         3 0.7885       Pass      0       Pass         0          Pass
4 60         3 0.9089       Pass      0       Pass         0          Pass
5 68         3 0.9089       Pass      0       Pass         0          Pass
  NumPosValues
1           16
2           16
3           14
4           14
5           14
```

#### Analyze Demand Data

Results of the analysis return both empirical (observed) and derived measures for
use in additional analyses and model specification. Equations include
the linear model, exponential model, and exponentiated model. Soon, I
will be including the nonlinear mixed effects model, mixed effects
versions of the exponential and exponentiated model, and others.

##### Obtaining Empirical Measures

Empirical measures can be obtained separately on their own. When used within the `FitCurves` function, empirical measures will reflect any data modifications (e.g., replacing 0 values).

```r
> head(GetEmpirical(apt), 5)
  id Intensity BP0 BP1 Omaxe Pmaxe
1 19        10  NA  20    45    15
2 30         3  NA  20    20    20
3 38         4  15  10    21     7
4 60        10  15  10    24     8
5 68        10  15  10    36     9
```

##### Analyze demand data using either Exponential/Exponentiated models
Results of the analysis return both empirical and derived measures for
use in additional analyses and model specification. Refer to the function's documentation for use of passing additional arguments (e.g., aggregating, bounding). Pay special attention for the argument `detailed = TRUE`, which returns a list of objects useful for other operations such as plotting.

```r
> head(FitCurves(apt, "hs"), 3)
No k value specified. Defaulting to empirical mean range +.5
  id Intensity BP0 BP1 Omaxe Pmaxe Equation       Q0d        K       Alpha
1 19        10  NA  20    45    15       hs 10.475734 1.031479 0.004657143
2 30         3  NA  20    20    20       hs  2.932407 1.031479 0.013455713
3 38         4  NA  10    21     7       hs  4.523155 1.031479 0.008793484
         R2      Q0se      Alphase  N      AbsSS      SdRes    Q0Low    Q0High
1 0.9660008 0.4159581 0.0002357623 16 0.01933543 0.03716319 9.583592 11.367875
2 0.7922379 0.2506946 0.0017320590 16 0.09783499 0.08359553 2.394720  3.470093
3 0.8662632 0.2357693 0.0008877504 14 0.02590834 0.04646535 4.009458  5.036852
     AlphaLow   AlphaHigh        EV    Omaxd    Pmaxd     Notes
1 0.004151484 0.005162803 2.0496979 45.49394 14.39311 converged
2 0.009740816 0.017170610 0.7094189 15.74586 17.79622 converged
3 0.006859242 0.010727726 1.0855466 24.09418 17.65453 converged
Warning message:
Zeros found in data not compatible with equation! Dropping zeros!

> head(FitCurves(apt, "koff"), 3)
No k value specified. Defaulting to empirical mean range +.5
  id Intensity BP0 BP1 Omaxe Pmaxe Equation       Q0d        K       Alpha
1 19        10  NA  20    45    15     koff 10.131767 1.429419 0.002931890
2 30         3  NA  20    20    20     koff  2.989613 1.429419 0.009371645
3 38         4  15  10    21     7     koff  4.607551 1.429419 0.007056167
         R2      Q0se      Alphase  N    AbsSS     SdRes    Q0Low    Q0High
1 0.9668576 0.2438729 0.0001662994 16 2.908243 0.4557758 9.608712 10.654822
2 0.8136932 0.1721284 0.0013099679 16 1.490454 0.3262837 2.620434  3.358791
3 0.8403625 0.3078231 0.0010630760 16 4.429941 0.5625161 3.947336  5.267766
     AlphaLow   AlphaHigh        EV    Omaxd    Pmaxd     Notes
1 0.002575213 0.003288567 1.9957818 46.56622 15.14091 converged
2 0.006562044 0.012181247 0.6243741 14.56810 16.05291 converged
3 0.004776096 0.009336238 0.8292622 19.34861 13.83393 converged
```

The output of `FitCurves` contains important outputs:

```r
> fc <- FitCurves(apt, "hs")
```

###### Empirical Measures

```r
 > head(fc, 5)[ , 1:6]
  id Intensity BP0 BP1 Omaxe Pmaxe
1 19        10  NA  20    45    15
2 30         3  NA  20    20    20
3 38         4  NA  10    21     7
4 60        10  NA  10    24     8
5 68        10  NA  10    36     9
```

###### Fitted Measures

```r
> head(fc, 5)[ , 7:11]
  Equation       Q0d        K       Alpha        R2
1       hs 10.475734 1.031479 0.004657143 0.9660008
2       hs  2.932407 1.031479 0.013455713 0.7922379
3       hs  4.523155 1.031479 0.008793484 0.8662632
4       hs 10.492134 1.031479 0.010223101 0.9664814
5       hs 10.651761 1.031479 0.006126171 0.9699408
```

###### Uncertainty and Model Information

```r
> head(fc, 5)[ , 12:20]
       Q0se      Alphase  N      AbsSS      SdRes    Q0Low    Q0High    AlphaLow
1 0.4159581 0.0002357623 16 0.01933543 0.03716319 9.583592 11.367875 0.004151484
2 0.2506946 0.0017320590 16 0.09783499 0.08359553 2.394720  3.470093 0.009740816
3 0.2357693 0.0008877504 14 0.02590834 0.04646535 4.009458  5.036852 0.006859242
4 0.6219725 0.0005117874 14 0.02366515 0.04440829 9.136972 11.847296 0.009108012
5 0.3841063 0.0002713043 14 0.01094388 0.03019917 9.814865 11.488656 0.005535049
    AlphaHigh
1 0.005162803
2 0.017170610
3 0.010727726
4 0.011338190
5 0.006717292
```

###### Derived Measures

```r
> head(fc, 5)[ , 21:24]
         EV    Omaxd     Pmaxd     Notes
1 2.0496979 45.49394 14.393110 converged
2 0.7094189 15.74586 17.796221 converged
3 1.0855466 24.09418 17.654534 converged
4 0.9337418 20.72481  6.546546 converged
5 1.5581899 34.58471 10.760891 converged
```

#### Share k globally while fitting other parameters locally
Provides the ability to share k globally (across all participants)
while estimating Q<sub>0</sub> and alpha locally.

```r
> head(FitCurves(apt, "hs", k = "share"), 3)
Beginning search for best-starting k
Best k fround at 0.93813356574003 = err: 0.744881846162718
Searching for shared K, this can take a while...
  id Intensity BP0 BP1 Omaxe Pmaxe Equation       Q0d        K       Alpha
1 19        10  NA  20    45    15       hs 10.014577 3.318335 0.001161591
2 30         3  NA  20    20    20       hs  2.766313 3.318335 0.003333121
3 38         4  NA  10    21     7       hs  4.485810 3.318335 0.002457986
         R2      Q0se      Alphase  N      AbsSS      SdRes    Q0Low    Q0High
1 0.9820968 0.2429150 3.081357e-05 16 0.01018161 0.02696772 9.493576 10.535578
2 0.7641766 0.2192797 3.738918e-04 16 0.11104904 0.08906219 2.296005  3.236621
3 0.8803145 0.2074990 1.963343e-04 14 0.02318624 0.04395664 4.033709  4.937912
     AlphaLow   AlphaHigh        EV    Omaxd    Pmaxd     Notes
1 0.001095502 0.001227679 1.4241851 44.55169 13.16054 converged
2 0.002531203 0.004135039 0.4963278 15.52624 16.60378 converged
3 0.002030210 0.002885762 0.6730390 21.05416 13.88478 converged
Warning message:
Zeros found in data not compatible with equation! Dropping zeros!

> head(FitCurves(apt, "koff", k = "share"), 3)
Beginning search for best-starting k
Best k fround at 0.888177169408706 = err: 220.834181088202
Searching for shared K, this can take a while...
  id Intensity BP0 BP1 Omaxe Pmaxe Equation      Q0d       K        Alpha
1 19        10  NA  20    45    15     koff 9.978965 5.60517 0.0006678335
2 30         3  NA  20    20    20     koff 2.934651 5.60517 0.0021193395
3 38         4  15  10    21     7     koff 4.594258 5.60517 0.0016454844
         R2      Q0se      Alphase  N    AbsSS     SdRes    Q0Low    Q0High
1 0.9673407 0.2268256 3.393702e-05 16 2.865854 0.4524421 9.492472 10.465457
2 0.8030401 0.1656446 2.782734e-04 16 1.575679 0.3354825 2.579378  3.289923
3 0.8699897 0.2619979 1.959469e-04 16 3.607787 0.5076407 4.032328  5.156187
      AlphaLow    AlphaHigh        EV    Omaxd    Pmaxd     Notes
1 0.0005950458 0.0007406212 1.1283624 44.46417 12.61035 converged
2 0.0015225024 0.0027161766 0.3555628 14.01128 13.51213 converged
3 0.0012252201 0.0020657487 0.4579552 18.04615 11.11660 converged
```

#### Extra Sum of Squares F Test
When one has multiple groups, it may be beneficial to compare whether separate curves are preferred over a single curve. This is accomplished by the Extra Sum-of-Squares F-test. This function (using the argument `compare`) will determine whether a single alpha or a single Q<sub>0</sub> is better than multiple alphas or Q<sub>0</sub>. A single curve will be fit, the residual deviations calculated and those residuals are compared to residuals obtained from multiple curves. A resulting _F_ statistic will be reporting along with a _p_ value.


```r
> set.seed(1234)
> apt$group <- NA
> apt[apt$id %in% sample(unique(apt$id), length(unique(apt$id))/2), "group"] <- "a"
> apt$group[is.na(apt$group)] <- "b"
> ExtraF(apt, "koff", k = 2, groupcol = "group")
[1] "Null hypothesis: alpha same for all data sets"
[1] "Alternative hypothesis: alpha different for each data set"
[1] "Conclusion: fail to reject the null hypothesis"
[1] "F(1,156) = 2.8929, p = 0.091"
```

#### Plots

Plots can be created using the `PlotCurves` function. This function takes the output from `FitCurves` when the argument from `FitCurves`, `detailed = TRUE`. The default will be to save figures into a temporary folder, but the user can specify a different directory. Plots can be saved as either PNG or PDF. Plots can also be shown interactively and not saved, and can be automatically created at both an aggregate and individual level.

#### See function details
To learn more about a function and what arguments it takes, type "?"
in front of the function name.

```r
> ?CheckUnsystematic
CheckUnsystematic          package:beezdemand          R Documentation

Systematic Purchase Task Data Checker

Description:

     Applies Stein, Koffarnus, Snider, Quisenberry, & Bickel's (2015)
     criteria for identification of nonsystematic purchase task data.
     ...
```

### Acknowledgments
-------------------

- Shawn P. Gilroy, Contributor [GitHub](https://github.com/miyamot0)

- Derek D. Reed, Applied Behavioral Economics Laboratory
(www.behavioraleconlab.com)

- Paul E. Johnson, Center for Research Methods and Data Analysis, University of Kansas
(www.crmda.ku.edu)

- W. Brady DeHart (Virginia Tech Carilion Research Institute)

- Michael Amlung, Cognitive Neuroscience of Addictions Laboratory
(www.cnalab.weebly.com)

- Peter G. Roma, Institutes for Behavior Resources, Inc.
(www.ibrinc.org)

- Steven R. Hursh, Institutes for Behavior Resources, Inc.
(www.ibrinc.org)


### Recommended Readings
------------------------

- Reed, D. D., Niileksela, C. R., & Kaplan, B. A. (2013). Behavioral economics: A tutorial for behavior analysts in practice. *Behavior Analysis in Practice, 6* (1), 34–54.

- Reed, D. D., Kaplan, B. A., & Becirevic, A. (2015). Basic research on the behavioral economics of reinforcer value. In *Autism Service Delivery* (pp. 279-306). Springer New York.

- Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential value. *Psychological Review, 115* (1), 186-198.

- Koffarnus, M. N., Franck, C. T., Stein, J. S., & Bickel, W. K. (2015). A modified exponential behavioral economic demand model to better describe consumption data. *Experimental and Clinical Psychopharmacology, 23* (6), 504-512.

- Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015). Identification and management of nonsystematic purchase task data: Toward best practice. *Experimental and Clinical Psychopharmacology*

### Questions, Suggestions, and Contributions
---------------------------------------------

Have a question? Have a suggestion for a feature? Would you like to contribute? Open an issue, submit a pull request, or [email me](bkaplan.ku@gmail.com)!

### License
-----------

GPL Version 2 or later
