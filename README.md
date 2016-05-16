# Behavioral Economic (be) Easy (ez) Demand
An R package containing commonly used functions for analyzing behavioral economic demand curve data.

### Installation
Install and load the devtools package. Then, use install_github to install the package.

```r
install.packages("devtools")
install.packages("digest")

library(devtools)

install_github("bkaplan4/beezdemand")

library(beezdemand)
```

### Note About Use
Currently, this version is under development. You are free to use it, but be aware that there might be bugs present. If you find issues or things that should be changed, please contact me.

### Sample Implementation

```r
# Example Dataset (note long form)
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

# Descriptive Summary
> GetDescriptives(apt)
             0  0.5    1  1.5    2  2.5    3    4    5    6    7    8    9   10
Mean      6.80 6.80 6.50 6.10 5.30 5.20 4.80 4.30 3.90 3.50 3.30 2.60 2.40 2.20
SD        2.62 2.62 2.27 1.91 1.89 1.87 1.48 1.57 1.45 1.43 1.34 1.51 1.58 1.32
PropZeros 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.10 0.10 0.10
NAs       0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
            15   20
Mean      1.10 0.80
SD        1.37 1.14
PropZeros 0.50 0.60
NAs       0.00 0.00

# Apply Stein et al., (2015) Algorithm
> head(CheckUnsystematic(apt), 3)
  Participant TotalPass DeltaQ DeltaQPass Bounce BouncePass Reversals
1          19         3 0.2112       Pass      0       Pass         0
2          30         3 0.1437       Pass      0       Pass         0
3          38         3 0.7885       Pass      0       Pass         0
  ReversalsPass NumPosValues
1          Pass           16
2          Pass           16
3          Pass           14

# Analyze Using Either Exponential/Exponentiated
> head(FitCurves(apt, "hs"), 3)
  Participant Q0e BP0 BP1 Omaxe Pmaxe Equation        Q0 K        R2
1          19  10  NA  20    45    15       hs 10.492736 1 0.9643380
2          30   3  NA  20    20    20       hs  2.942614 1 0.7934822
3          38   4  15  10    21     7       hs  4.525925 1 0.8656883
        Alpha      Q0se      Alphase  N      AbsSS      SdRes    Q0Low
1 0.004856258 0.4312016 0.0002574193 16 0.02028108 0.03806112 9.567901
2 0.014057972 0.2531714 0.0018292289 16 0.09724906 0.08334484 2.399616
3 0.009120746 0.2373820 0.0009315781 14 0.02601971 0.04656511 4.008714
     Q0High    AlphaLow   AlphaHigh        EV    Omaxd    Pmaxd     Notes
1 11.417572 0.004304148 0.005408367 2.0591989 45.63181 14.38512 converged
2  3.485613 0.010134666 0.017981278 0.7113402 15.76329 17.71936 converged
3  5.043136 0.007091012 0.011150481 1.0964015 24.29624 17.75686 converged

> head(FitCurves(apt, "koff"), 3)
  Participant Q0e BP0 BP1 Omaxe Pmaxe Equation        Q0 K        R2
1          19  10  NA  20    45    15     koff 10.219980 1 0.9636911
2          30   3  NA  20    20    20     koff  3.025350 1 0.8188253
3          38   4  15  10    21     7     koff  4.599041 1 0.8182557
        Alpha      Q0se      Alphase  N    AbsSS     SdRes    Q0Low    Q0High
1 0.004501826 0.2673350 0.0002894209 16 3.186106 0.4770524 9.646603 10.793356
2 0.014498545 0.1780691 0.0021436221 16 1.449398 0.3217583 2.643430  3.407271
3 0.010588272 0.3414723 0.0018843340 16 5.043406 0.6002027 3.866656  5.331426
     AlphaLow   AlphaHigh        EV    Omaxd    Pmaxd     Notes
1 0.003881080 0.005122573 2.2213207 49.22443 15.93181 converged
2 0.009900933 0.019096158 0.6897244 15.28428 16.71106 converged
3 0.006546778 0.014629766 0.9444412 20.92880 15.05260 converged



Apt <- data.frame(
  p=c(rep(1,16), rep(2,16)),
  x=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 15.0, 20.0,
      0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 15.0, 20.0),
  y=c(10,10,10,8,8,8,7,7,7,6,6,5,5,4,3,2,
      10,10, 8,8,6,6,5,5,4,4,3,3,2,2,0,0)
)

k <- log10(max(Apt[Apt$y>1,]$y)) -
     log10(min(Apt[Apt$y>1,]$y))

testMethods <- FitCurves(Apt, equation = "hs", k = k, remq0e = FALSE, replfree = 0.01)
testMethods

testMethods <- FitCurves(Apt, equation = "koff", k = k, remq0e = FALSE, replfree = 0.01)
testMethods
```


### Acknowledgments
- Derek D. Reed, Applied Behavioral Economics Laboratory
(www.behavioraleconlab.com)

- Paul E. Johnson, Center for Research Methods and Data Analysis, University of Kansas
(www.crmda.ku.edu)

- Michael Amlung, Cognitive Neuroscience of Addictions Laboratory
(www.cnalab.weebly.com)

- Peter G. Roma, Institutes for Behavior Resources, Inc.
(www.ibrinc.org)

- Steven R. Hursh, Institutes for Behavior Resources, Inc.
(www.ibrinc.org)

- Shawn P. Gilroy, (<shawn.gilroy@temple.edu>; [GitHub](https://github.com/miyamot0))

### Recommended Readings
- Reed, D. D., Niileksela, C. R., & Kaplan, B. A. (2013). Behavioral economics: A tutorial for behavior analysts in practice. *Behavior Analysis in Practice, 6* (1), 34–54.

- Reed, D. D., Kaplan, B. A., & Becirevic, A. (2015). Basic research on the behavioral economics of reinforcer value. In *Autism Service Delivery* (pp. 279-306). Springer New York.

- Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential value. *Psychological Review, 115* (1), 186-198.

- Koffarnus, M. N., Franck, C. T., Stein, J. S., & Bickel, W. K. (2015). A modified exponential behavioral economic demand model to better describe consumption data. *Experimental and Clinical Psychopharmacology, 23* (6), 504-512.

- Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015). Identification and management of nonsystematic purchase task data: Toward best practice. *Experimental and Clinical Psychopharmacology*

### Questions, Suggestions, and Contributions
Have a question? Have a suggestion for a feature? Would you like to contribute? Email me at <bkaplan4@ku.edu>.

### License
GPL-Version 2
