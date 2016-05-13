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
ExampleDataFromAptInDataFrame <- data.frame(
  p=c(rep(1,16), rep(2,16)),
  x=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 15.0, 20.0,
      0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 15.0, 20.0),
  y=c(10,10,10,8,8,8,7,7,7,6,6,5,5,4,3,2,
      10,10, 8,8,6,6,5,5,4,4,3,3,2,2,0,0)
)

k <- log10(max(ExampleDataFromAptInDataFrame[ExampleDataFromAptInDataFrame$y>1,]$y)) -
     log10(min(ExampleDataFromAptInDataFrame[ExampleDataFromAptInDataFrame$y>1,]$y))

testMethods <- FitCurves(ExampleDataFromAptInDataFrame, equation="hs", k=k, remq0e = TRUE, replfree = 0.01)
testMethods

testMethods <- FitCurves(ExampleDataFromAptInDataFrame, equation="koff", k=k, remq0e = TRUE, replfree = 0.01)
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
