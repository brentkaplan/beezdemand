##
## Copyright 2016 Brent Kaplan
##
## This file is part of beezdemand.
##
## beezdemand is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## beezdemand is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with beezdemand.  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.
##
## summary
## R script for analysis functions
##
## dependencies
## @minpack.lm = implements Levenberg-Marquardt algorithm for nonlinear least squares (Copyright - 1999 - University of Chicago - GPLv3)
## link @ https://cran.r-project.org/web/packages/minpack.lm/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-3
##
## @nls2 = adds brute force and multiple starting values to nls. (Copyright - 2013 - G. Grothendieck - GPLv2)
## link @ https://cran.r-project.org/web/packages/nls2/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##
## @nlstools = confint2 for bootstrapped confidence intervals (Copyright 2015 - Baty and Delignette-Muller - GPLv2+)
## link @ https://cran.r-project.org/web/packages/nlstools/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##
## @nlmrt (R package) = Nash's customized optimization of L-M residual reduction (Copyright 2016 - John C. Nash - GPLv2)
## link @ https://cran.r-project.org/web/packages/nlmrt/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##
## @ggplot2 (R package) = Grammar of Graphics plotting library (Copyright 2016 - Hadley Wickham - GPLv2)
## link @ https://cran.r-project.org/web/packages/ggplot2/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##

##' Analyzes purchase task data
##'
##' @title FitCurves
##' @param dat data frame (long form) of purchase task data.
##' @param equation Character vector of length one. Accepts either "hs" for Hursh and Silberberg (2008) or "koff" for Koffarnus, Franck, Stein, and Bickel (2015).
##' @param k A numeric vector of length one. Reflects the range of consumption in log10 units. If none provided, k will be calculated based on the max/min of the entire sample. If k = "ind", k will be calculated per individual using max/min + .5. If k = "fit", k will be a free parameter on an individual basis
##' @param remq0e If TRUE, removes consumption and price where price == 0. Default value is FALSE
##' @param replfree Optionally replaces price == 0 with specified value. Note, if fitting using equation == "hs", and 0 is first price, 0 gets replaced by replfree. Default value is .01
##' @param rem0 If TRUE, removes all 0s in consumption data prior to analysis. Default value is FALSE.
##' @param nrepl Number of zeros to replace with replacement value (replnum). Can accept either a number or "all" if all zeros should be replaced. Default is to replace the first zero only.
##' @param replnum Value to replace zeros. Default is .01
##' @param plotcurves Boolean whether to create individual plots. If TRUE, a "plots/" directory is created one level above working directory
##' @param vartext Character vector specifying indices to report on plots. Valid indices include "Q0d", "Alpha", "Q0e", "EVd", "Pmaxe", "Omaxe", "Pmaxd", "Omaxd", "K", "Q0se", "Alphase", "R2", "AbsSS"
##' @return Data frame, fitting params and CI's/SE's
##' @author Brent Kaplan <bkaplan4@@ku.edu> Shawn Gilroy <shawn.gilroy@temple.edu>
##' @export
FitCurves <- function(dat, equation, k, remq0e = FALSE, replfree = NULL, rem0 = FALSE, nrepl = NULL, replnum = NULL, plotcurves = FALSE, vartext = NULL) {

    if ("p" %in% colnames(dat)) {
        colnames(dat)[which(colnames(dat) == "p")] <- "id"
    } else if (!"id" %in% colnames(dat)) {
        stop("Make sure there is an id column in data")
    }

    if (plotcurves == TRUE) {
        if (!dir.exists("../plots/")) dir.create("../plots/")
        basedir <- "../plots/"
        basename <- paste0("indplots-", equation, "-")
        outdir <- createOutdir(basedir = basedir, basename = basename)[[1]]

        tobquote = NULL
        if (!is.null(vartext)) {
            dict <- data.frame(Name = c("Q0d", "Alpha", "Q0e", "EVd", "Pmaxe",
                                        "Omaxe", "Pmaxd", "Omaxd",
                                        "K", "Q0se", "Alphase", "R2", "AbsSS"),
                               Variable = c("Q[0[d]]", "alpha", "Q[0[e]]", "EV", "P[max[e]]",
                                            "O[max[e]]", "P[max[d]]",  "O[max[d]]",
                                            "k", "Q[0[se]]", "alpha[se]", "R^2", "AbsSS"))
            if (any(is.na(dict$Variable[match(vartext, dict$Name)]))) {
                warning(paste0("Invalid parameter in vartext! I will go on but won't print any parameters. Try again with one of the following: ", dict$Name))
            } else {
                tobquote <- as.character(dict$Variable[match(vartext, dict$Name)])
            }
        }
    }

    ## get rid of NAs
    dat <- dat[!is.na(dat$y), ]

    ## Get N unique participants, informing loop
    participants <- unique(dat$id)
    np <- length(participants)

    ## Preallocate for speed
    cnames <- c("Participant", "Q0e", "BP0", "BP1", "Omaxe", "Pmaxe", "Equation", "Q0d", "K",
              "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
              "AlphaLow", "AlphaHigh", "EVd", "Omaxd", "Pmaxd", "Notes")

    dfres <- data.frame(matrix(vector(),
                             np,
                             length(cnames),
                             dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    ## loop to find empirical measures before transformations
    for (i in seq_len(np)) {
        dfres[i, "Participant"] <- participants[i]
        dfres[i, "Equation"] <- equation

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]

        adf[, "expend"] <- adf$x * adf$y

        ## Find empirical Q0, BP0, BP1
        dfres[i, "Q0e"] <- adf[which(adf$x == min(adf$x), arr.ind = TRUE), "y"]
        if (0 %in% adf$y) {
            for (j in nrow(adf):1) {
                if (adf$y[j] == 0) {
                    next
                } else {
                    dfres[i, "BP0"] <- j + 1
                    break
                }
            }
        } else {
            dfres[i, "BP0"] <- NA
        }

        dfres[i, "BP1"] <- if (sum(adf$y) > 0) max(adf[adf$y != 0, "x"]) else NA

        ## Find empirical Pmax, Omax
        dfres[i, "Omaxe"] <- max(adf$expend)
        dfres[i, "Pmaxe"] <- adf[max(which(adf$expend == max(adf$expend))), "x"]
    }

    ## Transformations if specified
    if (!is.null(nrepl) && !is.null(replnum)) {
        dat <- ReplaceZeros(dat, nrepl = nrepl, replnum = replnum)
    }

    ## If no k is provided, otherwise
    ## TODO: provide a character element to fit empirical max/min range
    if (missing(k)) {
        k <- GetK(dat)
        kest <- FALSE
    } else if (is.numeric(k)) {
        k <- k
        kest <- FALSE
    } else if (is.character(k)) {
        if (k == "fit") {
            kest <- "fit"
            kstart <- GetK(dat)
        } else if (k == "ind") {
            kest <- "ind"
        } else if (k == "share") {
            k <- GetSharedK(dat, equation, remq0e, replfree, rem0)
            if (is.character(k)) {
                warning(k)
                k <- GetK(dat)
                kest <- FALSE
            }
            kest <- "share"
        }
    } else {
        k <- GetK(dat)
        kest <- FALSE
    }

    ## loop to fit data
    for (i in seq_len(np)) {
        dfres[i, "Participant"] <- participants[i]
        dfres[i, "Equation"] <- equation

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]

        if (kest == "ind") {
            k <- GetK(adf)
        } else if (kest == "fit") {
            k <- kstart
        }

        adf[, "k"] <- k

        if (equation == "hs") {
            ## If retain y where x = 0, replace
            if (remq0e) {
                adf <- adf[adf$x != 0, ]
            } else if (!is.null(replfree)) {
                replfree <- if (is.numeric(replfree)) replfree else 0.01
                adf[adf$x == 0, "x"] <- replfree
            }

            ## Drop any zero consumption points altogether
            ## Will change when q+1 equation gets coded
            adf <- adf[adf$y != 0, ]

            if (!kest == "fit") {
                fit <- NULL
                suppressWarnings(fit <- try(nlmrt::wrapnls(data = adf,
                (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
                start = list(q0 = 10, alpha = 0.01),
                control = list(maxiter = 1000)), silent = TRUE))
            } else {
                suppressWarnings(fit <- try(nlmrt::wrapnls(data = adf,
                (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
                start = list(q0 = 10, k = kstart, alpha = 0.01),
                control = list(maxiter = 1000)), silent = TRUE))
            }

            if (!class(fit) == "try-error") {
                dfres[i, "K"] <- if (kest == "fit") as.numeric(coef(fit)["k"]) else min(adf$k)
                dfres[i, c("Q0d", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
                dfres[i, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 2), 2]
                dfres[i, "N"] <- length(adf$k)
                dfres[i, "R2"] <- 1.0 - (deviance(fit)/sum((log10(adf$y) - mean(log10(adf$y)))^2))
                dfres[i, "AbsSS"] <- deviance(fit)
                dfres[i, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
                dfres[i, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 3)]
                dfres[i, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(2, 4)]
                dfres[i, "EVd"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
                dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0d"] * dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5)) * (0.083 * dfres[i, "K"] + 0.65)
                dfres[i, "Omaxd"] <- (10^(log10(dfres[i, "Q0d"]) + (dfres[i, "K"] * (exp(-dfres[i, "Alpha"] * dfres[i, "Q0d"] * dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
                dfres[i, "Notes"] <- fit$convInfo$stopMessage

                if (plotcurves == TRUE) {
                    PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ], outdir = outdir,
                               fitfail = FALSE, tobquote = tobquote, vartext = vartext)
                }
            } else if (class(fit) == "try-error") {
                dfres[i, "Notes"] <- fit[1]
                dfres[i, "Notes"] <- strsplit(dfres[i, "Notes"], "\n")[[1]][2]

                if (plotcurves == TRUE) {
                    suppressWarnings(PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ],
                                                outdir = outdir, fitfail = TRUE,
                                                tobquote = tobquote, vartext = vartext))
                }
            }
        } else if (equation == "koff") {
            if (rem0) {
                adf <- adf[adf$y != 0, ]
            }

            ## If retain y where x = 0, replace
            if (remq0e) {
                adf <- adf[adf$x != 0, ]
            } else if (!is.null(replfree)) {
                replfree <- if (is.numeric(replfree)) replfree else 0.01
                adf[adf$x == 0, "x"] <- replfree
            }

            if (!kest == "fit") {
                fit <- NULL
                fit <- try(nlmrt::wrapnls(data = adf,
                                          y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                                          start = list(q0 = 10, alpha = 0.01),
                                          control = list(maxiter = 1000)), silent = TRUE)
            } else {
                fit <- try(nlmrt::wrapnls(data = adf,
                                          y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                                          start = list(q0 = 10, k = kstart, alpha = 0.01),
                                          control = list(maxiter = 1000)), silent = TRUE)
            }

            if (!class(fit) == "try-error") {
                dfres[i, "K"] <- if (kest == "fit") as.numeric(coef(fit)["k"]) else min(adf$k)
                dfres[i, c("Q0d", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
                dfres[i, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 2), 2]
                dfres[i, "N"] <- length(adf$k)
                dfres[i, "R2"] <-  1.0 -(deviance(fit)/sum((adf$y - mean(adf$y))^2))
                dfres[i, "AbsSS"] <- deviance(fit)
                dfres[i, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
                dfres[i, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 3)]
                dfres[i, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(2, 4)]
                dfres[i, "EVd"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
                dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0d"] * dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5)) * (0.083 * dfres[i, "K"] + 0.65)
                dfres[i, "Omaxd"] <- (dfres[i, "Q0d"] * (10^(dfres[i, "K"] * (exp(-dfres[i, "Alpha"] * dfres[i, "Q0d"] * dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
                dfres[i, "Notes"] <- fit$convInfo$stopMessage

                if (plotcurves == TRUE) {
                    PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ], outdir = outdir,
                               fitfail = FALSE, tobquote = tobquote, vartext = vartext)
                }
            } else if (class(fit) == "try-error") {
                dfres[i, "Notes"] <- fit[1]
                dfres[i, "Notes"] <- strsplit(dfres[i, "Notes"], "\n")[[1]][2]

                if (plotcurves == TRUE) {
                    suppressWarnings(PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ],
                                                outdir = outdir, fitfail = TRUE,
                                                tobquote = tobquote, vartext = vartext))
                }
            }
        }

    trim.leading <- function (x)  sub("^\\s+", "", x)
        dfres[i, "Notes"] <- trim.leading(dfres[i, "Notes"])
    }

  ## if(plotting) {
  ##   ## Can add this to build tools and remove later, just added for now -sg

  ##   if(!require(ggplot2)) {
  ##     install.packages("ggplot2")
  ##     require(ggplot2)
  ##     library(ggplot2)
  ##   }

  ##   xDraw <- seq(min(dat$x), max(dat$x), 0.01)

  ##   p.rep <- seq(1,max(dat$id),1)

  ##   graphFrame<-data.frame(Individual=rep(seq(min(p.rep), max(p.rep),1),each=length(xDraw)),
  ##                          DemandSeries=rep(seq(1:length(xDraw)-1),length(p.rep)),
  ##                          YSeries=rep(seq(1:length(xDraw)-1),length(p.rep)),
  ##                          XSeries=rep(seq(1:length(xDraw)-1),length(p.rep)))

  ##   pointFrame <- NULL

  ##   for(i in unique(dat$id))
  ##   {
  ##     qTemp <- dfres[i,]$Q0
  ##     aTemp <- dfres[i,]$Alpha
  ##     kTemp <- dfres[i,]$K

  ##     for (j in 1:length(xDraw))
  ##     {
  ##       ### mapped fittings, based on model
  ##       if (equation == "hs")
  ##       {
  ##         graphFrame[ graphFrame$Individual==i & graphFrame$DemandSeries==as.numeric(j),]$YSeries <- log10(qTemp) + kTemp * (exp(-aTemp*qTemp*xDraw[j])-1)
  ##       }else if (equation == "koff")
  ##       {
  ##         graphFrame[ graphFrame$Individual==i & graphFrame$DemandSeries==as.numeric(j),]$YSeries <- qTemp * 10^(kTemp * (exp(-aTemp * qTemp * xDraw[j]) - 1))
  ##       }

  ##       ### Base domain
  ##       graphFrame[ graphFrame$Individual==i & graphFrame$DemandSeries==as.numeric(j),]$XSeries <- xDraw[j]
  ##     }
  ##   }

  ##   axis_mod <- function(l) {
  ##     l <- paste("10^", l, sep = "")
  ##     parse(text=l)
  ##   }

  ##   if (equation == "hs")
  ##   {
  ##     pointFrame <- data.frame(X=dat$x, Y=log10(dat$y), Individual=dat$id)

  ##     logChart <- ggplot() +
  ##       geom_line(data=graphFrame, aes(x=XSeries, y=YSeries, group=Individual, colour = factor(Individual))) +
  ##       geom_point(data=pointFrame, aes(x=pointFrame$X, y=pointFrame$Y, shape=factor(Individual))) +
  ##       expand_limits(y=0) +
  ##       theme_bw() +
  ##       theme(panel.grid.minor = element_blank()) +
  ##       ggtitle("Fitted Demand Curves\n") +
  ##       ylab("log(Consumption)") +
  ##       scale_x_log10(
  ##         breaks = scales::trans_breaks("log10", function(x) 10^x),
  ##         labels = scales::trans_format("log10", scales::math_format(10^.x))
  ##       ) +
  ##       scale_y_continuous(labels=axis_mod) +
  ##       annotation_logticks(sides = "b") +
  ##       xlab("log(Price)") +
  ##       theme(legend.title = element_blank()) +
  ##       theme(legend.position = "none") +
  ##       theme(legend.direction = "vertical") +
  ##       theme(panel.grid.minor = element_blank()) +
  ##       theme(panel.grid.major = element_blank()) +
  ##       guides(col = guide_legend(ncol = 3))

  ##       print(logChart)

  ##   }else if (equation == "koff")
  ##   {
  ##     pointFrame <- data.frame(X=dat$x, Y=dat$y, Individual=dat$id)

  ##     logChart <- ggplot() +
  ##       geom_line(data=graphFrame, aes(x=XSeries, y=YSeries, group=Individual, colour = factor(Individual))) +
  ##       geom_point(data=pointFrame, aes(x=pointFrame$X, y=pointFrame$Y, shape=factor(Individual))) +
  ##       expand_limits(y=0) +
  ##       theme_bw() +
  ##       theme(panel.grid.minor = element_blank()) +
  ##       ggtitle("Fitted Demand Curves\n") +
  ##       ylab("log(Consumption)") +
  ##       scale_x_log10(
  ##         breaks = scales::trans_breaks("log10", function(x) 10^x),
  ##         labels = scales::trans_format("log10", scales::math_format(10^.x))
  ##       ) +
  ##       annotation_logticks(sides = "b") +
  ##       xlab("log(Price)") +
  ##       theme(legend.title = element_blank()) +
  ##       theme(legend.position = "none") +
  ##       theme(legend.direction = "vertical") +
  ##       theme(panel.grid.minor = element_blank()) +
  ##       theme(panel.grid.major = element_blank()) +
  ##       guides(col = guide_legend(ncol = 3))

  ##     print(logChart)

  ##   }
  ## }
     if (kest == "share") {
         names(dfres)[names(dfres) == "K"] <- "SharedK"
     } else if (kest == "fit") {
         names(dfres)[names(dfres) == "K"] <- "FittedK"
     }
    return(dfres)
}

##' Fits curve to pooled data
##'
##' @title Fit Pooled Curves
##' @param dat data frame (long form) of purchase task data.
##' @param equation Character vector of length one. Accepts either "hs" for Hursh and Silberberg (2008) or "koff" for Koffarnus, Franck, Stein, and Bickel (2015).
##' @param k A numeric vector of length one. Reflects the range of consumption in log10 units. If none provided, k will be calculated based on the max/min of the entire sample. If k = "fit", k will be a free parameter
##' @param remq0e If TRUE, removes consumption and price where price == 0. Default value is FALSE
##' @param replfree Optionally replaces price == 0 with specified value. Note, if fitting using equation == "hs", and 0 is first price, 0 gets replaced by replfree. Default value is .01
##' @param rem0 If TRUE, removes all 0s in consumption data prior to analysis. Default value is FALSE.
##' @param nrepl Number of zeros to replace with replacement value (replnum). Can accept either a number or "all" if all zeros should be replaced. Default is to replace the first zero only.
##' @param replnum Value to replace zeros. Default is .01
##' @param plotcurves Boolean whether to create plot. If TRUE, a "plots/" directory is created one level above working directory. Default is FALSE.
##' @param method Character string of length 1. Accepts "Mean" to fit to mean data or "Pooled" to fit to pooled data
##' @param indpoints Boolean whether to plot individual points in gray. Default is TRUE.
##' @param vartext Character vector specifying indices to report on plots. Valid indices include "Q0d", "Alpha", "Q0e", "EVd", "Pmaxe", "Omaxe", "Pmaxd", "Omaxd", "K", "Q0se", "Alphase", "R2", "AbsSS"
##' @return Data frame
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
FitMeanCurves <- function(dat, equation, k, remq0e = FALSE, replfree = NULL, rem0 = FALSE, nrepl = NULL, replnum = NULL, plotcurves = FALSE, method = NULL, indpoints = TRUE, vartext = NULL) {

    if (is.null(method)) stop("No method specified. Choose either 'Mean' or 'Pooled'")

    if ("p" %in% colnames(dat)) {
        colnames(dat)[which(colnames(dat) == "p")] <- "id"
    } else if (!"id" %in% colnames(dat)) {
         stop("Make sure there is an id column in data")
    }

    if (plotcurves) {
        if (!dir.exists("../plots/")) dir.create("../plots/")
        basedir <- "../plots/"
        basename <- paste0(method, "-", equation, "-")
        outdir <- createOutdir(basedir = basedir, basename = basename)[[1]]

        tobquote = NULL
        if (!is.null(vartext)) {
            dict <- data.frame(Name = c("Q0d", "Alpha", "Q0e", "EVd", "Pmaxe",
                                        "Omaxe", "Pmaxd", "Omaxd",
                                        "K", "Q0se", "Alphase", "R2", "AbsSS"),
                               Variable = c("Q[0[d]]", "alpha", "Q[0[e]]", "EV", "P[max[e]]",
                                            "O[max[e]]", "P[max[d]]",  "O[max[d]]",
                                            "k", "Q[0[se]]", "alpha[se]", "R^2", "AbsSS"))
            if (any(is.na(dict$Variable[match(vartext, dict$Name)]))) {
                warning(paste0("Invalid parameter in vartext! I will go on but won't print any parameters. Try again with one of the following: ", dict$Name))
            } else {
                tobquote <- as.character(dict$Variable[match(vartext, dict$Name)])
                printvars <- TRUE
            }
        }

    }

    dat <- dat[!is.na(dat$y), ]

    cnames <- c("Participant", "Q0e", "BP0", "BP1", "Omaxe", "Pmaxe", "Equation", "Q0d", "K",
              "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
              "AlphaLow", "AlphaHigh", "EVd", "Omaxd", "Pmaxd", "Notes")

    dfres <- data.frame(matrix(vector(),
                             1,
                             length(cnames),
                             dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    dfres[["Participant"]] <- method

    ## Find empirical measures before transofrmations
    adf <- aggregate(y ~ x, data = dat, mean)
    adf[, "expend"] <- adf$x * adf$y
    dfres[["Q0e"]] <- adf[which(adf$x == min(adf$x), arr.ind = TRUE), "y"]
    if (0 %in% adf$y) {
        for (j in nrow(adf):1) {
            if (adf$y[j] == 0) {
                next
            } else {
                dfres[["BP0"]] <- j + 1
                break
            }
        }
    } else {
        dfres[["BP0"]] <- NA
    }
    dfres[["BP1"]] <- if (sum(adf$y) > 0) max(adf[adf$y != 0, "x"]) else NA

    ## Find empirical Pmax, Omax
    dfres[["Omaxe"]] <- max(adf$expend)
    dfres[["Pmaxe"]] <- adf[max(which(adf$expend == max(adf$expend))), "x"]

    ## Transformations if specified
    if (!is.null(nrepl) && !is.null(replnum)) {
        dat <- ReplaceZeros(dat, nrepl = nrepl, replnum = replnum)
    }

    if (method == "Mean") {
        dato <- dat
        dat <- aggregate(y ~ x, data = dat, mean)
    }

    ## If no k is provided, otherwise
    ## TODO: provide a character element to fit empirical max/min range
    if (missing(k)) {
        k <- GetK(dat)
        kest <- FALSE
    } else if (is.numeric(k)) {
        k <- k
        kest <- FALSE
    } else if (k == "fit") {
        kest <- "fit"
        kstart <- GetK(dat)
    } else {
        k <- GetK(dat)
        kest <- FALSE
    }

    if (kest == "fit") {
        k <- kstart
    } else {
        dat[, "k"] <- k
    }

    if (remq0e) {
        dat <- dat[dat$x != 0, ]
    } else if (!is.null(replfree)) {
        replfree <- if (is.numeric(replfree)) replfree else 0.01
        dat[dat$x == 0, "x"] <- replfree
    }

    if (equation == "hs") {
        ## Drop any zero consumption points altogether
        dat <- dat[dat$y != 0, ]

        fit <- NULL
        if (!kest == "fit") {
            suppressWarnings(fit <- try(nlmrt::wrapnls(data = dat,
            (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
            start = list(q0 = 10, alpha = 0.01),
            control = list(maxiter = 1000)), silent = TRUE))
        } else {
            suppressWarnings(fit <- try(nlmrt::wrapnls(data = dat,
            (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
            start = list(q0 = 10, k = kstart, alpha = 0.01),
            control = list(maxiter = 1000)), silent = TRUE))
        }
    } else if (equation == "koff") {
        fit <- NULL
        if (!kest == "fit") {
            fit <- try(nlmrt::wrapnls(data = dat,
                                          y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                                          start = list(q0 = 10, alpha = 0.01),
                                          control = list(maxiter = 1000)), silent = TRUE)
        } else {
            fit <- try(nlmrt::wrapnls(data = dat,
                                          y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                                          start = list(q0 = 10, k = kstart, alpha = 0.01),
                                          control = list(maxiter = 1000)), silent = TRUE)
        }
    }

        if (!class(fit) == "try-error") {
            dfres[["Equation"]] <- equation
            dfres[["K"]] <- if (kest == "fit") as.numeric(coef(fit)["k"]) else min(dat$k)
            dfres[["Alpha"]] <- as.numeric(coef(fit)["alpha"])
            dfres[["Q0d"]] <- as.numeric(coef(fit)["q0"])
            dfres[["Q0se"]] <- summary(fit)[[10]][1, 2]
            dfres[["Alphase"]] <- summary(fit)[[10]][2, 2]
            dfres[["N"]] <- length(dat$k)
            if (equation == "hs") {
                dfres[["R2"]] <- 1.0 - (deviance(fit)/sum((log10(dat$y) - mean(log10(dat$y)))^2))
            } else if (equation == "koff") {
                dfres[["R2"]] <- 1.0 - (deviance(fit)/sum((dat$y - mean(dat$y))^2))
            }
            dfres[["AbsSS"]] <- deviance(fit)
            dfres[["SdRes"]] <- sqrt(deviance(fit)/df.residual(fit))
            dfres[["Q0Low"]] <- nlstools::confint2(fit)[1]
            dfres[["Q0High"]] <- nlstools::confint2(fit)[3]
            dfres[["AlphaLow"]] <- nlstools::confint2(fit)[2]
            dfres[["AlphaHigh"]] <- nlstools::confint2(fit)[4]
            dfres[["EVd"]] <- 1/(dfres[["Alpha"]] * (dfres[["K"]] ^ 1.5) * 100)
            dfres[["Pmaxd"]] <- 1/(dfres[["Q0d"]] * dfres[["Alpha"]] * (dfres[["K"]] ^ 1.5)) * (0.083 * dfres[["K"]] + 0.65)
            dfres[["Omaxd"]] <- (10^(log10(dfres[["Q0d"]]) + (dfres[["K"]] * (exp(-dfres[["Alpha"]] * dfres[["Q0d"]] * dfres[["Pmaxd"]]) - 1)))) * dfres[["Pmaxd"]]
            dfres[["Notes"]] <- fit$convInfo$stopMessage

            if (plotcurves) {

                majlabels <- c(".0000000001", ".000000001", ".00000001", ".0000001", ".000001", ".00001", ".0001", ".001", ".01", ".1", "1", "10", "100", "1000")
                majticks <- lseq()
                minticks <- minTicks(majticks)

                datmean <- aggregate(y ~ x, data = dat, mean)
                tempnew <- data.frame(x = seq(min(dat$x[dat$x > 0]), max(dat$x), length.out = 1000),  k = dfres[["K"]])
                if (equation == "hs") {
                    tempnew$y <- 10^(predict(fit, newdata = tempnew))
                } else if (equation == "koff") {
                    tempnew$y <- predict(fit, newdata = tempnew)
                }
                tempnew$expend <- tempnew$x * tempnew$y

                xmin <- min(c(tempnew$x[tempnew$x > 0], .1))
                xmax <- max(tempnew$x)
                if (indpoints && method == "Mean") {
                    ymin <- min(c(tempnew$y, dato$y[dato$y > 0], 1))
                    ymax <- min(c(1000, max(c(tempnew$y, dato$y)))) + 5
                } else {
                    ymin <- min(c(tempnew$y, dat$y[dat$y > 0], 1))
                    ymax <- min(c(1000, max(c(tempnew$y, dat$y)))) + 5
                }

                pdf(file = paste0(outdir, method, ".pdf"))
                par(mar = c(5, 4, 4, 4) + 0.3)
                plot(tempnew$x, tempnew$y,
                     type = "n", log = "xy", yaxt = "n", xaxt = "n", bty = "l",
                     xlim = c(xmin, xmax),
                     ylim = c(ymin, ymax),
                     xlab = "Price", ylab = "Reported Consumption", main = method)


                if (indpoints && method == "Pooled") {
                    points(dat$x, jitter(dat$y, .8), col = "gray", pch = 16, cex = .5)
                } else if (indpoints && method == "Mean") {
                    points(dato$x, jitter(dato$y, .8), col = "gray", pch = 16, cex = .5)
                }
                points(datmean$x, datmean$y, pch = 16, cex = .9)
                axis(1, majticks, labels = majlabels)
                axis(1, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
                axis(2, majticks, labels = majlabels, las = 1)
                axis(2, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
                lines(tempnew$y ~ tempnew$x, lwd = 1.5)

                if (printvars) {
                    leg <- vector("expression", length(vartext))
                    for (j in seq_along(vartext)) {
                        tmp <- round(dfres[[vartext[j]]], 6)
                        leg[j] <- parse(text = paste(tobquote[[j]], " == ", tmp))
                    }
                    legend("bottomleft", legend = leg, xjust = 0, cex = .7)
                }
                graphics.off()
            }
        } else if (class(fit) == "try-error") {
            warning(paste0("Unable to fit error: ", strsplit(fit[1], "\n")[[1]][2]))
            if (plotcurves) {
                if (!dir.exists("../plots/")) dir.create("../plots/")
                basedir <- "../plots/"
                basename <- paste0(method, "-", equation, "-")
                outdir <- createOutdir(basedir = basedir, basename = basename)[[1]]
                majlabels <- c(".0000000001", ".000000001", ".00000001", ".0000001", ".000001", ".00001", ".0001", ".001", ".01", ".1", "1", "10", "100", "1000")
                majticks <- lseq()
                minticks <- minTicks(majticks)

                datmean <- aggregate(y ~ x, data = dat, mean)
                xmin <- min(c(dat$x[dat$x > 0], .1))
                xmax <- max(dat$x)
                ymin <- min(c(dat$y, dat$y[dat$y > 0], 1))
                ymax <- min(c(1000, max(dat$y))) + 5

                pdf(file = paste0(outdir, method, ".pdf"))
                par(mar = c(5, 4, 4, 4) + 0.3)
                plot(dat$x, dat$y,
                     type = "n", log = "xy", yaxt = "n", xaxt = "n", bty = "l",
                     xlim = c(xmin, xmax),
                     ylim = c(ymin, ymax),
                     xlab = "Price", ylab = "Reported Consumption", main = method)

                if (indpoints && method == "Pooled") {
                    points(dat$x, jitter(dat$y, .8), col = "gray", pch = 16, cex = .5)
                } else if (indpoints && method == "Mean") {
                    points(dato$x, jitter(dato$y, .8), col = "gray", pch = 16, cex = .5)
                }
                points(datmean$x, datmean$y, pch = 16, cex = .9)
                axis(1, majticks, labels = majlabels)
                axis(1, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
                axis(2, majticks, labels = majlabels, las = 1)
                axis(2, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
                graphics.off()
            }
        }
    dfres
}


##' Finds shared k among selected datasets using global regression
##'
##' Uses global regression to fit a shared k among datasets. Assumes the dataset is in its final form. As of now, only to be used within FitCurves
##' @title Get Shared K
##' @param dat Dataframe (longform)
##' @param equation Character vector. Accepts either "hs" or "koff"
##' @param remq0e If TRUE, removes consumption and price where price == 0.
##' @param replfree Optionally replaces price == 0 with specified value. Note, if fitting using equation == "hs", and 0 is first price, 0 gets replaced by replfree.
##' @param rem0 If TRUE, removes all 0s in consumption data prior to analysis.
##' @return Numeric value of shared k
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
GetSharedK <- function(dat, equation, remq0e, replfree, rem0) {

    ## get rid of NAs
    dat <- dat[!is.na(dat$y), ]

    ## remove q0e if specified, otherwise replace if specified
    if (remq0e) {
        dat <- dat[dat$x != 0, ]
    } else if (!is.null(replfree)) {
        replfree <- if (is.numeric(replfree)) replfree else 0.01
        dat[dat$x == 0, "x"] <- replfree
    }

    ## drop zeros if hs, otherwise drop zeros if rem0
    if (equation == "hs") {
        dat <- dat[dat$y != 0, ]
    } else if (rem0) {
        dat <- dat[dat$y !=0, ]
    }

    j <- 1
    for (i in unique(dat$id)) {
        dat[dat$id == i, "ref"] <- j
        j <- j+1
    }
    dat$ref <- as.factor(dat$ref)

    ## create contrasts
    dat2 <- cbind(dat, model.matrix(~0 + ref, dat))
    nparams <- length(unique(dat2$ref))

    if (equation == "hs") {
        paramslogq0 <- paste(sprintf("log(q0%d)/log(10)*ref%d", 1:nparams, 1:nparams), collapse = "+")
        paramsalpha <- paste(sprintf("alpha%d*ref%d", 1:nparams, 1:nparams), collapse = "+")
        paramsq0 <- paste(sprintf("q0%d*ref%d", 1:nparams, 1:nparams), collapse = "+")

        startq0 <- paste(sprintf("q0%d", 1:nparams))
        startalpha <- paste(sprintf("alpha%d", 1:nparams))

        startingvals <- as.vector(c(rep(10, length(startq0)), rep(.001, length(startalpha)), 4))
        names(startingvals) <- c(startq0, startalpha, "k")

        fo <- sprintf("log(y)/log(10) ~ (%s) + k * (exp(-(%s) * (%s) * x)-1)", paramslogq0, paramsalpha, paramsq0)

        fit <- NULL
        fit <- try(nlmrt::wrapnls(fo, data = dat2, start = c(startingvals)), silent = TRUE)
browser()
        if (!class(fit) == "try-error") {
            sharedk <- summary(fit)$coefficients["k", 1]
            return(sharedk)
        } else {
            sharedk <- "Unable to find a shared k. Using empirical range of dataset"
            return(sharedk)
        }
    } else if (equation == "koff") {
        paramsq0 <- paste(sprintf("q0%d*ref%d", 1:nparams, 1:nparams), collapse = "+")
        paramsalpha <- paste(sprintf("alpha%d*ref%d", 1:nparams, 1:nparams), collapse = "+")

        startq0 <- paste(sprintf("q0%d", 1:nparams))
        startalpha <- paste(sprintf("alpha%d", 1:nparams))

        startingvals <- as.vector(c(rep(10, length(startq0)), rep(.001, length(startalpha)), 4))
        names(startingvals) <- c(startq0, startalpha, "k")

        fo <- sprintf("y ~ (%s) * 10^(k * exp(-(%s) * (%s) * x)-1)", paramsq0, paramsalpha, paramsq0)

        fit <- NULL
        fit <- try(nlmrt::wrapnls(fo, data = dat2, start = c(startingvals)), silent = TRUE)

        if (!class(fit) == "try-error") {
            sharedk <- summary(fit)$coefficients["k", 1]
            return(sharedk)
        } else {
            sharedk <- "Unable to find a shared k. Using empirical range of dataset"
            return(sharedk)
        }
    }
}

##' Calculates a k value by looking for the max/min consumption across entire dataset and adds .5 to that range
##'
##' Will look for minimum greater zero
##' @title Get K
##' @param dat Dataframe (long form)
##' @return Numeric
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @examples
##' GetK(apt)
##' @export
GetK <- function(dat) {
     (log10(max(dat[dat$y > 0, "y"], na.rm = TRUE)) - log10(min(dat[dat$y > 0, "y"], na.rm = TRUE))) + .5
 }

##' Analyzes a dataframe and returns the regression model.
##'
##' This function takes a dataframe, consisting of prices (x) and consumption values (y), and analyzes it using nonlinear regression.
##' @title Demand Analyzer
##' @param adf A dataframe consisting of prices (x) and consumption values (y).
##' @param eq Character vector of length one. Accepts either "hs" for Hursh and Silberberg (2008) or "koff" for Koffarnus, Franck, Stein, and Bickel (2015). If "hs" and the first price (x) is 0, it will be replaced by replfree.
##' @param nltype Character vector of length one. "default" will use the nonlinear (weighted) least-squares estimate and Gauss-Newton algorithm (for more information, see ?nls). "lm" will fit using the Levenberg-Marquardt algorithm (for more information, see ?nlsLM in the minpack.lm package). "brute" will fit using a two-stage process. First, a grid of potential starting values between q0 and alpha is created; 1 value of q0 being the maximum consumption value in the given dataframe and 100 values of alpha ranging from 0.00000001 to 0.1. Since q0 is recycled, the residual sum of squares is evaluated in each combination of q0 and alpha. The combination of of q0 and alpha that result in the least residual sum of squares is subsequently used as starting values in the nonlinear least-squares fitting process. The port algorithm is used to allow for lower and upper bounds. Lower and upper bounds for q0 are 0.001 and 1.5 times the maximum consumption; lower and upper bounds for alpha are 0.0000000001 and 1. For all fitting methods, a maximum of 1000 iterations are used.
##' @param k A numeric vector of length one. Reflects the range of consumption in log10 units.
##' @param q0st A numeric vector of length one. Starting value used for q0 in both "default" and "lm" fitting methods. Default value is 10.
##' @param ast A numeric vector of length one. Starting value used for alpha in both "defualt" and "lm" fitting methods. Default value is 0.001
##' @param seetrace If TRUE, intermediate results may be shown. Default value is FALSE.
##' @param replfree If eq = "hs" and 0 is first price, 0 gets replaced by replfree. Default value is .01
##' @return In most cases, an object of class 'nls' is returned. If there is an error in the fitting process, the convergence note (type = character) will be returned.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
analyze <- function(adf = NULL, eq = NULL, k = NULL,
                    q0st = 10, ast = .001, seetrace = FALSE, replfree = .01) {

    fo <- switch(eq,
                 "hs" = log(y, 10) ~ log(q0, 10) + (k * (exp(-alpha * q0 * x) - 1)),
                 "koff" = y ~ q0 * (10 ^ (k * (exp(-alpha * q0 * x) - 1))),
                 stop("No equation specified"))

    if (is.null(k)) stop("No value for k specified")

    if (eq == "hs") {
        if (any(adf$y == 0)) {
            ## adf <- adf[- (which(adf$y == 0)), ]
            xremove <- which(adf$y == 0)
            if (length(xremove) > 0) adf <- adf[-xremove, ]
        }
        if (adf$x[1] == 0) {
            adf$x[1] <- replfree
        }
    }

    if (nltype == "default") {
        fit <- try(nlmrt::wrapnls(fo, start = list(q0 = q0st, alpha = ast),
                       control = list(maxiter = 1000, warnOnly = TRUE),
                       trace = seetrace, data = adf), silent = TRUE)
    }

    if (nltype == "brute") {
        grid.start <- expand.grid(list(q0 = c(max(adf$y)),
                                       alpha = seq(.00000001, .1, length.out = 100)))
        start.m <- nls2::nls2(fo, data = adf, start = grid.start,
                              algorithm = "brute-force")
        fit <- try(nlmrt::wrapnls(fo, start = start.m,
                                  control = list(maxiter = 1000, warnOnly = TRUE),
                                  trace = seetrace, data = adf), silent = TRUE)
        ## mod <- try(nls2::nls2(fo, start = start.m, algorithm = "port",
        ##                 lower = list(0.001, 0.0000000001),
        ##                 upper = list((max(adf$y) * 1.5), 1),
        ##                 control = list(maxiter = 1000, warnOnly = TRUE),
        ##                 trace = seetrace, data = adf), silent = TRUE)
    }
    if(inherits(mod, "try-error")) {
        fit <- fit[1]
        attr(fit, "eq") <- eq
        attr(fit, "nltype") <- nltype
        attr(fit, "k") <- k
    } else {
        attr(fit, "eq") <- eq
        attr(fit, "nltype") <- nltype
        attr(fit, "k") <- k
    }
    fit
}

##' Analyzes matrix of purchase task data.
##'
##' Analyzed matrix of purchase task data.
##' @title Do Everything
##' @param mat Matrix of purchase task data.
##' @param prices Vector of prices coinciding with purchase task data
##' @param include0 If FALSE, removes all 0s in consumption data prior to analysis. Default value is TRUE.
##' @param equation Character vector of length one. Accepts either "hs" for Hursh and Silberberg (2008) or "koff" for Koffarnus, Franck, Stein, and Bickel (2015). If "hs" and the first price (x) is 0, it will be replaced by replfree.
##' @param optimizer Character vector of length one. "default" will use the nonlinear (weighted) least-squares estimate and Gauss-Newton algorithm (for more information, see ?nls). "lm" will fit using the Levenberg-Marquardt algorithm (for more information, see ?nlsLM in the minpack.lm package). "brute" will fit using a two-stage process. First, a grid of potential starting values between q0 and alpha is created; 1 value of q0 being the maximum consumption value in the given dataframe and 100 values of alpha ranging from 0.00000001 to 0.1. Since q0 is recycled, the residual sum of squares is evaluated in each combination of q0 and alpha. The combination of of q0 and alpha that result in the least residual sum of squares is subsequently used as starting values in the nonlinear least-squares fitting process. The port algorithm is used to allow for lower and upper bounds. Lower and upper bounds for q0 are 0.001 and 1.5 times the maximum consumption; lower and upper bounds for alpha are 0.0000000001 and 1. For all fitting methods, a maximum of 1000 iterations are used.
##' @param k A numeric vector of length one. Reflects the range of consumption in log10 units.
##' @param remq0e If FALSE, retains consumption and price where price == 0. Default value is TRUE
##' @param replfree Optionally replaces price == 0 with specified value. Note, if fitting using equation == "hs", and 0 is first price, 0 gets replaced by replfree. Default value is .01
##' @return List of results. Each element in the list contains the following subelements: [1] a dataframe used for fitting, [2] a model object, [3] a summary of the model object, [4] a dataframe of indices associated with the dataframe and model to be used in further demand analyses.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
doEverything <- function(mat, prices, include0 = TRUE, equation, optimizer, k, remq0e = TRUE, replfree = NULL) {
    if (is.infinite(k)) stop("k is Inf. Please make sure you calculated k correctly.")
    if (NCOL(mat) > 1) {
        mn <- vector(length = NROW(mat))
        md <- vector(length = NROW(mat))
        for (i in 1:NROW(mat)) {
            mn[i] <- mean(mat[i, ])
            md[i] <- median(mat[i, ])
        }
    } else {
        mn <- mat
        md <- mat
    }

    if ("parshift" %in% names(attributes(mat))) {
        parshift <- attr(mat, "parshift")
        replnum <- attr(mat, "replnum")
    } else {
        parshift <- NA
        replnum <- NA
    }

    dfs <- splitMat(mat, x = prices, incl0 = include0, replfree = replfree)

    mndf <- data.frame("x" = prices, "y" = mn, "expend" = prices * mn)
    mddf <- data.frame("x" = prices, "y" = md, "expend" = prices * md)

    dfs$mean <- mndf
    dfs$median <- mddf

    f <- function(adf, equation, optimizer, k, remq0e) {
        if (is.character(adf)) {
            if (adf == "Fewer than 3 datapoints") stop("I can not and will not analyze a dataset with less than 3 positive datapoints. Please run your dataset through systemCheck.")
            if (adf == "No consumption") stop("I can not and will not analyze a dataset with no consumption. . Please run your dataset through systemCheck.")
        }
        q0e <- adf[1, "y"]
        adf <- if (remq0e) adf[-1,] else adf
        ## TODO: allow user to pass different starting values
        model <- analyze(adf, eq = equation, nltype = optimizer, k = k)

    if (!is.character(model)) {
        sumtemp <- try(summary(model), silent = T)
        if (inherits(sumtemp, "try-error")) {
            modelsum <- sumtemp[1]
        } else {
            modelsum <- sumtemp
        }
    } else {
        modelsum <- model
    }

    if (is.character(modelsum)) {
        sum <- data.frame("q0d" = NA, "k" = NA, "alpha" = NA, "q0se" = NA,
                          "alphase" = NA, "df" = NA, "Sy.x" = NA,
                          "pointsanalyzed" = NA, "convergnotes" = modelsum,
                          "eq" = equation, "nltype" = optimizer)
        sum$convergnotes <- as.character(sum$convergnotes)
    } else {
        sum <- data.frame("q0d" = coef(modelsum)[[1]], "k" = k,
                          "alpha" = coef(modelsum)[[2]],
                          "q0se" = coef(modelsum)[[3]],
                          "alphase" = coef(modelsum)[[4]],
                          "df" = modelsum$df[2],
                          "Sy.x" = modelsum$sigma,
                          "pointsanalyzed" = length(modelsum$residuals),
                          "convergnotes" = modelsum$convInfo$stopMessage,
                          "eq" = equation, "nltype" = optimizer)
        sum$convergnotes <- if (sum$pointsanalyzed < 3) "Less than 3 datapoints after 0s removed" else sum$convergnotes
        varstochar <- c("convergnotes", "eq", "nltype")
        sum[varstochar] <- lapply(sum[varstochar], as.character)

    }

    sum$pmaxe <- max(adf$x[which(adf$expend == max(adf$expend))])
    sum$omaxe <- max(adf$expend)
    sum$q0e <- q0e

    if (!is.na(sum$alpha)) {
        sum$ev <- 1/(sum$alpha * (sum$k ^ 1.5) * 100)
        sum$pmaxdq0d <- 1/(sum$q0d * sum$alpha * (sum$k ^ 1.5)) * (0.083 * sum$k + 0.65)
        sum$pmaxdq0e <- 1/(sum$q0e * sum$alpha * (sum$k ^ 1.5)) * (0.083 * sum$k + 0.65)
        if (equation == "hs") {
            sum$omaxdq0d <- (10^(log10(sum$q0d) + (sum$k * (exp(-sum$alpha * sum$q0d * sum$pmaxdq0d) - 1)))) * sum$pmaxdq0d
            sum$omaxdq0e <- (10^(log10(sum$q0e) + (sum$k * (exp(-sum$alpha * sum$q0e * sum$pmaxdq0e) - 1)))) * sum$pmaxdq0e
        } else {
            sum$omaxdq0d <- (sum$q0d * (10^(sum$k * (exp(-sum$alpha * sum$q0d * sum$pmaxdq0d) - 1)))) * sum$pmaxdq0d
            sum$omaxdq0e <- (sum$q0e * (10^(sum$k * (exp(-sum$alpha * sum$q0e * sum$pmaxdq0e) - 1)))) * sum$pmaxdq0e
        }
    } else {
        sum$ev <- NA
        sum$pmaxdq0d <- NA
        sum$pmaxdq0e <- NA
        sum$omaxdq0d <- NA
        sum$omaxdq0e <- NA
    }
        sum$remq0e <- remq0e
        sum$parshift <- parshift
        sum$replnum <- replnum
        sum$incl0s <- include0
    res <- list("df" = adf, "mod" = model, "modsum" = modelsum, "indices" = sum)
}

    reslst <- mapply(f, adf = dfs, equation = equation, optimizer = optimizer, k = k,
                     remq0e = remq0e, SIMPLIFY = FALSE)
    names(reslst) <- names(dfs)
    reslst
}

##' Calculates Area Under the Curve
##'
##' Calculates Area Under the Curve as described in Amlung, Yurasek, McCarty, MacKillop, & Murphy (2015).
##' @title Calculate AUC
##' @param dfs A list of dataframes containing x (prices) and y (consumption).
##' @param qmaxs Required TRUE/FALSE. If TRUE, the maximum consumption value across all dataframes is used for normalization. If FALSE, the maximum consumption value for EACH dataframe is used for normalization.
##' @return Numeric vector of AUC values.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
calcAUC <- function(dfs, qmaxs = NULL) {
    cauc <- function(df) {
        aot <- vector("numeric", length(NROW(df)-1))
        for (i in 1:NROW(df)-1) {
            aot[i] <- (df$x[i+1] - df$x[i]) * (df$y[i+1] + df$y[i]) / 2
        }
        res <- sum(aot)
        res
    }
    if (is.null(qmaxs)) stop("Please specify qmax as either TRUE or FALSE")
    if (qmaxs) {
        z <- sapply(dfs, function(x) dim(x)[1])
        if (!all(z==z[1])) stop("It doesn't appear that all your dataframes are of the same length")
        tempdf <- data.frame("x" = dfs[[1]]$x,
                             "y" = rep(max(sapply(dfs, function(z) max(z$y))),
                                 length(NROW(dfs[[1]]))))
        maxauc <- cauc(tempdf)
        maxauc <- rep(maxauc, length(dfs))
    } else {
        lstmaxdfs <- lapply(dfs, function(z) data.frame(
            "x" = z$x,
            "y" = rep(max(z$y), NROW(z$x))))
        maxauc <- sapply(lstmaxdfs, cauc)
    }
    aucs <- sapply(dfs, cauc)
    aucs <- aucs / maxauc
    aucs
}
