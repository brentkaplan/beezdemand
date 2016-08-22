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
##' @param vartext Character vector specifying indices to report on plots. Valid indices include "Q0d", "Alpha", "Intensity", "EV", "Pmaxe", "Omaxe", "Pmaxd", "Omaxd", "K", "Q0se", "Alphase", "R2", "AbsSS"
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
            dict <- data.frame(Name = c("Q0d", "Alpha", "Intensity", "EV", "Pmaxe",
                                        "Omaxe", "Pmaxd", "Omaxd",
                                        "K", "Q0se", "Alphase", "R2", "AbsSS"),
                               Variable = c("Q[0[d]]", "alpha", "Intensity", "EV", "P[max[e]]",
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

    if (equation == "hs" || equation == "koff") {
        cnames <- c("Participant", "Equation", "Q0d", "K",
                    "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
                    "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Notes")
    } else if (equation == "linear") {
        cnames <- c("Participant", "Equation", "L", "b", "a",
                    "R2", "Lse", "bse", "ase", "N", "AbsSS", "SdRes", "LLow", "LHigh",
                    "bLow", "bHigh", "aLow", "aHigh", "Elasticity", "MeanElasticity",
                    "Omaxd", "Pmaxd", "Notes")
    }

    dfres <- data.frame(matrix(vector(),
                               np,
                               length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    dfresempirical <- GetEmpirical(dat)

    ## Transformations if specified
    if (!is.null(nrepl) && !is.null(replnum)) {
        dat <- ReplaceZeros(dat, nrepl = nrepl, replnum = replnum)
    }

    ## If no k is provided, otherwise
    ## TODO: provide a character element to fit empirical max/min range
    if (!equation == "linear") {
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
    }

    fo <- switch(equation,
                 "hs" = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
                 "koff" = y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                 "linear" = log(y) ~ log(l) + (b * log(x)) - a * x)

    ## loop to fit data
    for (i in seq_len(np)) {
        dfres[i, "Participant"] <- participants[i]
        dfres[i, "Equation"] <- equation

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]

        if (remq0e) {
            adf <- adf[adf$x != 0, ]
        } else if (!is.null(replfree)) {
            replfree <- if (is.numeric(replfree)) replfree else 0.01
            adf[adf$x == 0, "x"] <- replfree
        }

        if (rem0 || equation == "hs") {
            adf <- adf[adf$y != 0, ]
        }

        fit <- NULL
        if (!equation == "linear") {
            if (kest == "ind") {
                k <- GetK(adf)
            } else if (kest == "fit") {
                k <- kstart
            }
            adf[, "k"] <- k

            if (!kest == "fit") {
                suppressWarnings(fit <- try(nlmrt::wrapnls(
                                     formula = fo,
                                     start = list(q0 = 10, alpha = 0.01),
                                     control = list(maxiter = 1000),
                                     data = adf),
                                     silent = TRUE))
            } else {
                suppressWarnings(fit <- try(nlmrt::wrapnls(
                                     formula = fo,
                                     start = list(q0 = 10, k = kstart, alpha = 0.01),
                                     control = list(maxiter = 1000),
                                     data = adf),
                                     silent = TRUE))
            }
        } else if (equation == "linear") {
            fit <- try(nlmrt::wrapnls(
                formula = fo,
                start = list(l = 1, b = 0, a = 0),
                control = list(maxiter = 1000),
                data = adf),
                silent = TRUE)
        }

        if (class(fit) == "try-error") {
            dfres[i, "Notes"] <- fit[1]
            dfres[i, "Notes"] <- strsplit(dfres[i, "Notes"], "\n")[[1]][2]
            if (plotcurves) {
                suppressWarnings(PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ],
                                        outdir = outdir, fitfail = TRUE,
                                        tobquote = tobquote, vartext = vartext))
            }
        } else {
            dfres[i, "N"] <- length(adf$k)
            dfres[i, "AbsSS"] <- deviance(fit)
            dfres[i, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
            dfres[i, "Notes"] <- fit$convInfo$stopMessage
            if (equation == "linear") {
                dfres[i, c("L", "b", "a")] <- as.numeric(coef(fit)[c("l", "b", "a")])
                dfres[i, c("Lse", "bse", "ase")] <- as.numeric(summary(fit)[[10]][c(1:3), 2])
                dfres[i, "R2"] <- 1.0 - (deviance(fit)/sum((log(adf$y) - mean(log(adf$y)))^2))
                dfres[i, c("LLow", "LHigh")] <- nlstools::confint2(fit)[c(1, 4)]
                dfres[i, c("bLow", "bHigh")] <- nlstools::confint2(fit)[c(2, 5)]
                dfres[i, c("aLow", "aHigh")] <- nlstools::confint2(fit)[c(3, 6)]
                ## Calculates mean elasticity based on individual range of x
                pbar <- mean(unique(adf$x))
                dfres[i, "MeanElasticity"] <- dfres[i, "b"] - (dfres[i, "a"] * pbar)
                dfres[i, "Pmaxd"] <- (1 + dfres[i, "b"])/dfres[i, "a"]
                dfres[i, "Omaxd"] <- (dfres[i, "L"] * dfres[i, "Pmaxd"]^dfres[i, "b"]) /
                    exp(dfres[i, "a"] * dfres[i, "Pmaxd"]) * dfres[i, "Pmaxd"]
            } else {
                dfres[i, "K"] <- if (kest == "fit") as.numeric(coef(fit)["k"]) else min(adf$k)
                dfres[i, c("Q0d", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
                dfres[i, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 2), 2]
                dfres[i, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 3)]
                dfres[i, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(2, 4)]
                dfres[i, "EV"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
                dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0d"] * dfres[i, "Alpha"] *
                                        (dfres[i, "K"] ^ 1.5)) * (0.083 * dfres[i, "K"] + 0.65)
            }
            if (equation == "hs") {
                dfres[i, "R2"] <- 1.0 - (deviance(fit)/sum((log10(adf$y) - mean(log10(adf$y)))^2))
                dfres[i, "Omaxd"] <- (10^(log10(dfres[i, "Q0d"]) +
                                          (dfres[i, "K"] *
                                           (exp(-dfres[i, "Alpha"] *
                                                dfres[i, "Q0d"] *
                                                dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
            } else if (equation == "koff") {
                dfres[i, "R2"] <-  1.0 -(deviance(fit)/sum((adf$y - mean(adf$y))^2))
                dfres[i, "Omaxd"] <- (dfres[i, "Q0d"] *
                                      (10^(dfres[i, "K"] *
                                           (exp(-dfres[i, "Alpha"] *
                                                dfres[i, "Q0d"] *
                                                dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
            }
            trim.leading <- function (x)  sub("^\\s+", "", x)
            dfres[i, "Notes"] <- trim.leading(dfres[i, "Notes"])

            if (plotcurves) {
                suppressWarnings(PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ],
                                            outdir = outdir, fitfail = FALSE,
                                            tobquote = tobquote, vartext = vartext))
            }
        }
    }
    if (kest == "share") {
        names(dfres)[names(dfres) == "K"] <- "SharedK"
    } else if (kest == "fit") {
        names(dfres)[names(dfres) == "K"] <- "FittedK"
    }
    dfres <- merge(dfresempirical, dfres, by = "Participant")
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
##' @param vartext Character vector specifying indices to report on plots. Valid indices include "Q0d", "Alpha", "Q0e", "EV", "Pmaxe", "Omaxe", "Pmaxd", "Omaxd", "K", "Q0se", "Alphase", "R2", "AbsSS"
##' @return Data frame
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
FitMeanCurves <- function(dat, equation, k, remq0e = FALSE, replfree = NULL, rem0 = FALSE, nrepl = NULL, replnum = NULL, plotcurves = FALSE, method = NULL, indpoints = TRUE, vartext = NULL) {

    if (is.null(method) || !any(c("Mean", "Pooled") %in% method)) stop("No method specified. Choose either 'Mean' or 'Pooled'")

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
            dict <- data.frame(Name = c("Q0d", "Alpha", "Intensity", "EV", "Pmaxe",
                                        "Omaxe", "Pmaxd", "Omaxd",
                                        "K", "Q0se", "Alphase", "R2", "AbsSS"),
                               Variable = c("Q[0[d]]", "alpha", "Intensity", "EV", "P[max[e]]",
                                            "O[max[e]]", "P[max[d]]",  "O[max[d]]",
                                            "k", "Q[0[se]]", "alpha[se]", "R^2", "AbsSS"))
            if (any(is.na(dict$Variable[match(vartext, dict$Name)]))) {
                warning(paste0("Invalid parameter in vartext! I will go on but won't print any parameters. Try again with one of the following: ", dict$Name))
                printvars <- FALSE
            } else {
                tobquote <- as.character(dict$Variable[match(vartext, dict$Name)])
                printvars <- TRUE
            }
        } else {
            printvars <- FALSE
        }
    }

    dat <- dat[!is.na(dat$y), ]

    if (equation == "hs" || equation == "koff") {
        cnames <- c("Participant", "Equation", "Q0d", "K",
                    "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
                    "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Notes")
    } else if (equation == "linear") {
        cnames <- c("Participant", "Equation", "L", "b", "a",
                    "R2", "Lse", "bse", "ase", "N", "AbsSS", "SdRes", "LLow", "LHigh",
                    "bLow", "bHigh", "aLow", "aHigh", "Elasticity", "MeanElasticity",
                    "Omaxd", "Pmaxd", "Notes")
    }

    dfres <- data.frame(matrix(vector(),
                             1,
                             length(cnames),
                             dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)
    adf <- aggregate(y ~ x, data = dat, mean)
    adf$id <- method

    dfresempirical <- GetEmpirical(adf)

    dfres[["Participant"]] <- method
    dfres[["Equation"]] <- equation

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
    if (!equation == "linear") {
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
    }

    if (remq0e) {
        dat <- dat[dat$x != 0, ]
    } else if (!is.null(replfree)) {
        replfree <- if (is.numeric(replfree)) replfree else 0.01
        dat[dat$x == 0, "x"] <- replfree
    }

    if (rem0 || equation == "hs") {
        dat <- dat[dat$y != 0, ]
    }

    fo <- switch(equation,
                 "hs" = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
                 "koff" = y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                 "linear" = log(y) ~ log(l) + (b * log(x)) - a * x)

    fit <- NULL

    if (!equation == "linear") {
        if (!kest == "fit") {
                suppressWarnings(fit <- try(nlmrt::wrapnls(
                                     formula = fo,
                                     start = list(q0 = 10, alpha = 0.01),
                                     control = list(maxiter = 1000),
                                     data = dat),
                                     silent = TRUE))
            } else {
                suppressWarnings(fit <- try(nlmrt::wrapnls(
                                     formula = fo,
                                     start = list(q0 = 10, k = kstart, alpha = 0.01),
                                     control = list(maxiter = 1000),
                                     data = dat),
                                     silent = TRUE))
            }
    } else if (equation == "linear") {
        fit <- try(nlmrt::wrapnls(
                formula = fo,
                start = list(l = 1, b = 0, a = 0),
                control = list(maxiter = 1000),
                data = dat),
                silent = TRUE)
    }

    if (class(fit) == "try-error") {
        dfres[["Notes"]] <- fit[1]
        dfres[["Notes"]] <- strsplit(dfres[["Notes"]], "\n")[[1]][2]
    } else {
        dfres[["N"]] <- length(dat$k)
        dfres[["AbsSS"]] <- deviance(fit)
        dfres[["SdRes"]] <- sqrt(deviance(fit)/df.residual(fit))
        dfres[["Notes"]] <- fit$convInfo$stopMessage
        if (equation == "linear") {
            dfres[["L"]] <- as.numeric(coef(fit)["l"])
            dfres[["b"]] <- as.numeric(coef(fit)["b"])
            dfres[["a"]] <- as.numeric(coef(fit)["a"])
            dfres[["Lse"]] <- as.numeric(summary(fit)[[10]][1, 2])
            dfres[["bse"]] <- as.numeric(summary(fit)[[10]][2, 2])
            dfres[["ase"]] <- as.numeric(summary(fit)[[10]][3, 2])
            dfres[["R2"]] <- 1.0 - (deviance(fit)/sum((log(adf$y) - mean(log(adf$y)))^2))
            dfres[["LLow"]] <- nlstools::confint2(fit)[1]
            dfres[["LHigh"]] <- nlstools::confint2(fit)[4]
            dfres[["bLow"]] <- nlstools::confint2(fit)[2]
            dfres[["bHigh"]] <- nlstools::confint2(fit)[5]
            dfres[["aLow"]] <- nlstools::confint2(fit)[3]
            dfres[["aHigh"]] <- nlstools::confint2(fit)[6]
            ## Calculates mean elasticity based on individual range of x
            pbar <- mean(unique(dat$x))
            dfres[["MeanElasticity"]] <- dfres[["b"]] - (dfres[["a"]] * pbar)
            dfres[["Pmaxd"]] <- (1 + dfres[["b"]])/dfres[["a"]]
            dfres[["Omaxd"]] <- (dfres[["L"]] * dfres[["Pmaxd"]]^dfres[["b"]]) /
                    exp(dfres[["a"]] * dfres[["Pmaxd"]]) * dfres[["Pmaxd"]]
        } else {
            dfres[["K"]] <- if (kest == "fit") as.numeric(coef(fit)["k"]) else min(dat$k)
            dfres[["Alpha"]] <- as.numeric(coef(fit)["alpha"])
            dfres[["Q0d"]] <- as.numeric(coef(fit)["q0"])
            dfres[["Q0se"]] <- summary(fit)[[10]][1, 2]
            dfres[["Alphase"]] <- summary(fit)[[10]][2, 2]
            dfres[["Q0Low"]] <- nlstools::confint2(fit)[1]
            dfres[["Q0High"]] <- nlstools::confint2(fit)[3]
            dfres[["AlphaLow"]] <- nlstools::confint2(fit)[2]
            dfres[["AlphaHigh"]] <- nlstools::confint2(fit)[4]
            dfres[["EV"]] <- 1/(dfres[["Alpha"]] * (dfres[["K"]] ^ 1.5) * 100)
            dfres[["Pmaxd"]] <- 1/(dfres[["Q0d"]] * dfres[["Alpha"]] * (dfres[["K"]] ^ 1.5)) * (0.083 * dfres[["K"]] + 0.65)
        }
        if (equation == "hs") {
            dfres[["R2"]] <- 1.0 - (deviance(fit)/sum((log10(dat$y) - mean(log10(dat$y)))^2))
            dfres[["Omaxd"]] <- (10^(log10(dfres[["Q0d"]]) +
                                     (dfres[["K"]] *
                                      (exp(-dfres[["Alpha"]] *
                                           dfres[["Q0d"]] *
                                           dfres[["Pmaxd"]]) - 1)))) * dfres[["Pmaxd"]]
        } else if (equation == "koff") {
            dfres[["R2"]] <- 1.0 - (deviance(fit)/sum((dat$y - mean(dat$y))^2))
            dfres[["Omaxd"]] <- (dfres[["Q0d"]] *
                                    (10^(dfres[["K"]] *
                                      (exp(-dfres[["Alpha"]] *
                                           dfres[["Q0d"]] *
                                           dfres[["Pmaxd"]]) - 1)))) * dfres[["Pmaxd"]]
        }
    }
    dfres <- merge(dfresempirical, dfres, by = "Participant")

    if (plotcurves) {
        majlabels <- c(".0000000001", ".000000001", ".00000001", ".0000001", ".000001", ".00001", ".0001", ".001", ".01", ".1", "1", "10", "100", "1000")
        majticks <- lseq()
        minticks <- minTicks(majticks)

        datmean <- aggregate(y ~ x, data = dat, mean)
        if (!class(fit) == "try-error") {
            tempnew <- data.frame(x = seq(min(dat$x[dat$x > 0]), max(dat$x), length.out = 1000))
            if (equation == "hs") {
                tempnew$k = dfres[["K"]]
                tempnew$y <- 10^(predict(fit, newdata = tempnew))
            } else if (equation == "koff") {
                tempnew$k = dfres[["K"]]
                tempnew$y <- predict(fit, newdata = tempnew)
            } else if (equation == "linear") {
                tempnew$y <- exp^(predict(fit, newdata = tempnew))
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
        } else if (class(fit) == "try-error") {
            warning(paste0("Unable to fit error: ", strsplit(fit[1], "\n")[[1]][2]))
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

##' Calculates empirical measures for purchase task data
##'
##' Will calculate and return the following empirical measures: Intensity, BP0, BP1, Omax, and Pmax
##' @title GetEmpirical
##' @param dat data frame (long form) of purchase task data.
##' @return Data frame of empirical measures
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
GetEmpirical <- function(dat) {
    participants <- unique(dat$id)
    np <- length(participants)

    cnames <- c("Participant", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    dfres <- data.frame(matrix(vector(), np, length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    for (i in seq_len(np)) {
        dfres[i, "Participant"] <- participants[i]

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]

        adf[, "expend"] <- adf$x * adf$y

        ## Find empirical measures
        dfres[i, "Intensity"] <- adf[which(adf$x == min(adf$x), arr.ind = TRUE), "y"]
        if (0 %in% adf$y) {
            for (j in nrow(adf):1) {
                if (adf$y[j] == 0) {
                    next
                } else {
                    dfres[i, "BP0"] <- adf$x[j + 1]
                    break
                }
            }
        } else {
            dfres[i, "BP0"] <- NA
        }

        dfres[i, "BP1"] <- if (sum(adf$y) > 0) max(adf[adf$y != 0, "x"]) else NA

        dfres[i, "Omaxe"] <- max(adf$expend)
        dfres[i, "Pmaxe"] <- if (dfres[i, "Omaxe"] == 0) 0 else adf[max(which(adf$expend == max(adf$expend))), "x"]
     }
    dfres
}
