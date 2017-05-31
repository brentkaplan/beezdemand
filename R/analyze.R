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
##' @param k A numeric (or character) vector of length one. Reflects the range of consumption in log10 units. If none provided, k will be calculated based on the max/min of the entire sample + .5. If k = "ind", k will be calculated per individual using max/min + .5. If k = "fit", k will be a free parameter on an individual basis. If k = "range", k will be calculated based on the max/min of the entire sample + .5.
##' @param agg Character vector of length one accepts either "Mean" or "Pooled". If not NULL (default), data will be aggregrated appropriately and analyzed in the specified way.
##' @param detailed If TRUE, output will be a 3 element list including (1) dataframe of results, (2) list of model objects, (3) list of individual dataframes used in fitting. Default value is FALSE, which returns only the dataframe of results.
##' @param xcol The column name that should be treated as "x" data
##' @param ycol The column name that should be treated as "y" data
##' @param idcol The column name that should be treated as dataset identifier
##' @param groupcol The column name that should be treated as the groups
##' @param plotcurves Boolean whether to create individual plots. If TRUE, a "plots/" directory is created one level above working directory
##' @param vartext Character vector specifying indices to report on plots. Valid indices include "Q0d", "Alpha", "Intensity", "EV", "Pmaxe", "Omaxe", "Pmaxd", "Omaxd", "K", "Q0se", "Alphase", "R2", "AbsSS"
##' @param plotdestination Destination of plots
##' @return If detailed == FALSE (default), a dataframe of results. If detailed == TRUE, a 3 element list consisting of (1) dataframe of results, (2) list of model objects, (3) list of individual dataframes used in fitting
##' @author Brent Kaplan <bkaplan4@@ku.edu> Shawn Gilroy <shawn.gilroy@temple.edu>
##' @export
FitCurves <- function(dat, equation, k, agg = NULL, detailed = FALSE, xcol = "x", ycol = "y", idcol = "id", groupcol = NULL, plotcurves = FALSE, vartext = NULL, plotdestination = NULL) {

    if (missing(dat)) stop("Need to provide a dataframe!", call. = FALSE)
    origcols <- colnames(dat)

    dat <- CheckCols(dat, xcol = xcol, ycol = ycol, idcol = idcol, groupcol = groupcol)

    if (missing(equation)) stop("Need to specify an equation!", call. = FALSE)
    equation <- tolower(equation)

    if (equation == "hs" || equation == "koff") {
        cnames <- c("ID", "Equation", "Q0d", "K",
                    "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
                    "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Notes")
    } else if (equation == "linear") {
        cnames <- c("ID", "Equation", "L", "b", "a",
                    "R2", "Lse", "bse", "ase", "N", "AbsSS", "SdRes", "LLow", "LHigh",
                    "bLow", "bHigh", "aLow", "aHigh", "Elasticity", "MeanElasticity",
                    "Omaxd", "Pmaxd", "Notes")
    }

    if (any(is.na(dat$y))) {
        warning("NA values found in y column. Dropping NAs and continuing")
        dat <- dat[!is.na(dat$y), ]
    }

    if (!is.null(agg)) {
        agg <- tolower(agg)
        if (!any(c("mean", "pooled") %in% agg)) {
            stop("No correct agg specified. Choose either 'Mean' or 'Pooled'")
        } else if (agg == "mean") {
            dat <- aggregate(y ~ x, data = dat, mean)
            dat$id <- agg
        } else if (agg == "pooled") {
            tmpdat <- aggregate(y ~ x, data = dat, mean)
            tmpdat$id <- agg
            dat$id <- agg
        }
    }

    if (any(dat$y %in% 0) && (equation == "hs" || equation == "linear")) {
        warning("Zeros found in data no compatible with equation! Dropping zeros!", call. = FALSE)
        dat <- dat[dat$y != 0, , drop = FALSE]
    }

    ps <- unique(dat$id)
    ps <- as.character(ps)
    np <- length(ps)

    dfres <- data.frame(matrix(vector(),
                               np,
                               length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    fits <- vector(mode = "list", length = np)
    adfs <- vector(mode = "list", length = np)
    newdats <- vector(mode = "list", length = np)

    if (!is.null(agg) && agg == "pooled") {
        dfresempirical <- GetEmpirical(tmpdat)
    } else {
        dfresempirical <- GetEmpirical(dat)
    }

    if (!equation == "linear") {
        if (missing(k)) {
            k <- GetK(dat)
            kest <- FALSE
            message("No k value specified. Defaulting to empirical mean range +.5")
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
                k <- GetSharedK(dat, equation, sharecol = "id")
                if (is.character(k)) {
                    warning(k)
                    k <- GetK(dat)
                    kest <- FALSE
                }
                kest <- "share"
            } else if (k == "range") {
                k <- GetK(dat)
                kest <- FALSE
            }
        } else {
            k <- GetK(dat)
            kest <- FALSE
            warning("Defaulting to empirical mean range +.5")
        }
    }

    ## TODO: constraining parameters could be done here; sprintf?
    ## TODO: if groupcol is specified (or not), manufacture vector to loop (paste(agg, grps, sep = "-"))

    fo <- switch(equation,
                 "hs" = (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
                 "koff" = y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                 "linear" = log(y) ~ log(l) + (b * log(x)) - a * x)
    ## loop to fit data
    for (i in seq_len(np)) {
        dfres[i, "ID"] <- ps[i]
        dfres[i, "Equation"] <- equation

        adf <- NULL
        adf <- dat[dat$id == ps[i], ]

        if (nrow(adf) == 0) {
            dfres[i, setdiff(colnames(dfres), c("ID", "Equation", "N", "Notes"))] <- NA
            dfres[i, "N"] <- 0
            dfres[i, "Notes"] <- "No consumption"
            next()
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

        fits[[i]] <- fit
        adfs[[i]] <- adf

        dfres[i, ] <- Extractor(ps[i], adf, fit, eq = equation, cols = colnames(dfres), kest = kest)

        newdat <- NULL
        newdat <- data.frame("ID" = rep(i, length.out = 10000),
                             "x" = seq(min(adf$x), max(adf$x), length.out = 10000),
                             "y" = NA)
        newdat$k <- if (!kest == "fit") k else dfres[["K"]]

        if (!class(fit) == "try-error") {
            if (equation == "hs") {
                newdat$y <-  10^predict(fit, newdata = newdat)
            } else if (equation == "koff") {
                newdat$y <- predict(fit, newdata = newdat)
            } else if (equation == "linear") {
                newdat$y <-  exp(predict(fit, newdata = newdat))
            }
        }
        newdats[[i]] <- newdat


        ## if (plotcurves) {
        ##     if (class(fit) == "try-error") {
        ##         suppressWarnings(PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ],
        ##                                     outdir = outdir, fitfail = TRUE,
        ##                                     tobquote = tobquote, vartext = vartext))
        ##     } else {
        ##         suppressWarnings(PlotCurves(adf = adf, fit = fit, dfrow = dfres[i, ],
        ##                                    outdir = outdir, fitfail = FALSE,
        ##                                    tobquote = tobquote, vartext = vartext))
        ##     }
        ## }
    }

    ## if (kest == "share") {
    ##     names(dfres)[names(dfres) == "K"] <- "SharedK"
    ## } else if (kest == "fit") {
    ##     names(dfres)[names(dfres) == "K"] <- "FittedK"
    ## } else if (kest == "ind") {
    ##     names(dfres)[names(dfres) == "K"] <- "IndividualK"
    ## } else {
    ##     names(dfres)[names(dfres) == "K"] <- "RangeK"
    ## }

    dfres$Equation <- equation
    dfres <- merge(dfresempirical, dfres, by = "ID")
    names(fits) <- ps
    names(adfs) <- ps
    names(newdats) <- ps
    if (detailed) {
        return(list("dfres" = dfres,
                    "fits" = fits,
                    "newdats" = newdats,
                    "adfs" = adfs))
    } else {
        return(dfres)
    }
}

##' Extracts the useful information
##'
##' Populates a single row of a dataframe consisting of important information from fits, etc.
##' @title Extractor
##' @param pid Participant ID
##' @param adf A data frame
##' @param fit Fitted model object
##' @param eq Equation specified
##' @param cols Column names to populate the dataframe row
##' @param kest Specification of k value
##' @return One row of a dataframe with results
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
Extractor <- function(pid, adf, fit, eq, cols, kest) {
    dfrow <- data.frame(matrix(vector(),
                               1,
                               length(cols),
                               dimnames = list(c(), c(cols))), stringsAsFactors = FALSE)
    dfrow[["ID"]] <- pid
    if (class(fit) == "try-error") {
        dfrow[["Notes"]] <- fit[1]
        dfrow[["Notes"]] <- strsplit(dfrow[1, "Notes"], "\n")[[1]][2]
    } else if (eq == "linear") {
        dfrow[1, "N"] <- nrow(adf)
        dfrow[1, c("L", "b", "a")] <- as.numeric(coef(fit)[c("l", "b", "a")])
        dfrow[1, c("Lse", "bse", "ase")] <- as.numeric(summary(fit)[[10]][c(1:3), 2])
        dfrow[1, "R2"] <- 1.0 - (deviance(fit)/sum((log(adf$y) - mean(log(adf$y)))^2))
        dfrow[1, c("LLow", "LHigh")] <- nlstools::confint2(fit)[c(1, 4)]
        dfrow[1, c("bLow", "bHigh")] <- nlstools::confint2(fit)[c(2, 5)]
        dfrow[1, c("aLow", "aHigh")] <- nlstools::confint2(fit)[c(3, 6)]
        ## Calculates mean elasticity based on individual range of x
        pbar <- mean(unique(adf$x))
        dfrow[1, "MeanElasticity"] <- dfrow[1, "b"] - (dfrow[1, "a"] * pbar)
        dfrow[1, "Pmaxd"] <- (1 + dfrow[1, "b"])/dfrow[1, "a"]
        dfrow[1, "Omaxd"] <- (dfrow[1, "L"] * dfrow[1, "Pmaxd"]^dfrow[1, "b"]) /
            exp(dfrow[1, "a"] * dfrow[1, "Pmaxd"]) * dfrow[1, "Pmaxd"]
    } else {
        dfrow[1, "N"] <- length(adf$k)
        dfrow[1, "AbsSS"] <- deviance(fit)
        dfrow[1, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
        dfrow[1, "Notes"] <- fit$convInfo$stopMessage
        dfrow[1, c("Q0d", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
        if (kest == "fit") {
            dfrow[1, "K"] <- as.numeric(coef(fit)["k"])
            dfrow[1, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 3), 2]
            dfrow[1, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 4)]
            dfrow[1, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(3, 6)]
        } else {
            dfrow[1, "K"] <- min(adf$k)
            dfrow[1, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 2), 2]
            dfrow[1, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 3)]
            dfrow[1, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(2, 4)]
        }
        dfrow[1, "EV"] <- 1/(dfrow[1, "Alpha"] * (dfrow[1, "K"] ^ 1.5) * 100)
        dfrow[1, "Pmaxd"] <- 1/(dfrow[1, "Q0d"] * dfrow[1, "Alpha"] *
                                (dfrow[1, "K"] ^ 1.5)) * (0.083 * dfrow[1, "K"] + 0.65)
        if (eq == "hs") {
            dfrow[1, "R2"] <- 1.0 - (deviance(fit)/sum((log10(adf$y) - mean(log10(adf$y)))^2))
            dfrow[1, "Omaxd"] <- (10^(log10(dfrow[1, "Q0d"]) +
                                      (dfrow[1, "K"] *
                                       (exp(-dfrow[1, "Alpha"] *
                                            dfrow[1, "Q0d"] *
                                            dfrow[1, "Pmaxd"]) - 1)))) * dfrow[1, "Pmaxd"]
        } else if (eq == "koff") {
            dfrow[1, "R2"] <-  1.0 -(deviance(fit)/sum((adf$y - mean(adf$y))^2))
            dfrow[1, "Omaxd"] <- (dfrow[1, "Q0d"] *
                                  (10^(dfrow[1, "K"] *
                                       (exp(-dfrow[1, "Alpha"] *
                                            dfrow[1, "Q0d"] *
                                            dfrow[1, "Pmaxd"]) - 1)))) * dfrow[1, "Pmaxd"]
        }
    }
    dfrow[1, "Notes"] <- trim.leading(dfrow[1, "Notes"])
    dfrow
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
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
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
        cnames <- c("ID", "Equation", "Q0d", "K",
                    "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
                    "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Notes")
    } else if (equation == "linear") {
        cnames <- c("ID", "Equation", "L", "b", "a",
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

    dfres[["ID"]] <- method
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
    dfres <- merge(dfresempirical, dfres, by = "ID")

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

##' Extra Sum of Squares F-test
##'
##' One alpha better than individual alphas?
##' @title ExtraF
##' @param dat Long form data frame
##' @param equation "hs"
##' @param groups NULL for all. Character vector matching groups in groupcol
##' @param verbose If TRUE, prints all output including models
##' @param k User-defined k value; if missing will attempt to find shared k and then mean emprirical range (in log units)
##' @param compare Specify whether to compare alpha or Q0. Default is alpha
##' @param idcol The column name that should be treated as dataset identifier
##' @param xcol The column name that should be treated as "x" data
##' @param ycol The column name that should be treated as "y" data
##' @param groupcol The column name that should be treated as the groups
##' @return List of results and models
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
ExtraF <- function(dat, equation = "hs", groups = NULL, verbose = FALSE, k, compare = "alpha",
                   idcol = "id", xcol = "x", ycol = "y", groupcol = NULL) {

    if (missing(dat)) stop("Need to provide a dataframe!", call. = FALSE)
    origcols <- colnames(dat)
    #browser()
    dat <- CheckCols(dat, xcol = xcol, ycol = ycol, idcol = idcol, groupcol = groupcol)
    ## change from subsetting groupcol to subsetting "group"
    if (is.null(groups) && is.null(groupcol)) {
        stop("No groups selected!")
        ##grps <- unique(dat$id)
    } else if (length(groups) == 1) {
        stop("Must specify more than 1 group!")
    } else if (!is.null(groupcol) && !is.null(groups)) {
        dat <- subset(dat, group %in% groups)
        grps <- sort(groups)
    } else if (!is.null(groups) && is.null(groupcol)) {
        dat <- subset(dat, id %in% groups)
        grps <- sort(groups)
    } else if (is.null(groups) && !is.null(groupcol)) {
        grps <- sort(unique(dat$group))
    }

    if (!any(colnames(dat) %in% "group")) {
        dat$group <- dat$id
    }

     if (any(dat$y %in% 0) && (equation == "hs")) {
        warning("Zeros found in data no compatible with equation! Dropping zeros!")
        dat <- dat[dat$y != 0, , drop = FALSE]
    }

    ## find best fit k to constrain later
    if (!missing(k)) {
        bfk <- k
    } else {
        ## TODO: allow to specify id column in GetSharedK
        #kdat <- dat
        #colnames(kdat) <- c("old-id", "id", "x", "y")
        bfk <- try(GetSharedK(dat = dat, equation = equation, sharecol = "group"), silent = TRUE)
        if (class(bfk) == "character") {
            message(bfk)
            bfk <- GetK(dat)
        }
    }

    ## set references
    j <- 1
    for (i in grps) {
        #dat[dat$id == i, "ref"] <- j
        dat[dat$group == i, "ref"] <- j
        j <- j+1
    }
    dat$ref <- as.factor(dat$ref)

    ## create contrasts
    dat2 <- cbind(dat, model.matrix(~0 + ref, dat))
    nparams <- length(unique(dat2$ref))

    ## dummy code alpha
    if (compare == "q0") {
        paramsalpha <- paste(sprintf("alpha%d*ref%d", 1:nparams, 1:nparams), collapse = "+")
        startalpha <- paste(sprintf("alpha%d", 1:nparams))
        startingvals <- as.vector(c(10, rep(.001, length(startalpha))))
        names(startingvals) <- c("q0", startalpha)
    }

    ## dummy code q0
    if (compare == "alpha") {
        if (equation == "hs") {
            paramslogq0 <- paste(sprintf("log(q0%d)/log(10)*ref%d", 1:nparams, 1:nparams), collapse = "+")
            paramsq0 <- paste(sprintf("q0%d*ref%d", 1:nparams, 1:nparams), collapse = "+")
            startq0 <- paste(sprintf("q0%d", 1:nparams))
        } else if (equation == "koff") {
            paramsq0 <- paste(sprintf("q0%d*ref%d", 1:nparams, 1:nparams), collapse = "+")
            startq0 <- paste(sprintf("q0%d", 1:nparams))
        }
        startingvals <- as.vector(c(rep(10, length(startq0)), .001))
        names(startingvals) <- c(startq0, "alpha")
    }

    ## fit simple model
    if (compare == "alpha") {
        if (equation == "hs") {
            fu <- sprintf("log(y)/log(10) ~ (%s) + k * (exp(-(alpha) * (%s) * x)-1)", paramslogq0, paramsq0)
        } else if (equation == "koff") {
            fu <- sprintf("y ~ (%s) * 10 ^ (k * (exp(-(alpha) * (%s) * x) - 1))", paramsq0, paramsq0)
        }
    } else if (compare == "q0") {
        if (equation == "hs") {
            fu <- sprintf("log(y)/log(10) ~ q0 + k * (exp(-(%s) * q0 * x) - 1)", paramsalpha)
        } else if (equation == "koff") {
            fu <- sprintf("y ~ q0 * 10 ^ (k * (exp(-(%s) * q0 * x) - 1))", paramsalpha)
        }
    }

    fu <- gsub("k", bfk, fu)

    fit <- NULL
    fit <- try(nlmrt::wrapnls(fu, data = dat2, start = c(startingvals)), silent = TRUE)
    if (class(fit) == "try-error") stop("Unable to fit simple model!")
    ## fit complex model (q0 and alpha free, fixed k)
    if (equation == "hs") {
        fo <- "(log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1)"
    } else if (equation == "koff") {
        fo <- "y ~ q0  * 10 ^ (k * (exp(-(alpha * q0 * x)) - 1))"
    }
    fo <- gsub("k", bfk, fo)

    ## to hold predicted values
    newdat <- data.frame("group" = rep(grps, each = 1000),
                         "x" = rep(seq(min(unique(dat$x)),
                                       max(unique(dat$x)),
                                       length.out = 1000), times = length(grps)),
                         "y" = NA)

    ## on group by group basis
    lstfits <- list()
    for (i in grps) {
        #tmp <- subset(dat, id %in% i) ## groupcol %in% i
        tmp <- subset(dat, group %in% i)
        lstfits[[i]] <- try(nlmrt::wrapnls(formula = fo,
                           start = list(q0 = 10, alpha = 0.01),
                           control = list(maxiter = 1000),
                           data = tmp), silent = TRUE)
        if (equation == "hs") {
            newdat[newdat$group == i, "y"] <- 10^predict(lstfits[[i]],
                                                         subset(newdat, group %in% i, select = "x"))
        } else if (equation == "koff") {
            newdat[newdat$group == i, "y"] <- predict(lstfits[[i]],
                                                         subset(newdat, group %in% i, select = "x"))
        }

    }
    ss1 <- sum(resid(fit)^2)
    ss2 <- sum(sapply(sapply(sapply(lstfits, resid), function(x) x^2), sum))
    df1 <- df.residual(fit)
    df2 <- sum(sapply(lstfits, df.residual))

    F <- ((ss1-ss2)/ss2)/((df1-df2)/df2)
    pval <- 1 - pf(F, (df1-df2), df2)
    critF <- qf(c(0.025, 0.975), (df1 - df2), df2)

    print(paste0("Null hypothesis: ", if (compare == "alpha") "alpha" else "q0", " same for all data sets"))
    print(paste0("Alternative hypothesis: ", if (compare == "alpha") "alpha" else "q0", " different for each data set"))
    print(paste0("Conclusion: ", if(pval < .05) "reject" else "fail to reject", " the null hypothesis"))
    print(paste0("F(", (df1-df2), ",", df2, ") = ", round(F, 4), ", p = ", round(pval, 4)))

    cnames <- c("Group", "Q0d", "K",
                "R2", "Alpha", "N", "AbsSS", "SdRes", "EV",
                "Omaxd", "Pmaxd", "Notes", "F-Test")

    dfres <- data.frame(matrix(vector(),
                             (nparams * 2) + 2,
                             length(cnames),
                             dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    dfres[1, "Group"] <- "Shared"
    grps <- as.character(grps)
    for (i in 2:(1+nparams)) {
        dfres[i, "Group"] <- grps[(i-1)]
        if (compare == "alpha") {
            dfres[i, c("Q0d", "Alpha")] <- as.numeric(coef(fit)[c(startq0[i-1], "alpha")])
        } else if (compare == "q0") {
            dfres[i, c("Q0d", "Alpha")] <- as.numeric(coef(fit)[c("q0", startalpha[i-1])])
        }
        dfres[i, "K"] <- bfk
        dfres[i, "EV"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
        dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0d"] * dfres[i, "Alpha"] *
                                (dfres[i, "K"] ^ 1.5)) * (0.083 * dfres[i, "K"] + 0.65)
        if (equation == "hs") {
            dfres[i, "R2"] <- 1.0 - (deviance(fit)/sum((log10(dat2$y) - mean(log10(dat2$y)))^2))
            dfres[i, "Omaxd"] <- (10^(log10(dfres[i, "Q0d"]) +
                                      (dfres[i, "K"] *
                                       (exp(-dfres[i, "Alpha"] *
                                            dfres[i, "Q0d"] *
                                            dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
        } else if (equation == "koff") {
            dfres[i, "R2"] <-  1.0 -(deviance(fit)/sum((dat2$y - mean(dat2$y))^2))
            dfres[i, "Omaxd"] <- (dfres[i, "Q0d"] *
                                  (10^(dfres[i, "K"] *
                                       (exp(-dfres[i, "Alpha"] *
                                            dfres[i, "Q0d"] *
                                            dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
        }
        dfres[i, "N"] <- NROW(dat2$y)
        dfres[i, "AbsSS"] <- deviance(fit)
        dfres[i, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
        dfres[i, "Notes"] <- fit$convInfo$stopMessage
    }

    dfres[nparams+2, "Group"] <- "Not Shared"
    j <- 1
    for (i in (nparams+3):nrow(dfres)) {
        tmp <- lstfits[[j]]
        dfres[i, "Group"] <- grps[j]
        dfres[i, c("Q0d", "Alpha")] <- as.numeric(coef(tmp)[c("q0", "alpha")])
        dfres[i, "K"] <- bfk
        dfres[i, "EV"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
        dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0d"] * dfres[i, "Alpha"] *
                                (dfres[i, "K"] ^ 1.5)) * (0.083 * dfres[i, "K"] + 0.65)
        if (equation == "hs") {
            dfres[i, "R2"] <- 1.0 - (deviance(tmp)/sum((log10(subset(dat, group %in% grps[j])$y) -
                                                        mean(log10(subset(dat, group %in% grps[j])$y)))^2))
            dfres[i, "Omaxd"] <- (10^(log10(dfres[i, "Q0d"]) +
                                      (dfres[i, "K"] *
                                       (exp(-dfres[i, "Alpha"] *
                                            dfres[i, "Q0d"] *
                                            dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
        } else if (equation == "koff") {
            dfres[i, "R2"] <-  1.0 -(deviance(tmp)/sum((subset(dat, group %in% grps[j])$y -
                                                       mean(subset(dat, group %in% grps[j])$y))^2))
            dfres[i, "Omaxd"] <- (dfres[i, "Q0d"] *
                                  (10^(dfres[i, "K"] *
                                       (exp(-dfres[i, "Alpha"] *
                                            dfres[i, "Q0d"] *
                                            dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
        }
        dfres[i, "N"] <- NROW(subset(dat, group %in% grps[j])$y)
        dfres[i, "AbsSS"] <- deviance(tmp)
        dfres[i, "SdRes"] <- sqrt(deviance(tmp)/df.residual(tmp))
        dfres[i, "Notes"] <- tmp$convInfo$stopMessage
        j <- j+1
    }
    dfres[1, "F-test"] <- "Summary of F-test"
    dfres[2, "F-test"] <- paste0("Conclusion: ", if(pval < .05) "reject" else "fail to reject",
                                 " the null hypothesis")
    dfres[3, "F-test"] <- paste0("F(", (df1-df2), ",", df2, ") = ", round(F, 4),
                                 ", p = ", round(pval, 4))

    results <- list("ftest" = list("F" = F, "pval" = pval, "df1" = (df1 - df2), "df2" = df2),
                    "dfres" = dfres,
                    "newdat" = newdat,
                    "simpmodel" = fit,
                    "compmodels" = lstfits)
    if (verbose) results else invisible(results)

}


##' Finds shared k among selected datasets using global regression
##'
##' Uses global regression to fit a shared k among datasets. Assumes the dataset is in its final form. Used within FitCurves
##' @title Get Shared K
##' @param dat Dataframe (longform)
##' @param equation Character vector. Accepts either "hs" or "koff"
##' @param sharecol Character for column to find shared k. Default to "group" but can loop based on id.
##' @return Numeric value of shared k
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
GetSharedK <- function(dat, equation, sharecol = "group") {

    if (length(unique(dat[, sharecol])) == 1) {
        stop("Cannot find a shared k value with only one dataset!", call. = FALSE)
    }

    ## get rid of NAs
    dat <- dat[!is.na(dat$y), ]

    j <- 1
    for (i in unique(dat[, sharecol])) {
        dat[dat[, sharecol] == i, "ref"] <- j
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
##' Will look for maximum/minimum greater zero
##' @title Get K
##' @param dat Dataframe (long form)
##' @param mnrange Boolean for whether k should be calculated based on the mean range + .5
##' @return Numeric
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples
##' GetK(apt)
##' @export
GetK <- function(dat, mnrange = TRUE) {
    if (mnrange) {
        dat1 <- aggregate(y ~ x, dat, mean)
        (log10(max(dat1[dat1$y > 0, "y"], na.rm = TRUE)) - log10(min(dat1[dat1$y > 0, "y"], na.rm = TRUE))) + .5
    } else {
        (log10(max(dat[dat$y > 0, "y"], na.rm = TRUE)) - log10(min(dat[dat$y > 0, "y"], na.rm = TRUE))) + .5
    }
}

##' Calculates empirical measures for purchase task data
##'
##' Will calculate and return the following empirical measures: Intensity, BP0, BP1, Omax, and Pmax
##' @title GetEmpirical
##' @param dat data frame (long form) of purchase task data.
##' @param idcol The column name that should be treated as dataset identifier
##' @param xcol The column name that should be treated as "x" data
##' @param ycol The column name that should be treated as "y" data
##' @return Data frame of empirical measures
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
GetEmpirical <- function(dat, xcol = "x", ycol = "y", idcol = "id") {
    dat <- CheckCols(dat, xcol = xcol, ycol = ycol, idcol = idcol)

    ps <- unique(dat$id)
    ps <- as.character(ps)
    np <- length(ps)

    cnames <- c("ID", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    dfres <- data.frame(matrix(vector(), np, length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    for (i in seq_len(np)) {
        dfres[i, "ID"] <- ps[i]

        adf <- NULL
        adf <- dat[dat$id == ps[i], ]

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
