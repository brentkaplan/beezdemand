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

##' Analyzes purchase task data
##'
##' Analyzes purchase task data
##' @title FitCurves
<<<<<<< HEAD
##'
##' @param mat: data frame of purchase task data.
=======
##' @param mat data frame (long form) of purchase task data.
>>>>>>> refs/remotes/bkaplan4/master
##' @param equation Character vector of length one. Accepts either "hs" for Hursh and Silberberg (2008) or "koff" for Koffarnus, Franck, Stein, and Bickel (2015). If "hs" and the first price (x) is 0, it will be replaced by replfree.
##' @param k A numeric vector of length one. Reflects the range of consumption in log10 units. If none provided, k will be calculated based on the max/min of the entire sample
##' @param remq0e If TRUE, removes consumption and price where price == 0. Default value is FALSE
##' @param replfree Optionally replaces price == 0 with specified value. Note, if fitting using equation == "hs", and 0 is first price, 0 gets replaced by replfree. Default value is .01
<<<<<<< HEAD
##'
##' @return Data frame, fitting params, CI's/SE's and notes
##' @author Shawn Gilroy <shawn.gilroy@temple.edu>
=======
##' @param rem0 If TRUE, removes all 0s in consumption data prior to analysis. Default value is FALSE.
##' @return Data frame, fitting params and CI's/SE's
##' @author Shawn Gilroy <shawn.gilroy@temple.edu> Brent Kaplan <bkaplan4@@ku.edu>
>>>>>>> refs/remotes/bkaplan4/master
##' @export
FitCurves <- function(mat, equation, k = NULL, remq0e = FALSE, replfree = NULL, rem0 = FALSE) {

<<<<<<< HEAD
    ## Workaround to make sure nlstools is available and referenced
    if (!require(nlstools))
    {
      install.packages('nlstools', repos = 'http://cran.us.r-project.org')
      require(nlstools)
      library(nlstools)
=======
    ## Assert not Inf
    if (is.infinite(k)) {
        warning("k is Inf. I will calculate a k based on the entire sample.")
        k <- log10(max(mat[mat$y > 1, "y"])) - log10(min(mat[mat$y > 1, "y"]))
>>>>>>> refs/remotes/bkaplan4/master
    }

    ## If no k is provided
    if (is.null(k)) {
        k <- log10(max(mat[mat$y > 1, "y"])) - log10(min(mat[mat$y > 1, "y"]))
    }

<<<<<<< HEAD
    ## Get n unique participants, informing loop
=======
    ## Get unique participants, informing loop
>>>>>>> refs/remotes/bkaplan4/master
    participants <- length(unique(mat$p))

    ## Preallocate for speed
    cnames <- c("Participant", "Q0e", "BP0", "BP1", "Omaxe", "Pmaxe", "Equation", "Q0", "K",
                "R2", "Alpha", "Q0se", "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
                "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Notes")

    dfres <- data.frame(matrix(vector(), participants, length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    ## Loop through unique values as indices, not necessarily sequentially
<<<<<<< HEAD
    for (i in unique(mat$p))
    {
      DataFrameResult[i,]$Participant = i
      DataFrameResult[i,]$Mean = mean(mat[mat$p==i,]$y)
      DataFrameResult[i,]$Median = median(mat[mat$p==i,]$y)

      adf <- NULL
      adf <- mat[mat$p==i,]
      adf[,"k"] <- k

      if (equation == "hs")
      {

        ## Workaround for Hursh's zero point issues
        if (remq0e)
        {
          ## Drop any zero comsumption points altogether
          adf <- adf[adf$y != 0,]
        }

        ## Skirt 0 domain values away from intercept, for Hursh initial zero point issues
        if (adf[1,"x"] == 0.0)
        {
          adf[1,"x"] <- replfree
        }
=======
    for (i in unique(mat$p)) {
        dfres[i, "Participant"] <- i
        dfres[i, "Equation"] <- equation
        #browser()
        adf <- NULL
        adf <- mat[mat$p == i, ]
        adf[, "expend"] <- adf$x * adf$y
        adf[, "k"] <- k

        ## Find empirical Q0, BP0, BP1
        dfres[i, "Q0e"] <- if (0 %in% adf$x) adf[adf$x == 0, "y"] else NA
        dfres[i, "BP0"] <- if (0 %in% adf$y) min(adf[adf$y == 0, "x"]) else NA
        dfres[i, "BP1"] <- if (!0 %in% adf$y) max(adf[adf$y != 0, "x"]) else NA

        ## Find empirical Pmax, Omax
        dfres[i, "Omaxe"] <- max(adf$expend)
        dfres[i, "Pmaxe"] <- adf[max(which(adf$expend == max(adf$expend))), "x"]

        if (equation == "hs") {
>>>>>>> refs/remotes/bkaplan4/master

            ## If retain y where x = 0, replace
            if (remq0e) {
                adf <- adf[adf$x != 0, ]
            } else {
                replfree <- if (is.null(replfree)) 0.01 else replfree
                adf[adf$x == 0, "x"] <- replfree
            }

            ## Drop any zero consumption points altogether
            adf <- adf[adf$y != 0, ]

            fit <- NULL
            try(fit <- nlmrt::wrapnls(data = adf,
                               (log(y)/log(10)) ~ (log(q0)/log(10)) + k * (exp(-alpha * q0 * x) - 1),
                               start = list(q0 = 10, alpha = 0.01),
                               control = list(maxiter = 1000)), silent = TRUE)

            if (!is.null(fit)) {
                dfres[i, c("Q0", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
                dfres[i, "K"] <- min(adf$k)
                dfres[i, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 2), 2]
                dfres[i, "N"] <- length(adf$k)
                dfres[i, "R2"] <- 1.0 - (deviance(fit)/sum((log10(adf$y) - mean(log10(adf$y)))^2))
                dfres[i, "AbsSS"] <- deviance(fit)
                dfres[i, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
                dfres[i, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 3)]
                dfres[i, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(2, 4)]
                dfres[i, "EV"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
                dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0"] * dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5)) *
                                      (0.083 * dfres[i, "K"] + 0.65)
                dfres[i, "Omaxd"] <- (10^(log10(dfres[i, "Q0"]) + (dfres[i, "K"] * (exp(-dfres[i, "Alpha"] *
                                      dfres[i, "Q0"] * dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
            }
        } else {
            if (equation == "koff") {

                if (rem0) {
                    adf <- adf[adf$y != 0, ]
                }

                fit <- NULL
                try(fit <- nlmrt::wrapnls(data = adf,
                                   y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                                   start = list(q0 = 10, alpha = 0.01),
                                   control = list(maxiter = 1000)), silent = TRUE)

                if (!is.null(fit)) {
                    dfres[i, c("Q0", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
                    dfres[i, "K"] <- min(adf$k)
                    dfres[i, c("Q0se", "Alphase")] <- summary(fit)[[10]][c(1, 2), 2]
                    dfres[i, "N"] <- length(adf$k)
                    dfres[i, "R2"] <-  1.0 -(deviance(fit)/sum((adf$y - mean(adf$y))^2))
                    dfres[i, "AbsSS"] <- deviance(fit)
                    dfres[i, "SdRes"] <- sqrt(deviance(fit)/df.residual(fit))
                    dfres[i, c("Q0Low", "Q0High")] <- nlstools::confint2(fit)[c(1, 3)]
                    dfres[i, c("AlphaLow", "AlphaHigh")] <- nlstools::confint2(fit)[c(2, 4)]
                    dfres[i, "EV"] <- 1/(dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5) * 100)
                    dfres[i, "Pmaxd"] <- 1/(dfres[i, "Q0"] * dfres[i, "Alpha"] * (dfres[i, "K"] ^ 1.5)) *
                                          (0.083 * dfres[i, "K"] + 0.65)
                    dfres[i, "Omaxd"] <- (dfres[i, "Q0"] * (10^(dfres[i, "K"] * (exp(-dfres[i, "Alpha"] *
                                          dfres[i, "Q0"] * dfres[i, "Pmaxd"]) - 1)))) * dfres[i, "Pmaxd"]
                }
            }
        }
    }
<<<<<<< HEAD
    
    return(DataFrameResult)
=======
    return(dfres)
>>>>>>> refs/remotes/bkaplan4/master
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
analyze <- function(adf = NULL, eq = NULL, nltype = NULL, k = NULL,
                    q0st = 10, ast = .001, seetrace = FALSE, replfree = .01) {
    ## Change fo to eq? Can allow for user to put in their own equation
    fo <- switch(eq,
                 "hs" = log(y, 10) ~ log(q0, 10) + (k * (exp(-alpha * q0 * x) - 1)),
                 "koff" = y ~ q0 * (10 ^ (k * (exp(-alpha * q0 * x) - 1))),
                 stop("No equation specified"))
    if (is.null(k)) stop("No value for k specified")
    if (is.null(nltype)) stop("No optimizer specified")

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
        mod <- try(nls(fo, start = list(q0 = q0st, alpha = ast),
                       control = list(maxiter = 1000, warnOnly = TRUE),
                       trace = seetrace, data = adf), silent = TRUE)
    }
    if (nltype == "lm") {
        mod <- try(minpack.lm::nlsLM(fo, start = list(q0 = q0st, alpha = ast),
                         control = list(maxiter = 1000, warnOnly = TRUE, maxfev = 500),
                         trace = seetrace, data = adf), silent = TRUE)
    }
    if (nltype == "brute") {
        grid.start <- expand.grid(list(q0 = c(max(adf$y)),
                                       alpha = seq(.00000001, .1, length.out = 100)))
        start.m <- nls2::nls2(fo, data = adf, start = grid.start,
                        algorithm = "brute-force")
        mod <- try(nls2::nls2(fo, start = start.m, algorithm = "port",
                        lower = list(0.001, 0.0000000001),
                        upper = list((max(adf$y) * 1.5), 1),
                        control = list(maxiter = 1000, warnOnly = TRUE),
                        trace = seetrace, data = adf), silent = TRUE)
    }
    if(inherits(mod, "try-error")) {
        mod <- mod[1]
        attr(mod, "eq") <- eq
        attr(mod, "nltype") <- nltype
        attr(mod, "k") <- k
    } else {
        attr(mod, "eq") <- eq
        attr(mod, "nltype") <- nltype
        attr(mod, "k") <- k
    }
    mod
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
