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
## R script for simulating data
##
## dependencies
## @reshape2 = used to melt into long form (Copyright - 2014 - Hadley Wickham - MIT)
## link @ https://cran.r-project.org/web/packages/reshape2/index.html
## license @ https://cran.r-project.org/web/licenses/MIT
##
## @nlmrt (R package) = Nash's customized optimization of L-M residual reduction (Copyright 2016 - John C. Nash - GPLv2)
## link @ https://cran.r-project.org/web/packages/nlmrt/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##
##
##

##' Simulate demand data
##'
##' Generates and saves simulated datasets in the manner specified in Koffarnus, Franck, Stein, & Bickel (2015).
##' @title Simulate Demand Data
##' @param nruns Number of runs. Default value is 10
##' @param setparams A 6x1 matrix (or 6 element vector) containing (in order) mean log10alpha, sd log10alpha, mean log10q0, sd log10q0, k, sd of consumption values across all prices
##' @param sdindex A vector of n length of sd consumption values for n prices
##' @param x A vector of n prices
##' @param outdir Optional. Directory to save results. Must end with a "/"
##' @param fn Optional. Filename of saved RData object
##' @return Invisibly a list consisting of: rounded consumption values, unrounded consumption values, simulation parameters, and inState and outState of seeds.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @examples
##' ## set values
##' setparams <- vector(length = 4)
##' setparams <- c(-2.5547, .702521, 1.239893, .320221, 3.096, 1.438231)
##' names(setparams) <- c("alphalm", "alphalsd", "q0lm", "q0lsd", "k", "yvalssd")
##' sdindex <- c(2.1978, 1.9243, 1.5804, 1.2465, 0.8104, 0.1751, 0.0380, 0.0270)
##' x <- c(.1, 1, 3, 10, 30, 100, 300, 1000)
##' set.seed(1234)
##' sim <- SimulateDemand(nruns = 1, setparams = setparams, sdindex = sdindex, x = x)
##' sim
##' @export
SimulateDemand <- function(nruns = 10, setparams, sdindex, x, outdir = NULL, fn = NULL) {
    RunOneSim <- function(run = 1, setparams, sdindex, x) {
        ## Save inSate seed
        inState <- .Random.seed
        ## Initialize vector for simulation parameters
        simparams <- vector(length = 4)
        ## Store simulation parameters and label
        simparams[1] <- rnorm(1, setparams[1], setparams[2])
        simparams[2] <- rnorm(1, setparams[3], setparams[4])
        simparams[3] <- 10^simparams[1]
        simparams[4] <- 10^simparams[2]
        names(simparams) <- c("alphalr", "q0lr", "alphar", "q0r")
        ## Save outState seed
        outState <- .Random.seed
        ## Calculate consumption values at each price
        sim <- function(x, sdindex) {
            m <- 10^(simparams["q0lr"] + setparams[5] *
                         ((exp((-1 * simparams["alphar"]) * simparams["q0r"] * x)) - 1))
            s <- sdindex * setparams[6]
            rnorm(1, m, s)
        }
        ysim <- mapply(sim, x, sdindex)
        ## Round resulting consumption values, truncating at 0
        ysimr <- pmax(round(ysim, 0), 0)
        ## Return list: unrounded and rounded consumption values,
        ## simulation parameters, inState and outState of seed
        list(dat = data.frame("y"  = ysim, "yr" = ysimr),
             "simparams" = simparams, "inState" = inState, "outState" = outState)
    }

    runs <- seq_len(nruns)
    manysims <- lapply(runs, RunOneSim, setparams, sdindex, x)
    simvalues <- sapply(manysims, "[[", 1)
    simparams <- sapply(manysims, "[[", 2)
    seeds <- sapply(manysims, "[", 3:4)

    simvaluesraw <- simvalues[-2, , drop = FALSE]
    simvalues <- simvalues[-1, , drop = FALSE]
    simvaluesraw <- do.call("cbind", simvaluesraw)
    simvalues <- do.call("cbind", simvalues)
    simvaluesraw <- cbind(x, simvaluesraw)
    simvalues <- cbind(x, simvalues)
    colnames(simvaluesraw) <- c("x", runs)
    colnames(simvalues) <- c("x", runs)

    rownames(simparams) <- c("alphalr", "q0lr", "alphar", "q0r")
    colnames(simparams) <- runs
    simparams <- t(simparams)

    colnames(seeds) <- runs
    seeds <- t(seeds)

    ## Reshape long
    simvalues <- data.frame(simvalues)
    simvalues <- reshape2::melt(simvalues, id.vars = "x")
    colnames(simvalues) <- c("x", "id", "y")
    simvalues <- simvalues[, c("id", "x", "y")]
    simvalues$id <- gsub("X", "", simvalues$id)
    simvalues$id <- as.numeric(simvalues$id)

    simvaluesraw <- data.frame(simvaluesraw)
    simvaluesraw <- reshape2::melt(simvaluesraw, id.vars = "x")
    colnames(simvaluesraw) <- c("x", "id", "y")
    simvaluesraw <- simvaluesraw[, c("id", "x", "y")]
    simvaluesraw$id <- gsub("X", "", simvaluesraw$id)
    simvaluesraw$id <- as.numeric(simvaluesraw$id)

    if (!is.null(outdir) && !is.null(fn)) {
        if (!dir.exists(outdir)) {
            dir.create(outdir)
        }
        saveobjects <- c("simvalues", "simvaluesraw", "simparams", "seeds")
        save(list = saveobjects, file = paste0(outdir, fn, ".RData"))
    }

    invisible(list("simvalues" = simvalues, "simvaluesraw" = simvaluesraw,
                   "simparams" = simparams, "seeds" = seeds))
}


##' Gets values used in SimulateDemand
##'
##' Gets values used in SimulateDemand
##' @title Get Values for SimulateDemand
##' @param dat Dataframe (long form)
##' @return List of 2: setaparams, sdindex
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @examples
##' GetValsForSim(apt)
##' @export
GetValsForSim <- function(dat) {

    k <- log10(max(dat[dat$y > 0, "y"])) - log10(min(dat[dat$y > 0, "y"]))

    ## Get N unique participants, prices informing loops
    participants <- unique(dat$id)
    np <- length(participants)
    nprices <- length(unique(dat$x))

    cnames <- c("Participant", "Q0", "Alpha", as.numeric(unique(dat$x)))
    dfres <- data.frame(matrix(vector(),
                               np,
                               length(cnames),
                               dimnames = list(c(), c(cnames))),
                        stringsAsFactors = FALSE)

    for (i in seq_len(np)) {
        dfres[i, "Participant"] <- participants[i]

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]
        adf[, "k"] <- k

        fit <- NULL
        fit <- try(nlmrt::wrapnls(data = adf,
                                  y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1)),
                                  start = list(q0 = 10, alpha = 0.01),
                                  control = list(maxiter = 1000)), silent = TRUE)

        if (!class(fit) == "try-error") {
            dfres[i, c("Q0", "Alpha")] <- as.numeric(coef(fit)[c("q0", "alpha")])
            dfres[i, 4:NCOL(dfres)] <- resid(fit)
        }
    }

    setparams <- vector("numeric", 6)
    setparams[1] <- log10(mean(dfres$Alpha, na.rm = TRUE))
    setparams[2] <- log10(sd(dfres$Alpha, na.rm = TRUE))
    setparams[3] <- log10(mean(dfres$Q0, na.rm = TRUE))
    setparams[4] <- log10(sd(dfres$Q0, na.rm = TRUE))
    setparams[5] <- k
    setparams[6] <- sd(dat$y)
    names(setparams) <- c("alphalm", "alphalsd", "q0lm", "q0lsd", "k", "yvalssd")

    sdindex <- vector("numeric", nprices)

    for (i in 4:NCOL(dfres)) {
        j <- i - 3
        sdindex[j] <- sd(dfres[ , i], na.rm = TRUE)
    }
    list("setparams" = setparams, "sdindex" = sdindex)
}










