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
## R script for plotting demand functions
##
## lseq derived from
## @emdbook = auxiliary functions and data sets for "Ecological Models and Data", (Copyright - 2016 - Ben Bolker - GPLv2+)
## link @ https://cran.r-project.org/web/packages/emdbook/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##
##
##


##' Creates a logarithmically spaced sequence.
##'
##' A vector of logarithmically spaced values primarily used for plotting. Adapted from library("emdbook").
##' @title Create Logarithmically Spaced Sequence
##' @param from Starting value. Default value is 0.0000000001.
##' @param to Ending value. Default value is 1000.
##' @param length.out Number of values in vector. Default value is 14.
##' @return Vector
##' @author library("emdbook")
##' @export
lseq <- function(from=.0000000001, to=1000, length.out=14) {
  exp(seq(log(from), log(to), length.out = length.out))
}

## Create small ticks
##' Creates minor ticks for use in plotting.
##'
##'
##' @title Create Minor Tick Sequence
##' @param maj Value from function lseq
##' @return Vector
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
minTicks <- function(maj) {
    minticks <- vector(length = (length(maj)-1) * 10)
    for (i in 1:length(maj)) {
        if (i == length(maj)) {
            return(minticks)
        }
        if (i == 1) {
            minticks <- seq(maj[i], maj[i + 1], length.out = 10)
        } else {
            minticks <- c(minticks, seq(maj[i], maj[i + 1], length.out = 10))
        }
    }
}


##' Creates plots
##'
##' Creates and saves plots of individual demand curves
##' @title Plot Curves
##' @param adf Data frame (long form) of purchase task data.
##' @param dfrow A row of results from FitCurves
##' @param fit A nls model object from FitCurves
##' @param outdir Directory where plots are saved
##' @param fitfail Boolean whether there's a valid nls model object
##' @param tobquote Character string to be evaluated
##' @param vartext Character vector to match demand indices
##' @return Nothing
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
PlotCurves <- function(adf, dfrow, fit, outdir = "../plots/", fitfail, tobquote, vartext) {
    majlabels <- c(".0000000001", ".000000001", ".00000001", ".0000001", ".000001", ".00001", ".0001", ".001", ".01", ".1", "1", "10", "100", "1000")
    majticks <- lseq()
    minticks <- minTicks(majticks)

    if (!fitfail) {
        tempnew <- data.frame(x = seq(min(adf$x[adf$x > 0]), max(adf$x),
                                      length.out = 1000), k = dfrow[["K"]])
        if (dfrow[["Equation"]] == "hs") {
            tempnew$y <- 10^(predict(fit, newdata = tempnew))
        } else if (dfrow[["Equation"]] == "koff") {
            tempnew$y <- predict(fit, newdata = tempnew)
        }
        tempnew$expend <- tempnew$x * tempnew$y

        xmin <- min(c(tempnew$x[tempnew$x > 0], .1))
        xmax <- max(tempnew$x)
        ymin <- min(c(tempnew$y, adf$y[adf$y > 0], 1))
        ymax <- min(c(1000, max(c(tempnew$y, adf$y)))) + 5

        pdf(file = paste0(outdir, "Participant-", dfrow[["Participant"]], ".pdf"))
        par(mar = c(5, 4, 4, 4) + 0.3)
        plot(tempnew$x, tempnew$y, type = "n", log = "xy", yaxt = "n",
             xaxt = "n", bty = "l",
             xlim = c(xmin, xmax),
             ylim = c(ymin, ymax),
             xlab = "Price", ylab = "Reported Consumption",
             main = paste("Participant", dfrow[["Participant"]], sep = "-"))
        usr <- 10^par("usr")
        points(adf$x, adf$y)
        axis(1, majticks, labels = majlabels)
        axis(1, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
        axis(2, majticks, labels = majlabels, las = 1)
        axis(2, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
        lines(tempnew$y ~ tempnew$x, lwd = 1.5)

        if (!is.null(tobquote)) {
            leg <- vector("expression", length(vartext))
            for (j in seq_along(vartext)) {
                tmp <- round(dfrow[[vartext[j]]], 6)
                leg[j] <- parse(text = paste(tobquote[[j]], " == ", tmp))
            }
            legend("bottomleft", legend = leg, xjust = 0, cex = .7)
        }
        graphics.off()
    } else {
        xmin <- min(c(adf$x[adf$x > 0], .1))
        xmax <- max(adf$x)
        ymin <- min(c(adf$y[adf$y > 0], 1))
        ymax <- min(c(1000, max(adf$y))) + 5

        pdf(file = paste0(outdir, "Participant-", dfrow[["Participant"]], ".pdf"))
        par(mar = c(5, 4, 4, 4) + 0.3)
        plot(adf$x, adf$y, type = "n", log = "xy", yaxt = "n", xaxt = "n", bty = "l",
             xlim = c(xmin, xmax),
             ylim = c(ymin, ymax),
             xlab = "Price", ylab = "Reported Consumption",
             main = paste("Participant", dfrow[["Participant"]], sep = "-"))
        usr <- 10^par("usr")
        points(adf$x, adf$y)
        axis(1, majticks, labels = majlabels)
        axis(1, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
        axis(2, majticks, labels = majlabels, las = 1)
        axis(2, minticks, labels = NA, tcl = -0.25, lwd = 0, lwd.ticks = 1)
        graphics.off()
    }
}
