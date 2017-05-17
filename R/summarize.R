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
## R script for summarizing demand data
##
## @ggplot2 = grammar of graphics
## link @ https://cran.r-project.org/web/packages/ggplot2/index.html
## license @ https://cran.r-project.org/web/licenses/GPL-2
##

##' Applies Stein, Koffarnus, Snider, Quisenberry, & Bickel's (2015) criteria for identification of nonsystematic purchase task data.
##'
##' This function applies the 3 criteria proposed by Stein et al., (2015) for identification of nonsystematic purchase task data. The three criteria include trend (deltaq), bounce, and reversals from 0. Also reports number of positive consumption values.
##' @title Systematic Purchase Task Data Checker
##' @param dat Dataframe in long form. Colums are id, x, y.
##' @param deltaq Numeric vector of length equal to one. The criterion by which the relative change in quantity purchased will be compared. Relative changes in quantity purchased below this criterion will be flagged. Default value is 0.025.
##' @param bounce Numeric vector of length equal to one. The criterion by which the number of price-to-price increases in consumption that exceed 25\% of initial consumption at the lowest price, expressed relative to the total number of price increments, will be compared. The relative number of price-to-price increases above this criterion will be flagged. Default value is 0.10.
##' @param reversals Numeric vector of length equal to one. The criterion by which the number of reversals from number of consecutive (see ncons0) 0s will be compared. Number of reversals above this criterion will be flagged. Default value is 0.
##' @param ncons0 Numer of consecutive 0s prior to a positive value is used to flag for a reversal. Value can be either 1 (relatively more conservative) or 2 (default; as recommended by Stein et al., (2015).
##' @return Dataframe
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples
##' ## Using all default values
##' CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 2)
##' ## Specifying just 1 zero to flag as reversal
##' CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 1)
##' @export
CheckUnsystematic <- function(dat, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 2) {

    ## Get N unique participants, informing loop
    participants <- unique(dat$id)
    np <- length(participants)

    cnames <- c("Participant", "TotalPass", "DeltaQ", "DeltaQPass", "Bounce",
                "BouncePass", "Reversals", "ReversalsPass", "NumPosValues")

    dfres <- data.frame(matrix(vector(),
                               np,
                               length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    for (i in seq_len(np)) {
        dfres[i, "Participant"] <- participants[i]

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]


        adf[, c("x01", "y01")] <- adf[, c("x", "y")] + .01

        dfres[i, "DeltaQ"] <- round((log10(adf[1, "y01"]) - log10(adf[nrow(adf), "y01"])) /
            (log10(adf[nrow(adf), "x01"]) - log10(adf[1, "x01"])), 4)
        dfres[i, "Bounce"] <- round(sum(diff(adf[, "y"]) > adf[1, "y"] * 0.25, na.rm = TRUE)  /
            (nrow(adf) - 1), 4)

        nrev <- NULL
        if (0 %in% adf[, "y"]) {
            z <- which(adf[, "y"] == 0)
            if (ncons0 == 2) {
                if (any(z > NROW(adf) - 2)) {
                    z <- z[-which(z > NROW(adf) - 2)]
                   }
                if (length(z) > 0) {
                    for (j in (min(z)):(NROW(adf) - 2)) {
                        if (adf[j, "y"] == 0 && adf[j + 1, "y"] == 0 && adf[j + 2, "y"] != 0) {
                            nrev[j] <- 1
                        } else {
                            next
                        }
                    }
                    dfres[i, "Reversals"] <- sum(nrev, na.rm = TRUE)
                } else {
                    dfres[i, "Reversals"] <- 0
                }
            } else {
                if (ncons0 == 1) {
                    if (any(z > NROW(adf) - 1)) {
                        z <- z[-which(z > NROW(adf) - 1)]
                    }
                    if (length(z) > 0) {
                        for (j in (min(z)):(NROW(adf) - 1)) {
                            if (adf[j, "y"] == 0 && adf[j + 1, "y"] != 0) {
                                nrev[j] <- 1
                            } else {
                                next
                            }
                        }
                        dfres[i, "Reversals"] <- sum(nrev, na.rm = TRUE)
                    } else {
                        dfres[i, "Reversals"] <- 0
                    }
                }
            }
        } else {
            dfres[i, "Reversals"] <- 0
        }

        dfres[i, "NumPosValues"] <- length(adf[ adf$y != 0, "y"])
        dfres[i, "DeltaQPass"] <- ifelse(dfres[i, "DeltaQ"] >= deltaq, "Pass", "Fail")
        dfres[i, "BouncePass"] <- ifelse(dfres[i, "Bounce"] <= bounce, "Pass", "Fail")
        dfres[i, "ReversalsPass"] <- ifelse(dfres[i, "Reversals"] <= reversals, "Pass", "Fail")
        dfres[i, "TotalPass"] <- length(grep("Pass", dfres[i, ]))
    }
    dfres
}


##' Calculates descriptive statistics from purchase task data.
##'
##' Provides the following descriptive statistics from purchase task data at each price: mean consumption, standard deviation of consumption, proportion of 0 values, and number of NAs.
##' @title Get Purchase Task Descriptive Summary
##' @param dat Dataframe (long form)
##' @param bwplot Boolean. If TRUE, box and whisker plot is saved into "../plots/" directory. Default is FALSE
##' @return Dataframe with descriptive statistics
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples
##' GetDescriptives(apt)
##' @export
GetDescriptives <- function(dat, bwplot = FALSE) {

    ## Get N unique prices
    prices <- unique(dat$x)
    np <- length(prices)
    browser()
    dfres <-  data.frame(matrix(vector(), np, 5,
                                dimnames = list(c(),
                                                c("Price", "Mean", "SD", "PropZeros", "NAs"))),
                         stringsAsFactors = FALSE)
    dfres$Price <- gsub("X", "", prices)

    dfres[, "Mean"] <- aggregate(y ~ x, dat,
                                 function(x) round(mean(x, na.rm = TRUE),2))$y
    dfres[, "SD"] <- aggregate(y ~ x, dat,
                               function(x) round(sd(x, na.rm = TRUE),2))$y
    dfres[, "PropZeros"] <- aggregate(y ~ x, dat,
                                      function(x) round(sum(x == 0, na.rm = TRUE)/length(x), 2))$y
    dfres[, "NAs"] <- aggregate(y ~ x, dat,
                                function(x) sum(is.na(x)))$y

    if (bwplot) {
        if (!dir.exists("../plots/")) dir.create("../plots/")
        basedir <- "../plots/"
        basename <- "bwplot-"
        outdir <- createOutdir(basedir = basedir, basename = basename)[[1]]

        ## pdf(file = paste0(outdir, "bwplot", ".pdf"), width = 7, height = 6)
        ## boxplot(dat$y ~ dat$x, xlab = "Price", ylab = "Reported Consumption")
        ## dev.off()

        png(file = paste0(outdir, "bwplot.png"), width = 7, height = 6)
        ggplot(dat, aes(x = as.factor(x), y = y)) +
            geom_boxplot() +
            labs(x = "Price", y = "Reported Consumption") 
            #+ theme_apa()
        dev.off()
    }
    dfres
}


