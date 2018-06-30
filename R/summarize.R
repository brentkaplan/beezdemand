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
    
    dat$x <- if (!is.numeric(dat$x)) as.numeric(dat$x) else dat$x
    dat$y <- if (!is.numeric(dat$y)) as.numeric(dat$y) else dat$y
  
    if (any(is.na(dat$y))) {
      warning("NA values found in y column. Dropping NAs and continuing")
      dat <- dat[!is.na(dat$y), ]
    }
  
    ## Get N unique participants, informing loop
    ps <- unique(dat$id)
    ps <- as.character(ps)
    np <- length(ps)

    cnames <- c("ID", "TotalPass", "DeltaQ", "DeltaQPass", "Bounce",
                "BouncePass", "Reversals", "ReversalsPass", "NumPosValues")

    dfres <- data.frame(matrix(vector(),
                               np,
                               length(cnames),
                               dimnames = list(c(), c(cnames))), stringsAsFactors = FALSE)

    for (i in seq_len(np)) {
        dfres[i, "ID"] <- ps[i]

        adf <- NULL
        adf <- dat[dat$id == ps[i], ]
        adf <- adf[order(adf$x), ]

        adf[, c("x01", "y01")] <- adf[, c("x", "y")] + .01

        dfres[i, "DeltaQ"] <- round((log10(adf[1, "y01"]) - log10(adf[nrow(adf), "y01"])) /
            (log10(adf[nrow(adf), "x01"]) - log10(adf[1, "x01"])), 4)
        dfres[i, "Bounce"] <- round(sum(diff(adf$y) > adf[1, "y"] * 0.25, na.rm = TRUE)  /
            (nrow(adf) - 1), 4)

        nrev <- NULL
        if (0 %in% adf$y) {
            z <- which(adf$y == 0)
            if (ncons0 == 2) {
                if (any(z > NROW(adf) - 2)) {
                    z <- z[-which(z > NROW(adf) - 2)]
                   }
                if (length(z) > 0) {
                    for (j in (min(z)):(NROW(adf) - 2)) {
                        if (adf$y[j] == 0 && adf$y[j + 1] == 0 && adf$y[j + 2] != 0) {
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
                            if (adf$y[j] == 0 && adf$y[j + 1] != 0) {
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

        dfres[i, "NumPosValues"] <- length(adf$y[adf$y != 0])
        dfres[i, "DeltaQPass"] <- ifelse(dfres[i, "DeltaQ"] >= deltaq, "Pass", "Fail")
        dfres[i, "BouncePass"] <- ifelse(dfres[i, "Bounce"] <= bounce, "Pass", "Fail")
        dfres[i, "ReversalsPass"] <- ifelse(dfres[i, "Reversals"] <= reversals, "Pass", "Fail")
        dfres[i, "TotalPass"] <- length(grep("Pass", dfres[i, ]))
    }
    dfres
}


##' Calculates descriptive statistics from purchase task data.
##'
##' Provides the following descriptive statistics from purchase task data at each price: mean consumption, median consumption, standard deviation of consumption, proportion of 0 values, number of NAs, minimum consumption, and maximum consumption.
##' @title Get Purchase Task Descriptive Summary
##' @param dat Dataframe (long form)
##' @param bwplot Boolean. If TRUE, a ggplot2 box and whisker plot is saved. Default is FALSE.
##' @param outdir Character. Directory where plot will be saved. Be sure to include trailing '/'. Default location is one level up in "../plots/".
##' @param device Character. Type of file. Default is "png". Can be "pdf".
##' @param filename Character. Specify filename. Defualt is "bwplot". 
##' @return Dataframe with descriptive statistics
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples
##' GetDescriptives(apt)
##' @export
GetDescriptives <- function(dat, bwplot = FALSE, outdir = "../plots/", device = "png", filename = "bwplot") {

    ## Get N unique prices
    prices <- unique(dat$x)
    np <- length(prices)
    cnames <- c("Price", "Mean", "Median", "SD", "PropZeros", "NAs", "Min", "Max")
    dfres <-  data.frame(matrix(vector(),
                                np,
                                length(cnames),
                                dimnames = list(c(), cnames)),
                         stringsAsFactors = FALSE)
    dfres$Price <- gsub("X", "", prices)

    dfres[, "Mean"] <- aggregate(y ~ x, dat,
                                 function(x) round(mean(x, na.rm = TRUE),2))$y
    dfres[, "Median"] <- aggregate(y ~ x, dat,
                                 function(x) round(median(x, na.rm = TRUE),2))$y
    dfres[, "SD"] <- aggregate(y ~ x, dat,
                               function(x) round(sd(x, na.rm = TRUE),2))$y
    dfres[, "PropZeros"] <- aggregate(y ~ x, dat,
                                      function(x) round(sum(x == 0, na.rm = TRUE)/length(x), 2))$y
    dfres[, "NAs"] <- aggregate(y ~ x, dat,
                                function(x) sum(is.na(x)))$y
    dfres[, "Min"] <- aggregate(y ~ x, dat,
                                   function(x) round(min(x, na.rm = TRUE),2))$y
    dfres[, "Max"] <- aggregate(y ~ x, dat,
                                 function(x) round(max(x, na.rm = TRUE),2))$y

    if (bwplot) {
        if (!dir.exists(outdir)) dir.create(outdir)
        bwplt <- ggplot2::ggplot(dat, aes(x = as.factor(x), y = y)) +
            ggplot2::geom_boxplot() +
            ggplot2::stat_summary(fun.y = mean, geom = "point", shape = 43, size = 5, color = "red") +
            ggplot2::labs(x = "Price", y = "Reported Consumption") +
            theme_apa()
        print(bwplt)
        if (device == "png") {
          png(paste0(outdir, filename, ".png"), width = 800, height = 600, res = 120)
          suppressWarnings(print(bwplt))
          graphics.off()
        } else if (device == "pdf") {
          pdf(paste0(outdir, filename, ".pdf"))
          suppressWarnings(print(bwplt))
          graphics.off()
        }
    }
    dfres
}


