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
##
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
##' @author Brent Kaplan <bkaplan4@@ku.edu>
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
##' @return Dataframe with descriptive statistics
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @examples
##' GetDescriptives(apt)
##' @export
GetDescriptives <- function(dat) {

    ## Get N unique prices
    prices <- unique(dat$x)
    np <- length(prices)

    dfres <- data.frame(matrix(vector(), 4, np,
                               dimnames = list(c("Mean", "SD", "PropZeros", "NAs"),
                                   c(prices))), stringsAsFactors = FALSE)
    colnames(dfres) <- gsub("X", "", colnames(dfres))

    dfres["Mean", ] <- aggregate(y ~ x, dat,
                                 function(x) round(mean(x, na.rm = TRUE),2))$y
    dfres["SD", ] <- aggregate(y ~ x, dat,
                               function(x) round(sd(x, na.rm = TRUE),2))$y
    dfres["PropZeros", ] <- aggregate(y ~ x, dat,
                                      function(x) round(sum(x == 0, na.rm = TRUE)/length(x), 2))$y
    dfres["NAs", ] <- aggregate(y ~ x, dat,
                                function(x) sum(is.na(x)))$y

    dfres
}



##' Applies Stein, Koffarnus, Snider, Quisenberry, & Bickel's (2015) criteria for identification of nonsystematic purchase task data.
##'
##' This function applies the 3 criteria proposed by Stein et al., (2015) for identification of nonsystematic purchase task data. The three criteria include trend (deltaq), bounce, and reversals from 0. Also flags for a minimum number of positive consumption values.
##' @title Systematic Purchase Task Data Checker
##' @param mat A matrix in wide form where each column is a participant's responses. If a dataframe is provided, it will attempt to coerce it to a matrix.
##' @param x A vector of prices used in the purchase task. The number of elements must be equal to the number of rows in the matrix.
##' @param deltaq Numeric vector of length equal to one. The criterion by which the relative change in quantity purchased will be compared. Relative changes in quantity purchased below this criterion will be flagged. Default value is 0.025.
##' @param bounce Numeric vector of length equal to one. The criterion by which the number of price-to-price increases in consumption that exceed 25\% of initial consumption at the lowest price, expressed relative to the total number of price increments, will be compared. The relative number of price-to-price increases above this criterion will be flagged. Default value is 0.10.
##' @param reversals Numeric vector of length equal to one. The criterion by which the number of reversals from 0 will be compared. Number of reversals above this criterion will be flagged. Default value is 0.
##' @param minpos Minimum positive values to be checked against. Default value if 4.
##' @return A dataframe providing individual values of deltaq, bounce, reversals, and number of positive consumption values. Also contains passing information for each criterion, as well as a total number of passes (total number of passes applies only to Stein et al.'s criteria). Prints the number of cases and the percentage of cases that fail at least one criterion.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
systemCheck <- function(mat = NULL, x = NULL, deltaq = 0.025, bounce = 0.10, reversals = 0, minpos = 4) {
    if (is.null(mat) || is.null(x)) stop("You are missing either a matrix or vector of prices")
    if (is.data.frame(mat)) mat <- as.matrix(mat)
    critCheck <- function(y, x, deltaq, bounce, reversals, minpos) {
        y01 <- y + .01
        x01 <- x + .01
        crit1 <- (log(y01[1]) - log(y01[length(y)])) /
            (log(x01[length(x)]) - log(x01[1]))
        crit2 <- sum(diff(y) > y[1] * 0.25, na.rm = TRUE) / (length(y) - 1)
        crit3 <- if (length(which(y == 0)) == 0) 0 else sum(diff(y[min(which(y == 0)):length(y)]) > 0, na.rm = TRUE)
        numpos <- length(which(y != 0))
        crit1pass <- ifelse(crit1 >= deltaq, 1, 0)
        crit2pass <- ifelse(crit2 <= bounce,  1, 0)
        crit3pass <- ifelse(crit3 <= reversals,  1, 0)
        numpospass <- ifelse(numpos >= minpos, 1, 0)
        res <- c("Total_Pass" = crit1pass + crit2pass + crit3pass,
                 "DeltaQ" = round(crit1, 4),
                 "DeltaQ_Pass" = ifelse(crit1pass == 1, 1, 0),
                 "Bounce" = round(crit2, 4),
                 "Bounce_Pass" = ifelse(crit2pass == 1, 1, 0),
                 "Reversals" = crit3,
                 "Reversals_Pass" = ifelse(crit3pass == 1, 1, 0),
                 "Number_Postive_Values" = numpos,
                 "Minimum_Positive_Values" = ifelse(numpospass == 1, 1, 0))
        return(res)
    }
    rownames(mat) <- NULL
    res <- as.data.frame(t(apply(mat, 2, critCheck, x, deltaq, bounce, reversals, minpos)))
    for (i in c("DeltaQ_Pass", "Bounce_Pass", "Reversals_Pass", "Minimum_Positive_Values")) {
        res[ , i] <- gsub(1, "Yes", res[ , i])
        res[ , i] <- gsub(0, "No", res[ , i])
    }
    res <- cbind("ID" = attributes(mat)$dimnames[[2]], res)
    cat(paste0("I examined ", nrow(res), " participants' data.\n There were ", length(which(res$Total_Pass < 3)), " participant(s) that failed at least one criterion, representing ", (round(length(which(res$Total_Pass < 3)) / nrow(res), 3)) * 100, "% of the sample.\n Additionally, there were ", length(which(res$Minimum_Positive_Values == "No")), " participants who had less than ", minpos, " non-zero (positive) datapoints.\n Please consider examining these participant(s)"))
    res
}

##' Makes a summary table.
##'
##' Makes a summary table from the return from doEverything.
##' @title Make Summary Table
##' @param reslst The return from doEverything.
##' @return A dataframe with the following indices: empirical q0 (q0e), derived q0 (q0d), alpha (alpha), essential value (ev), empirical pmax (pmaxe), derived pmax using empirical q0 (pmaxdq0e), derived pmax using derived q0 (pmaxdq0d), empirical omax (omaxe), derived omax using empirical q0e (omaxdq0e), derived omax using derived q0 (omaxdq0d), k (k), standard error of derived q0 (q0se), standard error of alpha (alphase), Sy.x (Sy.x), number of points analyzed (pointsanalyzed), degrees of freedom (df), convergence notes from model (convergnotes), equation used in fitting (eq), optimizer used in fitting (nltype), whether empirical q0 was used in fitting (remq0e), whether a parallel shift was used (parshift), replacement number for 0s if applicable (replnum), whether 0s were included in fitting (incl0s).
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
makeSumTable <- function(reslst) {
    flatten <- function(indices) {
        temp1 <- apply(indices, 2, function(x) class(unlist(x)))
        types <- unique(temp1)
        index <- list()
        for (i in seq_along(types)) {
            index[[i]] <- assign(types[i], which(temp1 == types[i]))
            index[[i]] <- apply(indices[, index[[i]]], 2, unlist)
        }
        sumtable <- data.frame(index, stringsAsFactors = FALSE)
        sumtable
    }
    target <- c("q0e", "q0d", "alpha", "ev", "pmaxe", "pmaxdq0e", "pmaxdq0d", "omaxe",
                "omaxdq0e", "omaxdq0d", "k", "q0se", "alphase", "Sy.x", "pointsanalyzed",
                "df", "convergnotes", "eq", "nltype", "remq0e",
                "parshift", "replnum", "incl0s")
    indices <- t(sapply(reslst, "[[", 4))
    sumtable <- flatten(indices)
    sumtable <- sumtable[ , match(target, names(sumtable))]
    sumtable
}

##' Calculates descriptive statistics from purchase task data.
##'
##' Provides the following descriptive statistics from purchase task data at each price: mean consumption, standard deviation of consumption, and percentage of 0 values.
##' @title Purchase Task Descriptive Summary
##' @param mat A matrix of purchase task data.
##' @param x A vector of prices coinciding with purchase task data.
##' @return A matrix of descriptive statistics.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
conssum <- function(mat, x = x) {
    consdescr <- matrix(NA, length(x), 3)
    mat <- as.matrix(mat)
    for (i in 1:length(x)) {
        consdescr[i, 1] <- mean(mat[i, 1:NCOL(mat)])
        consdescr[i, 2] <- sd(mat[i, 1:NCOL(mat)])
        consdescr[i, 3] <- (sum(mat[i, 1:NCOL(mat)] == 0))/NCOL(mat)
    }
    consdescr <- cbind(x, consdescr)
    colnames(consdescr) <- c("Price", "Mean", "SD", "Proportion 0s")
    consdescr
}
