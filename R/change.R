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
## R script for changing values and splitting data
##
##
##

##' Replaces 0 values
##'
##' Replaces specified number of 0s with replacement value.
##' @title Replace Zeros
##' @param dat Dataframe (long form)
##' @param nrepl Number of zeros to replace with replacement value (replnum). Can accept either a number or "all" if all zeros should be replaced. Default is to replace the first zero only.
##' @param replnum Value to replace zeros. Default is .01
##' @return Dataframe (long form)
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @examples
##' ReplaceZeros(apt)
##' ## Replace all zeros with .01
##' ReplaceZeros(apt, nrepl = "all", replnum = .01)
##' @export
ReplaceZeros <- function(dat, nrepl = 1, replnum = .01) {

    ## Get N unique participants, informing loop
    participants <- unique(dat$id)
    np <- length(participants)

    for (i in seq_len(np)) {

        adf <- NULL
        adf <- dat[dat$id == participants[i], ]

        if (!any(adf$y == 0)) {
            dat[dat$id == participants[i], ] <- adf
            next
        } else {
            if (nrepl == "all") {
                ind <- which(adf$y == 0, arr.ind = TRUE)
                adf$y[ind] <- replnum
                dat[dat$id == participants[i], ] <- adf
            } else {
                ind <- which(adf$y == 0, arr.ind = TRUE)
                nrep <- if (length(ind) < nrepl) length(ind) else nrepl
                adf$y[ind[1:nrep]] <- replnum
                dat[dat$id == participants[i], ] <- adf
            }
        }
    }
    dat
}



##' Changes 0 values or applies a parallel shift.
##'
##' Takes a matrix of consumption values and replaces specified number of 0s with replacement value. Optionally, can apply a parallel shift to all consumption values and prices.
##' @title Change 0s
##' @param mat A matrix of consumption values.
##' @param repl The number of 0s to be replaced by replnum. Default is NULL (i.e., all 0s).
##' @param replnum Replacement number. Default value is 0.01.
##' @param parshift Whether to apply a parallel shift of replnum to all consumption and prices.
##' @param prices If parshift == TRUE, value is required.
##' @return Matrix of transformed data. If parshift == TRUE, list of 2. First element is the transformed matrix. Second element is transformed price vector.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
change0 <- function(mat, repl = NULL, replnum = .01, parshift = FALSE, prices = NULL) {
    f <- function(x, repl = NULL, replnum = .01) {
        ind <- which(x == 0, arr.ind = TRUE)
        if (is.null(repl)) {
            x[ind] <- replnum
        } else {
            x[ind[1:repl]] <- replnum
        }
        x
    }
    if (!parshift ) {
        mat <- apply(mat, 2, f, repl = repl, replnum = replnum)
        attr(mat, "parshift") <- parshift
        attr(mat, "replnum") <- replnum
        return(mat)
    } else {
        if (parshift && is.null(prices)) stop("Trying to apply a parallel shift but missing a vector of prices")
        mat <- mat + replnum
        attr(mat, "parshift") <- parshift
        attr(mat, "replnum") <- replnum
        prices <- prices + replnum
        return(list(mat, prices))
    }
}

##' Splits a matrix of purchase task data into individual dataframes consisting of price, consumption, and expenditure.
##'
##' This function takes a matrix and splits it into a list of dataframes. Each dataframe consists of prices (x), consumption values (y), and expenditure values (expend). If a participant has no consumption, the dataframe will instead be a character vector indicating "No consumption." If a participant has fewer than 3 datapoints, the dataframe will instead be a character vector indicating "Fewer than 3 datapoints." Note that these conditions are more likely to be met when 0s are deleted.
##' @title Split Matrix
##' @param mat A matrix in wide form where each column is a participant's responses. If a dataframe is provided, it will attempt to coerce it into a matrix.
##' @param x A vector of prices used in the purchase task. The number of elements must be equal to the number of rows in the matrix.
##' @param incl0 If FALSE, ALL 0 values and their associated prices will be removed, even those that precede reverals from 0.
##' @param replfree Optionally replaces price=0 with specified number.
##' @return A list of dataframes and/or character vectors whose length is equal to the number of columns (i.e., participants) in the original matrix. Prints the number of cases that contain no consumption and fewer than 3 datapoints.
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
splitMat <- function(mat, x = x, incl0 = TRUE, replfree = NULL) {
    fu <- function(y, x = x, incl0, replfree) {
        if (!incl0) {
            xremove <- which(y == 0)
            if (length(xremove) > 0) x <- x[-xremove]
            y <- y[ y > 0 ]
            ifelse(length(y) == 0, df <- "No consumption",
                   ifelse(length(y) <= 2, df <- "Fewer than 3 datapoints",
                          df <- data.frame("x" = x, "y" = y, "expend" = x * y)))
        } else {
            ifelse(length(y) == 0, df <- "No consumption",
                   ifelse(length(y) <= 2, df <- "Fewer than 3 datapoints",
                          df <- data.frame("x" = x, "y" = y, "expend" = x * y)))
        }
        ## CHANGE? because this occurs after the df is made, expend doesn't take into account replfree
        if (is.numeric(replfree)) {
            if (df$x[1] == 0) {
                df$x[1] <- replfree
            } else {
                warning(paste0("Tried to replace free with ", replfree, " but the first price was not 0"))
            }
        }
        return(df)
    }
    if (is.data.frame(mat)) mat <- as.matrix(mat)
    if (NCOL(mat) > 1) {
        dfs <- apply(mat, 2, fu, x = x, incl0, replfree)
        if (length(which(dfs == "No consumption")) > 1 || length(which(dfs == "Fewer than 3 datapoints"))) {
            cat(paste0("There are ", length(which(dfs == "No consumption")), " participant(s) who have no consumption. There are ", length(which(dfs == "Fewer than 3 datapoints")), " participant(s) who have fewer than 3 datapoints. Consider excluding these participant(s)."))
        }
    } else {
        dfs <- list(fu(mat, x = x, incl0, replfree))
    }
    ## TODO: add attribute for replfree
    return(dfs)
}
