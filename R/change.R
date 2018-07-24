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
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples
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

##' Recodes outliers
##'
##' Recodes outliers using Tabachnick and Fidell's (2013) criteria. A variable is standardized and values that are greater/less than or equal to a specified outlier value (specified in standard deviations; default 3.29SD) are recoded to a certain number of units (default 0) higher/lower than the greatest nonoutlier value. Disregards 'NA' values.
##' @title Recode Outliers
##' @param df A dataframe of consumption values
##' @param outval Values greater/less than or equal to this number (specified in standard deviations) will be recoded. Default is 3.29SD as specified by Tabachnick and Fidell (2013)
##' @param unitshigher Outliers identified by outval will be coded to a certain number of units higher/lower than the greatest nonoutlier value. Default is 0 units.
##' @return Invisibly, a dataframe with original and recoded (if any) values
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples 
##' ## If any outliers are detected, they would be coded as 1 unit higher
##' \donttest{
##' emp <- GetEmpirical(apt)
##' RecodeOutliers(emp[, c(2:6)], unitshigher = 1)
##' }
##' @export
RecodeOutliers <- function(df, outval = 3.29, unitshigher = 0) {

    outval <- abs(outval)
    unitshigher <- abs(unitshigher)

    dfout <- data.frame(matrix(vector(),
                               nrow(df), length(colnames(df))), stringsAsFactors = FALSE)
    colnames(dfout) <- colnames(df)
    rownames(dfout) <- rownames(df)
    totoutliers <- 0
    for (i in colnames(df)) {
        x <- df[, i]
        ztmp <- scale(df[, i])
        if (!any(ztmp >= outval | ztmp <= -outval, na.rm = TRUE)) {
            print(paste0("No outliers detected in column: ", i, "."))
            dfout[, i] <- x
        } else {
            if (any(ztmp >= outval, na.rm = TRUE)) {
                replind <- which(ztmp >= outval)
                replval <- x[which(ztmp == max(ztmp[ztmp < outval], na.rm = TRUE))][[1]] + unitshigher
                print(paste0(length(replind), " outliers greater than ", outval, "SDs detected in: ", i, "."))
                totoutliers <- totoutliers + length(replind)
                for (j in seq(length(replind))) {
                    print(paste0("Recoding ", x[replind[j]], " with ", replval, "."))
                    x[replind[j]] <- replval
                }
            }
            if (any(ztmp <= -outval, na.rm = TRUE)) {
                replind <- which(ztmp <= -outval)
                replval <- x[which(ztmp == min(ztmp[ztmp > -outval], na.rm = TRUE))][[1]] - unitshigher
                print(paste0(length(replind), " outliers less than ", outval, "SDs detected in: ", i, "."))
                totoutliers <- totoutliers + length(replind)
                for (j in seq(length(replind))) {
                    print(paste0("Recoding ", x[replind[j]], " with ", replval, "."))
                    x[replind[j]] <- replval
                }
            }
           dfout[, i] <- x
        }
    }
    print(paste0("A total of ", totoutliers, " outlying values were replaced"))
    invisible(dfout)
}

##' Changes demand data
##'
##' Change demand data in various ways. Ways include replacing any number of 0 values with a replacement number (or remove them completely), removing price and consumption at free, replacing free with some number. This will soon replace ReplaceZeros and certain arguments in FitCurves.
##' @title ChangeData
##' @param dat A long form dataframe
##' @param nrepl Number of zeros to replace with replacement value (replnum). Can accept either a number or "all" if all zeros should be replaced. Default is to replace the first zero only
##' @param replnum Value to replace zeros. Default is .01
##' @param rem0 If TRUE, removes all 0s in consumption data prior to analysis. Default value is FALSE
##' @param remq0e If TRUE, removes consumption and price where price == 0. Default value is FALSE
##' @param replfree Optionally replaces price == 0 with specified value.
##' @param xcol Column name in dataframe that signifies x values (usually price or the IV)
##' @param ycol Column name in dataframe that signifies y values (usually consumption or the DV)
##' @param idcol Column name in dataframe that signifies identifying id grouping
##' @return Long form dataframe resembling the originally provided dataframe
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples 
##' ## Change just the first instance of 0 within each unique value of id with .1
##' ChangeData(apt, nrepl = 1, replnum = .1)
##' @export
ChangeData <- function(dat, nrepl = 1, replnum = .01, rem0 = FALSE, remq0e = FALSE, replfree = NULL, xcol = "x", ycol = "y", idcol = "id") {
    origcols <- colnames(dat)
    if (any(colnames(dat) %in% "x") && any(colnames(dat) %in% "y") && any(colnames(dat) %in% "id")) {

    } else if (any(colnames(dat) %in% xcol) && any(colnames(dat) %in% ycol) && any(colnames(dat) %in% idcol)) {

        if (!any(colnames(dat) %in% "x") && any(colnames(dat) %in% xcol)) {
            colnames(dat) <- gsub(xcol, "x", colnames(dat))
        }
        if (!any(colnames(dat) %in% "y") && any(colnames(dat) %in% ycol)) {
            colnames(dat) <- gsub(ycol, "y", colnames(dat))
        }
        if (!any(colnames(dat) %in% "id") && any(colnames(dat) %in% idcol)) {
            colnames(dat) <- gsub(idcol, "id", colnames(dat))
        }
    } else {
        stop("Can't find x, y, and id column names in data!", call. = FALSE)
    }

    ids <- unique(dat$id)
    nids <- length(ids)
    newdat <- vector(mode = "list", length = nids)
    for (i in seq_len(nids)) {

        adf <- NULL
        adf <- dat[dat$id == ids[i], ]

        if (remq0e && is.null(replfree)) {
            adf <- adf[adf$x != 0, , drop = FALSE]
        } else if (remq0e && !is.null(replfree)) {
            warning("You asked to remove q0e and replace this value. I will only replace it")
            replfree <- if (is.numeric(replfree)) replfree else 0.01
            adf[adf$x == 0, "x"] <- replfree
        }

        if (!any(adf$y == 0)) {
            newdat[[i]] <- adf
            next
        } else if (rem0) {
            adf <- adf[adf$y != 0, ]
            newdat[[i]] <- adf
        } else {
            if (nrepl == "all") {
                ind <- which(adf$y == 0, arr.ind = TRUE)
                adf$y[ind] <- replnum
                newdat[[i]] <- adf
            } else {
                ind <- which(adf$y == 0, arr.ind = TRUE)
                nrep <- if (length(ind) < nrepl) length(ind) else nrepl
                adf$y[ind[1:nrep]] <- replnum
                newdat[[i]] <- adf
            }
        }
    }
    dat <- do.call(rbind, newdat)
    colnames(dat) <- origcols
    dat
}
