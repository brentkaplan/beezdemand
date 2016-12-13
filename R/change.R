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

##' Recodes outliers
##'
##' Recodes outliers using Tabachnick and Fidell's (2013) criteria. A variable is standardized and values that are greater/less than or equal to a specified outlier value (specified in standard deviations; default 3.29SD) are recoded to a certain number of units (default 0) higher/lower than the greatest nonoutlier value. Disregards 'NA' values.
##' @title Recode Outliers
##' @param df A dataframe of consumption values
##' @param outval Values greater/less than or equal to this number (specified in standard deviations) will be recoded. Default is 3.29SD as specified by Tabachnick and Fidell (2013)
##' @param unitshigher Outliers identified by outval will be coded to a certain number of units higher/lower than the greatest nonoutlier value. Default is 0 units.
##' @return Invisibly, a dataframe with original and recoded (if any) values
##' @author Brent Kaplan <bkaplan4@@ku.edu>
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
