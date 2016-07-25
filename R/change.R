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


