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
## R script for utility functions
##
##createOutDir derived from
## @kutils = functions used in the CRMDA (Copyright - 2016 - Paul Johnson - GPLv2)
##
##
##

##' Manufacture an output directory that is unique
##'
##' Creates out directory and if proposed new directory already
##' exists, then a new name is proposed, and checked, until a unique
##' new directory name is found.
##'
##' A more detailed description here.
##' @param basedir Where to place the output file? (slash on end needed)
##' @param basename Proposed name of output directory (slash on end needed)
##' @param timestring Leave as default, will use MMDD to give unique directory name.
##' @return A string with a directory name
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
createOutdir <- function(basedir = "../", basename = "/output/",
                         timestring = format(Sys.time(), "%m%d"))
{
    outdir <- paste0(basedir, basename, timestring)
    j <- 1
    outcandidate <- outdir
    while(file.exists(outcandidate)){
        j <- j + 1
        outcandidate <- paste0(outdir, "-", j)
        outcandidate <- gsub("//", "/", outcandidate)
    }
    outdir <- paste0(outcandidate, "/")
    if(!file.exists(outdir)) res <- dir.create(outdir, recursive = TRUE)
    if (!res) stop("output directory could not be created")
    list(outdir, basename)
}

##' Pull vector from data frame
##'
##' Pulls a single vector from a data frame. Good to use with dplyr.
##' From http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
##' @title Pull
##' @param x A data frame
##' @param y Name of column
##' @return Vector
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
pull <- function(x, y) {
    if (ncol(x) == 1) y <- 1 else y
    x[ , if (is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
}

# Trim Leading Characters
# x A string
trim.leading <- function (x)  sub("^\\s+", "", x)

##' Checks to ensure column names are specified
##'
##' Check column names
##' @title Check Column Names
##' @param dat Dataframe
##' @param xcol Name of x column
##' @param ycol Name of y column
##' @param idcol Name of id column
##' @param groupcol Name of group column
##' @return Dataframe
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @export
CheckCols <- function(dat, xcol, ycol, idcol, groupcol = NULL) {
  
    dat <- if (is.tbl(dat)) as.data.frame(dat) else dat
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

    if (!is.null(groupcol) && any(colnames(dat) %in% groupcol)) {
        colnames(dat) <- gsub(groupcol, "group", colnames(dat))
    } else if (!is.null(groupcol) && !any(colnames(dat) %in% "group")) {
        stop("Can't find groupcol column name in data!", call. = FALSE)
    } else if (!is.null(groupcol) && any(colnames(dat) %in% "group") && !any(colnames(dat) %in% groupcol)) {
        stop("Groupcol does not match column names. Column name 'group' was found and will be used.", call. = FALSE)
    }
  
    dat[, xcol] <- if (!is.numeric(dat[, xcol])) as.numeric(dat[, xcol]) else dat[, xcol]
    dat[, ycol] <- if (!is.numeric(dat[, ycol])) as.numeric(dat[, ycol]) else dat[, ycol]
    
    if (any(is.na(dat[, ycol]))) {
      warning("NA values found in ", ycol, " column. Dropping NAs and continuing")
      dat <- dat[!is.na(dat[, ycol]), ]
    }
    
    return(dat)
}

