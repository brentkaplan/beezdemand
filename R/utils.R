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
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
pull <- function(x, y) {
    if (ncol(x) == 1) y <- 1 else y
    x[ , if (is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
}

##' Trims leading characters
##'
##' Trims leading characters
##' @title
##' @param x A string
##' @return Character vector of length 1
##' @author Brent Kaplan <bkaplan4@@ku.edu>
##' @export
trim.leading <- function (x)  sub("^\\s+", "", x)
