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
##' @noRd
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

    dat <- if (dplyr::is.tbl(dat)) {
      print("Data casted as data.frame")
      dat <- as.data.frame(dat)
      } else {
        dat
      }

    dat[, xcol] <- if (!is.numeric(dat[, xcol])) as.numeric(dat[, xcol]) else dat[, xcol]
    dat[, ycol] <- if (!is.numeric(dat[, ycol])) as.numeric(dat[, ycol]) else dat[, ycol]

    if (any(is.na(dat[, ycol]))) {
      warning("NA values found in ", ycol, " column. Dropping NAs and continuing")
      dat <- dat[!is.na(dat[, ycol]), ]
    }

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

    return(dat)
}

##' Ben Bolker's port of Lambert W from GNU Scientific Library (GPLV3)
##'
##' Ben Bolker's port of Lambert W from GNU Scientific Library
##' @title Lambert W
##' @param z input value
##' @param b branch, set to principal by default
##' @param maxiter Halley iteration count
##' @param eps error precision
##' @param min.imag minimum for imaginary solution
##' @return numeric
##' @author Benjamin Bolker (port)
##' @export
lambertW = function(z,b=0,maxiter=10,eps=.Machine$double.eps,min.imag=1e-9) {
  if (any(round(Re(b)) != b))
    stop("branch number for W must be an integer")
  if (!is.complex(z) && any(z<0)) z=as.complex(z)
  ## series expansion about -1/e
  ##
  ## p = (1 - 2*abs(b)).*sqrt(2*e*z + 2);
  ## w = (11/72)*p;
  ## w = (w - 1/3).*p;
  ## w = (w + 1).*p - 1
  ##
  ## first-order version suffices:
  ##
  w = (1 - 2*abs(b))*sqrt(2*exp(1)*z + 2) - 1
  ## asymptotic expansion at 0 and Inf
  ##
  v = log(z + as.numeric(z==0 & b==0)) + 2*pi*b*1i;
  v = v - log(v + as.numeric(v==0))
  ## choose strategy for initial guess
  ##
  c = abs(z + exp(-1));
  c = (c > 1.45 - 1.1*abs(b));
  c = c | (b*Im(z) > 0) | (!Im(z) & (b == 1))
  w = (1 - c)*w + c*v
  ## Halley iteration
  ##
  for (n in 1:maxiter) {
    p = exp(w)
    t = w*p - z
    f = (w != -1)
    t = f*t/(p*(w + f) - 0.5*(w + 2.0)*t/(w + f))
    w = w - t
    if (abs(Re(t)) < (2.48*eps)*(1.0 + abs(Re(w)))
        && abs(Im(t)) < (2.48*eps)*(1.0 + abs(Im(w))))
      break
  }
  if (n==maxiter) warning(paste("iteration limit (",maxiter,
                                ") reached, result of W may be inaccurate",sep=""))
  if (all(Im(w)<min.imag)) w = as.numeric(w)
  return(w)
}
