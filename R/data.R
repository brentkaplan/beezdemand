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
## Data used in examples
##
##
##

#' Example alcohol purchase task data
#'
#' A dataset containing alcohol purchase task data for a small number of participants
#'
#' @format Long-form data.frame with columns: id, x, y. Participants were asked how many standard sized alcoholic beverages they would buy at various prices.
"apt"

#' Example Experimental Tobacco Marketplace data
#'
#' A dataset containing ETM data for a small number of participants
#'
#' @format Long-form data.frame with columns: id, x, y, target, group. Participants were asked how many cigarettes, e-cigarettes, combustible, and non-combustible products they would buy at various prices.
"etm"

#' Example cross‐price dataset
#'
#' A small illustrative dataset of price (x) and consumption (y) target
#' (target), and group (group).
#'
#' @format A data frame with N rows and columns:
#'   \describe{
#'     \item{id}{unique identifier}
#'     \item{x}{price of the alternative}
#'     \item{y}{consumption/demand}
#'     \item{target}{target (e.g., alone, own, alt)}
#'     \item{group}{e.g., drug or product}
#'   }
"cp"

#' Example nonhuman demand data with drug and dose
#'
#' Data from Ko et al. (2002)
#'
#' @format A tibble with 135 rows and 6 columns:
#'   \describe{
#'     \item{monkey}{unique identifier}
#'     \item{x}{fixed-ratio requirement}
#'     \item{y}{consumption}
#'     \item{y_ll4}{consumption transformed via ll4}
#'     \item{drug}{drug}
#'     \item{dose}{dose}
#'   }
"ko"
