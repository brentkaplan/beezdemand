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
## along with beezdemand.  If not, see <https://www.gnu.org/licenses/gpl-2.0.html>.
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

#' Full alcohol purchase task dataset
#'
#' A larger dataset containing alcohol purchase task data with demographic covariates.
#' Suitable for testing hurdle models and mixed-effects models with covariates.
#'
#' @format A data frame with 18,700 rows and 8 columns:
#'   \describe{
#'     \item{id}{Unique participant identifier (1-1100)}
#'     \item{gender}{Participant gender (Male/Female)}
#'     \item{age}{Participant age in years}
#'     \item{binges}{Number of binge drinking episodes}
#'     \item{totdrinks}{Total number of drinks consumed}
#'     \item{tothours}{Total hours spent drinking}
#'     \item{x}{Price point for the purchase task}
#'     \item{y}{Number of drinks participant would purchase at price x}
#'   }
#'
#' @examples
#' \donttest{
#' data(apt_full)
#' # Use a subset for quick demonstration
#' apt_sub <- apt_full[apt_full$id %in% unique(apt_full$id)[1:20], ]
#' fit <- fit_demand_hurdle(apt_sub, y_var = "y", x_var = "x", id_var = "id")
#' summary(fit)
#' }
#'
#' @keywords datasets
"apt_full"

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

#' Low-nicotine cigarette purchase task
#'
#' Long-form purchase task data across nicotine conditions.
#'
#' @format A data frame with 5 columns: id, condition, x, y, commodity.
#' @keywords datasets
#' @examples
#' # data(lowNicClean)
"lowNicClean"

#' Cannabis/cigarette cross-price responses
#'
#' Cross-price style data with cannabis and cigarette context.
#'
#' @format A data frame with columns including: id, x, y, commodity, and auxiliary fields (Q1035, CigPrice, CanPrice, variable, value).
#' @keywords datasets
#' @examples
#' # data(cannabisCigarettes)
"cannabisCigarettes"

#' Experimental Tobacco Marketplace (ETM) data
#'
#' ETM data across price points with product quantities.
#'
#' @format A data frame with 6 columns: id, x, AdjCig, FixCig, ECig, flavor.
#' @keywords datasets
#' @examples
#' # data(ongoingETM)
"ongoingETM"
