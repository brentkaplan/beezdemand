% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{validate_cp_data}
\alias{validate_cp_data}
\title{Validate and Filter Cross-Price Demand Data}
\usage{
validate_cp_data(
  data,
  required_cols = c("x", "y"),
  filter_target = TRUE,
  require_id = FALSE
)
}
\arguments{
\item{data}{A data frame containing cross-price demand data.}

\item{required_cols}{Character vector of required column names.
Default is c("x", "y").}

\item{filter_target}{Logical; if TRUE and data contains a "target" column,
filters to keep only rows where target == "alt". Default is TRUE.}

\item{require_id}{Logical; if TRUE, validates that an "id" column exists
in the data. Default is FALSE.}
}
\value{
A validated (and potentially filtered) data frame.
}
\description{
This function performs validation checks on cross-price demand data and applies
filtering if specified. It ensures the data meets the requirements for analysis
by checking for required columns, filtering by target type if needed, and
confirming ID column presence when required.
}
\examples{
\dontrun{
# Basic validation requiring x and y columns
validated_data <- validate_cp_data(my_data)

# Require additional columns
validated_data <- validate_cp_data(my_data,
                                  required_cols = c("x", "y", "price"))

# Require ID column and don't filter by target
validated_data <- validate_cp_data(my_data,
                                  filter_target = FALSE,
                                  require_id = TRUE)
}

}
\keyword{internal}
