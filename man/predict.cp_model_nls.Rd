% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp-methods.R
\name{predict.cp_model_nls}
\alias{predict.cp_model_nls}
\title{Predict from a Cross-Price Demand Model (Nonlinear)}
\usage{
\method{predict}{cp_model_nls}(object, newdata = NULL, inverse_fun = NULL, ...)
}
\arguments{
\item{object}{A cross-price model object from fit_cp_nls with return_all=TRUE.}

\item{newdata}{A data frame containing an 'x' column.}

\item{inverse_fun}{Optional inverse transformation function.}

\item{...}{Additional arguments.}
}
\value{
A data frame with x values and predicted y values.
}
\description{
Predict from a Cross-Price Demand Model (Nonlinear)
}
