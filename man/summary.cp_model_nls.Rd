% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp-methods.R
\name{summary.cp_model_nls}
\alias{summary.cp_model_nls}
\title{Summarize a Cross-Price Demand Model (Nonlinear)}
\usage{
\method{summary}{cp_model_nls}(object, inverse_fun = NULL, ...)
}
\arguments{
\item{object}{A cross-price model object from fit_cp_nls with return_all=TRUE.}

\item{inverse_fun}{Optional function to inverse-transform predictions (e.g., ll4_inv).}

\item{...}{Additional arguments (unused).}
}
\value{
A list containing model summary information.
}
\description{
Summarize a Cross-Price Demand Model (Nonlinear)
}
