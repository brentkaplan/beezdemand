% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp-methods.R
\name{plot.cp_model_nls}
\alias{plot.cp_model_nls}
\title{Plot a Cross-Price Demand Model (Nonlinear)}
\usage{
\method{plot}{cp_model_nls}(
  x,
  data = NULL,
  inverse_fun = NULL,
  n_points = 100,
  title = NULL,
  xlab = "Price",
  ylab = "Consumption",
  x_trans = "identity",
  y_trans = "identity",
  point_size = 3,
  ...
)
}
\arguments{
\item{x}{A cross-price model object from fit_cp_nls with return_all=TRUE.}

\item{data}{Optional data frame with x and y; if NULL, uses object$data.}

\item{inverse_fun}{Optional function to inverse-transform predictions.}

\item{n_points}{Number of points used for prediction curve.}

\item{title}{Optional plot title.}

\item{xlab}{X-axis label.}

\item{ylab}{Y-axis label.}

\item{x_trans}{Transformation for x-axis: "identity", "log10", or "pseudo_log".}

\item{y_trans}{Transformation for y-axis: "identity", "log10", or "pseudo_log".}

\item{point_size}{Size of data points.}

\item{...}{Additional arguments (passed to predict).}
}
\value{
A ggplot2 object.
}
\description{
Plot a Cross-Price Demand Model (Nonlinear)
}
