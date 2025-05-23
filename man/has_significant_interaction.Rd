% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp-methods.R
\name{has_significant_interaction}
\alias{has_significant_interaction}
\title{Test for significant interaction in a cross-price demand model}
\usage{
has_significant_interaction(object, alpha = 0.05)
}
\arguments{
\item{object}{A cp_model_lmer object from fit_cp_linear}

\item{alpha}{Significance level for testing (default: 0.05)}
}
\value{
Logical indicating whether interaction is significant
}
\description{
Test for significant interaction in a cross-price demand model
}
\keyword{internal}
