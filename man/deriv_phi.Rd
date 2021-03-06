% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distribution-functions.R
\name{deriv_phi}
\alias{deriv_phi}
\title{Derivative of the interpolation function from generalized Pareto
interpolation}
\usage{
deriv_phi(dist, x, ...)
}
\arguments{
\item{dist}{A \code{gpinter_dist_orig} object, as returned by
\code{tabulation_fit} or \code{share_fit}.}

\item{x}{The function evaluation point(s).}

\item{...}{Ignored.}
}
\value{
The value of the derivative of the interpolation at \code{x}.
}
\description{
This function is the first derivative of \code{phi} applied
to objects of class \code{gpinter_dist_orig}.
}
\author{
Thomas Blanchet, Juliette Fournier, Thomas Piketty
}
