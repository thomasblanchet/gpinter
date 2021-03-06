% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individualize.R
\name{individualize_dist}
\alias{individualize_dist}
\title{Individualize a distribution}
\usage{
individualize_dist(
  dist,
  p,
  singleshare = NULL,
  coupleshare = NULL,
  singletop = NULL,
  coupletop = NULL,
  singlebracket = NULL,
  couplebracket = NULL,
  ratio = NULL
)
}
\arguments{
\item{dist}{An object of class \code{gpinter_dist_orig}.}

\item{p}{A vector of fractiles in [0, 1].}

\item{singleshare}{The overall share of singles.}

\item{coupleshare}{The overall share of couples.}

\item{singletop}{A vector with the same length as \code{p}: the share
of singles in the top 100*(1 - p)\%.}

\item{coupletop}{A vector with the same length as \code{p}: the share
of couples in the top 100*(1 - p)\%.}

\item{singlebracket}{A vector with the same length as \code{p}: the share
of singles in the matching bracket.}

\item{couplebracket}{A vector with the same length as \code{p}: the share
of couples in the matching bracket.}

\item{ratio}{A vector with the same length as \code{p}: the ratio of
singles average income over couples average income in each bracket. Default
is 1 for all brackets.}
}
\value{
An object of class \code{gpinter_dist_indiv}.
}
\description{
Individualize the distribution (of income or wealth) under
equal splitting among spouses, given the share of couples or singles
at different points of the distribution.
}
\author{
Thomas Blanchet, Juliette Fournier, Thomas Piketty
}
