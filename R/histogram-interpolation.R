#' @title Histogram interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description This function interpolates the distribution over a bracket
#' with an histogram (that is, piecewise constant density with two values,
#' and piecewise linear quantile function with two slopes).
#'
#' @param p0 A value in [0, 1].
#' @param p1 A value in [0, 1].
#' @param q0 The \code{p0}-th quantile.
#' @param q1 The \code{p1}-th quantile.
#' @param average The average value in the bracket.
#'
#' @return A list with the following components:
#' \describe{
#'     \item{average}{The average inside the bracket.}
#'     \item{p0}{A value in [0, 1].}
#'     \item{p1}{A value in [0, 1].}
#'     \item{q0}{The \code{p0}-th quantile.}
#'     \item{q1}{The \code{p1}-th quantile.}
#'     \item{pstar}{A value in [\code{p0}, \code{p1}].}
#'     \item{qstar}{A value in [\code{q0}, \code{q1}]: the \code{pstar}-th quantile.}
#'     \item{f0}{The value of the density in [\code{q0}, \code{qstar}].}
#'     \item{f1}{The value of the density in [\code{qstar}, \code{q1}].}
#' }

hist_interpol <- function(p0, p1, q0, q1, average) {
    return(list(
        average = average,
        p0 = p0,
        p1 = p1,
        q0 = q0,
        q1 = q1,
        pstar = p0 + (p1 - p0)*(q1 - average)/(q1 - q0),
        qstar = average,
        f0 = (p1 - p0)/(q1 - q0)*(q1 - average)/(average - q0),
        f1 = (p1 - p0)/(q1 - q0)*(average - q0)/(q1 - average)
    ))
}

#' @title Quantile function for histogram density
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculates the quantile function over an interval where the
#' distribution has a constant density.
#'
#' @param p The values at which to estimate the quantile function.
#' @param p0 The probability lower bound of the interval with constant density.
#' @param q0 The quantile lower bound of the interval with constant density.
#' @param f The value of the constant density.

hist_quantile <- function(p, p0, q0, f) {
    return(q0 + (p - p0)/f)
}

#' @title CDF for histogram density
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculates the CDF over an interval where the
#' distribution has a constant density.
#'
#' @param q The values at which to estimate the CDF.
#' @param p0 The probability lower bound of the interval with constant density.
#' @param q0 The quantile lower bound of the interval with constant density.
#' @param f The value of the constant density.

hist_cdf <- function(q, p0, q0, f) {
    return(p0 + f*(q - q0))
}

#' @title Non-normalized Lorenz curve for histogram density
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculates the non-normalized Lorenz curve (average times the
#' top share) for a distribution with constant density over an interval.
#'
#' @param p The values at which to estimate the Lorenz curve.
#' @param p0 The probability lower bound of the interval with constant density.
#' @param p1 The probability upper bound of the interval with constant density.
#' @param q0 The quantile lower bound of the interval with constant density.
#' @param q1 The quantile upper bound of the interval with constant density.
#' @param m1 The value of the Lorenz curve at \code{p1}.
#' @param f The value of the constant density.

hist_lorenz <- function(p, p0, p1, q0, q1, m1, f) {
    return(m1 + q0*(p1 - p) + 0.5*(p1 - p)*(p - p0 + p1 - p0)/f)
}

#' @title Pareto interpolation function for histogram density
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculates the Pareto interpolation function for a distribution
#' with constant density over an interval.
#'
#' @param x The values at which to estimate the interpolation function.
#' @param p0 The probability lower bound of the interval with constant density.
#' @param p1 The probability upper bound of the interval with constant density.
#' @param q0 The quantile lower bound of the interval with constant density.
#' @param q1 The quantile upper bound of the interval with constant density.
#' @param m1 The value of the Lorenz curve at \code{p1}.
#' @param f The value of the constant density.

hist_phi <- function(x, p0, p1, q0, q1, m1, f) {
    return(-log(hist_lorenz(1 - exp(-x), p0, p1, q0, q1, m1, f)))
}

#' @title Derivative of Pareto interpolation function for histogram density
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculates the derivative of the Pareto interpolation function
#' for a distribution with constant density over an interval.
#'
#' @param x The values at which to estimate the interpolation function.
#' @param p0 The probability lower bound of the interval with constant density.
#' @param p1 The probability upper bound of the interval with constant density.
#' @param q0 The quantile lower bound of the interval with constant density.
#' @param q1 The quantile upper bound of the interval with constant density.
#' @param m1 The value of the Lorenz curve at \code{p1}.
#' @param f The value of the constant density.

hist_deriv_phi <- function(x, p0, p1, q0, q1, m1, f) {
    p <- 1 - exp(-x)
    return(((1 - p)*hist_quantile(p, p0, q0, f))/hist_lorenz(p, p0, p1, q0, q1, m1, f))
}
