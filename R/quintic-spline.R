#' @title Basis functions for the quintic spline
#'
#' @author Thomas Blanchet
#'
#' @description Basis functions and their derivatives for the quintic Hermite
#' spline.
#'
#' @param x A value in [0, 1], at which the functions are evaluated.
#'
#' @return The value of the function.
#'
#' @rdname basis_functions
#' @name basis_functions
#' @aliases h00 h01 h10 h11 h20 h21 h00d1 h01d1 h10d1 h11d1 h20d1 h21d1
#' h00d2 h01d2 h10d2 h11d2 h20d2 h21d2

# Basis functions
h00 <- function(x) {
    return(1 - 10*x^3 + 15*x^4 - 6*x^5)
}
h01 <- function(x) {
    return(10*x^3 - 15*x^4 + 6*x^5)
}
h10 <- function(x) {
    return(x - 6*x^3 + 8*x^4 - 3*x^5)
}
h11 <- function(x) {
    return(-4*x^3 + 7*x^4 - 3*x^5)
}
h20 <- function(x) {
    return(x^2/2 - (3*x^3)/2 + (3*x^4)/2 - x^5/2)
}
h21 <- function(x) {
    return(x^3/2 - x^4 + x^5/2)
}

# First derivatives
h00d1 <- function(x) {
    return(-30*x^2 + 60*x^3 - 30*x^4)
}
h01d1 <- function(x) {
    return(30*x^2 - 60*x^3 + 30*x^4)
}
h10d1 <- function(x) {
    return(1 - 18*x^2 + 32*x^3 - 15*x^4)
}
h11d1 <- function(x) {
    return(-12*x^2 + 28*x^3 - 15*x^4)
}
h20d1 <- function(x) {
    return(x - (9*x^2)/2 + 6*x^3 - (5*x^4)/2)
}
h21d1 <- function(x) {
    return((3*x^2)/2 - 4*x^3 + (5*x^4)/2)
}

# Second derivatives
h00d2 <- function(x) {
    return(-60*x + 180*x^2 - 120*x^3)
}
h01d2 <- function(x) {
    return(60*x - 180*x^2 + 120*x^3)
}
h10d2 <- function(x) {
    return(-36*x + 96*x^2 - 60*x^3)
}
h11d2 <- function(x) {
    return(-24*x + 84*x^2 - 60*x^3)
}
h20d2 <- function(x) {
    return(1 - 9*x + 18*x^2 - 10*x^3)
}
h21d2 <- function(x) {
    return(3*x - 12*x^2 + 10*x^3)
}

#' @title Quintic spline function
#'
#' @author Thomas Blanchet
#'
#' @description Quintic spline over an interval \code{[x0, x1]}. It is such that:
#' \itemize{
#'     \item{\code{h(x0) == y0}}
#'     \item{\code{h(x1) == y1}}
#'     \item{\code{h’(x0) == s0}}
#'     \item{\code{h’(x1) == s1}}
#'     \item{\code{h’’(x0) == a0}}
#'     \item{\code{h’’(x1) == a1}}
#' }
#'
#' @param x A number between \code{x0} and \code{x1}, at which the spline is
#' evaluated.
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0}.
#' @param y1 Value of the spline at \code{x1}.
#' @param s0 Slope of the spline at \code{x0}.
#' @param s1 Slope of the spline at \code{x1}.
#' @param a0 Second derivative of the spline at \code{x0}.
#' @param a1 Second derivative of the spline at \code{x1}.
#'
#' @return The value of the interpolation at \code{x}.

h <- function(x, x0, x1, y0, y1, s0, s1, a0, a1) {
    # Normalize x in the [0, 1] interval
    x <- (x - x0)/(x1 - x0)
    return(
        y0*h00(x) +
        y1*h01(x) +
        s0*(x1 - x0)*h10(x) +
        s1*(x1 - x0)*h11(x) +
        a0*(x1 - x0)^2*h20(x) +
        a1*(x1 - x0)^2*h21(x)
    )
}

#' @title First derivative of the quintic spline function
#'
#' @author Thomas Blanchet
#'
#' @description First derivative of the quintic spline over an interval
#' \code{[x0, x1]}.
#'
#' @param x A number between \code{x0} and \code{x1}, at which the function is
#' evaluated.
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0}.
#' @param y1 Value of the spline at \code{x1}.
#' @param s0 Slope of the spline at \code{x0}.
#' @param s1 Slope of the spline at \code{x1}.
#' @param a0 Second derivative of the spline at \code{x0}.
#' @param a1 Second derivative of the spline at \code{x1}.
#'
#' @return The value of first derivative of the interpolation at \code{x}.

hd1 <- function(x, x0, x1, y0, y1, s0, s1, a0, a1) {
    # Normalize x in the [0, 1] interval
    x <- (x - x0)/(x1 - x0)
    return(
        y0*h00d1(x)/(x1 - x0) +
        y1*h01d1(x)/(x1 - x0) +
        s0*h10d1(x) +
        s1*h11d1(x) +
        a0*(x1 - x0)*h20d1(x) +
        a1*(x1 - x0)*h21d1(x)
    )
}

#' @title Second derivative of the quintic spline function
#'
#' @author Thomas Blanchet
#'
#' @description Second derivative of the quintic spline over an interval
#' \code{[x0, x1]}.
#'
#' @param x A number between \code{x0} and \code{x1}, at which the function is
#' evaluated.
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0}.
#' @param y1 Value of the spline at \code{x1}.
#' @param s0 Slope of the spline at \code{x0}.
#' @param s1 Slope of the spline at \code{x1}.
#' @param a0 Second derivative of the spline at \code{x0}.
#' @param a1 Second derivative of the spline at \code{x1}.
#'
#' @return The value of first second of the interpolation at \code{x}.

hd2 <- function(x, x0, x1, y0, y1, s0, s1, a0, a1) {
    # Normalize x in the [0, 1] interval
    x <- (x - x0)/(x1 - x0)
    return(
        y0*h00d2(x)/(x1 - x0)^2 +
        y1*h01d2(x)/(x1 - x0)^2 +
        s0*h10d2(x)/(x1 - x0) +
        s1*h11d2(x)/(x1 - x0) +
        a0*h20d2(x) +
        a1*h21d2(x)
    )
}

#' @title Quintic spline function
#'
#' @author Thomas Blanchet
#'
#' @description Quintic spline interpolation function over several intervals.
#'
#' @param x A vector of values between \code{min(xk)} and \code{max(xk)},
#' at which the interpolation function is evaluated.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#'
#' @return The values of the interpolation function at each point \code{x}.

quintic_spline <- function(x, xk, yk, sk, ak) {
    # Find the interval in which each value of x falls
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)
    return(h(x,
        xk[k], xk[k + 1],
        yk[k], yk[k + 1],
        sk[k], sk[k + 1],
        ak[k], ak[k + 1]
    ))
}

#' @title First derivative of the quintic spline function
#'
#' @author Thomas Blanchet
#'
#' @description First derivative of the quintic spline interpolation function
#' over several intervals.
#'
#' @param x A vector of values between \code{min(xk)} and \code{max(xk)},
#' at which the function is evaluated.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#'
#' @return The values of the first derivative of the interpolation function at
#' each point \code{x}.

deriv_quintic_spline <- function(x, xk, yk, sk, ak) {
    # Find the interval in which each value of x falls
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)
    return(hd1(x,
        xk[k], xk[k + 1],
        yk[k], yk[k + 1],
        sk[k], sk[k + 1],
        ak[k], ak[k + 1]
    ))
}

#' @title Second derivative of the quintic spline function
#'
#' @author Thomas Blanchet
#'
#' @description Second derivative of the quintic spline interpolation function
#' over several intervals.
#'
#' @param x A vector of values between \code{min(xk)} and \code{max(xk)},
#' at which the function is evaluated.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#'
#' @return The values of the first derivative of the interpolation function at
#' each point \code{x}.

deriv2_quintic_spline <- function(x, xk, yk, sk, ak) {
    # Find the interval in which each value of x falls
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)
    return(hd2(x,
        xk[k], xk[k + 1],
        yk[k], yk[k + 1],
        sk[k], sk[k + 1],
        ak[k], ak[k + 1]
    ))
}

