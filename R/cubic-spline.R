#' @title Basis functions for the cubic spline
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Basis functions and their derivatives for the cubic Hermite
#' spline.
#'
#' @param x A value in [0, 1], at which the functions are evaluated.
#'
#' @return The value of the function.
#'
#' @rdname basis_functions_cubic_spline
#' @name basis_functions_cubic_spline
#' @aliases g00 g01 g10 g11 g20 g21 g00d1 g01d1 g10d1 g11d1 g20d1 g21d1

# Basis functions
g00 <- function(x) {
    return(1 - 3*x^2 + 2*x^3)
}
g01 <- function(x) {
    return(3*x^2 - 2*x^3)
}
g10 <- function(x) {
    return(x - 2*x^2 + x^3)
}
g11 <- function(x) {
    return(-x^2 + x^3)
}

# First derivatives
g00d1 <- function(x) {
    return(-6*x + 6*x^2)
}
g01d1 <- function(x) {
    return(6*x - 6*x^2)
}
g10d1 <- function(x) {
    return(1 - 4*x + 3*x^2)
}
g11d1 <- function(x) {
    return(-2*x + 3*x^2)
}

#' @title Cubic spline function
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Cubic spline over an interval \code{[x0, x1]}. It is such that:
#' \itemize{
#'     \item{\code{g(x0) == y0}}
#'     \item{\code{g(x1) == y1}}
#'     \item{\code{g’(x0) == s0}}
#'     \item{\code{g’(x1) == s1}}
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
#'
#' @return The value of the interpolation at \code{x}.

g <- function(x, x0, x1, y0, y1, s0, s1) {
    # Normalize x in the [0, 1] interval
    x <- (x - x0)/(x1 - x0)
    return(
        y0*g00(x) +
        y1*g01(x) +
        s0*(x1 - x0)*g10(x) +
        s1*(x1 - x0)*g11(x)
    )
}

#' @title First derivative of the cubic spline function
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description First derivative of the cubic spline over an interval
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
#'
#' @return The value of first derivative of the interpolation at \code{x}.

gd1 <- function(x, x0, x1, y0, y1, s0, s1) {
    # Normalize x in the [0, 1] interval
    x <- (x - x0)/(x1 - x0)
    return(
        y0*g00d1(x)/(x1 - x0) +
        y1*g01d1(x)/(x1 - x0) +
        s0*g10d1(x) +
        s1*g11d1(x)
    )
}

#' @title Cubic spline function
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Cubic spline interpolation function over several intervals.
#'
#' @param x A vector of values between \code{min(xk)} and \code{max(xk)},
#' at which the interpolation function is evaluated.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#'
#' @return The values of the interpolation function at each point \code{x}.

cubic_spline <- function(x, xk, yk, sk) {
    # Find the interval in which each value of x falls
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)
    return(g(x,
        xk[k], xk[k + 1],
        yk[k], yk[k + 1],
        sk[k], sk[k + 1]
    ))
}

#' @title First derivative of the cubic spline function
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description First derivative of the quintic spline interpolation function
#' over several intervals.
#'
#' @param x A vector of values between \code{min(xk)} and \code{max(xk)},
#' at which the function is evaluated.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#'
#' @return The values of the first derivative of the interpolation function at
#' each point \code{x}.

deriv_cubic_spline <- function(x, xk, yk, sk) {
    # Find the interval in which each value of x falls
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)
    return(gd1(x,
        xk[k], xk[k + 1],
        yk[k], yk[k + 1],
        sk[k], sk[k + 1]
    ))
}


#' @title Estimate a natural cubic spline with first derivative clamped at the last point
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Estimate a natural cubic spline from known values of the
#' function: that is, estimate the first derivative of the spline at each knot
#' to ensure a continuous second derivative, a zero second derivative
#' at the first point, and a given first derivative at the last point.
#'
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sn Derivative at the last knot.
#'
#' @return The vector \code{sk} of first derivatives at each knot.

clamped_cubic_spline <- function(xk, yk, sn) {
    n <- length(xk)

    x1 <- xk[1:(n - 2)]
    x2 <- xk[2:(n - 1)]
    x3 <- xk[3:(n - 0)]

    y1 <- yk[1:(n - 2)]
    y2 <- yk[2:(n - 1)]
    y3 <- yk[3:(n - 0)]

    # Tridiagonal matrix representing the system of equations
    A <- matrix(data=0, nrow=n, ncol=n)
    A[cbind(1:(n - 1), 2:n)] <- c(
        -2/(xk[2] - xk[1]),
        2/(x3 - x2)
    )
    A[cbind(1:n, 1:n)] <- c(
        -4/(xk[2] - xk[1]),
        4/(x2 - x1) + 4/(x3 - x2),
        1
    )
    A[cbind(2:n, 1:(n - 1))] <- c(
        2/(x2 - x1),
        0
    )

    b <- c(
        -6*(yk[2] - yk[1])/(xk[2] - xk[1])^2,
        6*(y2 - y1)/(x2 - x1)^2 + 6*(y3 - y2)/(x3 - x2)^2,
        sn
    )

    # Solve and return solution
    return(solve(A, b))
}
