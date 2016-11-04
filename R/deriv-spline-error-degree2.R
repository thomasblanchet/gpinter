#' @title Derivative of Peano kernel for the interpolation method in central
#' intervals with unknown derivatives
#'
#' @author Thomas Blanchet
#'
#' @description The Peano kernel correspond the spline interpolation error of
#' the function \eqn{max{(x - t), 0}^2}, which is the term inside the integral
#' of the Taylor remainder. This function returns the derivative of the Peano
#' kernel, which is used to get the interpolation error bound on the derivative
#' of the interpolation function.
#'
#' @param x A value between \code{x1} and \code{x2}.
#' @param t A value between \code{x1} and \code{x2} (integration variable).
#' @param x0 The first point of the grid.
#' @param x1 The second point of the grid.
#' @param x2 The third point of the grid.
#' @param x3 The fourth point of the grid.
#'
#' @return The value of the derivative if the Peano kernel.

deriv_central_peano_kernel_degree2 <- function(x, t, x0, x1, x2, x3) {
    # Parameters of the spline for the Peano kernel
    y0 <- 0
    y1 <- 0
    y2 <- (x2 - t)^2
    y3 <- (x3 - t)^2
    s1 <- central_derivative(x0, x1, x2, y0, y1, y2)
    s2 <- central_derivative(x1, x2, x3, y1, y2, y3)
    a1 <- second_derivative(x0, x1, x2, y0, y1, y2)
    a2 <- second_derivative(x1, x2, x3, y1, y2, y3)

    return(hd1(x, x1, x2, y1, y2, s1, s2, a1, a2) - 2*(x - t)*(x > t))
}

#' @title Derivative of the Peano kernel for the interpolation method in the
#' left interval with unknown derivatives
#'
#' @author Thomas Blanchet
#'
#' @description The Peano kernel correspond the spline interpolation error of
#' the function \eqn{max{(x - t), 0}^2}, which is the term inside the integral
#' of the Taylor remainder. This function estimates the derivative of the
#' kernel for the interpolation of the left interval, where the derivative
#' on the right is estimated from the three points to the right.
#'
#' @param x A value between \code{x0} and \code{x1}.
#' @param t A value between \code{x0} and \code{x1} (integration variable).
#' @param x0 The first point of the grid.
#' @param x1 The second point of the grid.
#' @param x2 The third point of the grid.
#'
#' @return The value of the derivative of the Peano kernel.

deriv_left_peano_kernel_degree2 <- function(x, t, x0, x1, x2) {
    # Parameters of the spline for the Peano kernel
    y0 <- 0
    y1 <- (x1 - t)^2
    y2 <- (x2 - t)^2
    s0 <- right_derivative(x0, x1, x2, y0, y1, y2)
    s1 <- central_derivative(x0, x1, x2, y0, y1, y2)
    a0 <- second_derivative(x0, x1, x2, y0, y1, y2)
    a1 <- second_derivative(x0, x1, x2, y0, y1, y2)

    return(hd1(x, x0, x1, y0, y1, s0, s1, a0, a1) - 2*(x - t)*(x > t))
}

#' @title Derivative of the Peano kernel for the interpolation method in the
#' right interval with unknown derivatives
#'
#' @author Thomas Blanchet
#'
#' @description The Peano kernel correspond the spline interpolation error of
#' the function \eqn{max{(x - t), 0}^2}, which is the term inside the integral
#' of the Taylor remainder. This function estimates the derivative of the
#' kernel for the interpolation of the right interval, where the last second
#' derivative is estimated from the three points to the left.
#'
#' @param x A value between \code{x1} and \code{x2}.
#' @param t A value between \code{x1} and \code{x2} (integration variable).
#' @param x0 The first point of the grid.
#' @param x1 The second point of the grid.
#' @param x2 The third point of the grid.
#'
#' @return The value of the derivative if the Peano kernel.

deriv_right_peano_kernel_degree2 <- function(x, t, x0, x1, x2) {
    # Parameters of the spline for the Peano kernel
    y0 <- 0
    y1 <- 0
    y2 <- (x2 - t)^2
    s1 <- central_derivative(x0, x1, x2, y0, y1, y2)
    s2 <- left_derivative(x0, x1, x2, y0, y1, y2)
    a1 <- second_derivative(x0, x1, x2, y0, y1, y2)
    a2 <- second_derivative(x0, x1, x2, y0, y1, y2)

    return(hd1(x, x1, x2, y1, y2, s1, s2, a1, a2) - 2*(x - t)*(x > t))
}

#' @title Interpolation error of the derivative for the central
#' intervals with unknown derivatives
#'
#' @author Thomas Blanchet
#'
#' @description Gives the bound on the interpolation error of the
#' derivative for central intervals.
#'
#' @param x A value between \code{x1} and \code{x2}.
#' @param x0 The first point of the grid.
#' @param x1 The second point of the grid.
#' @param x2 The third point of the grid.
#' @param x3 The fourth point of the grid.
#' @param upperbound A function that majorates the third derivative.
#'
#' @return The value of the bound at x.

deriv_central_spline_error_degree2 <- function(x, x0, x1, x2, x3, upperbound) {
    return(integrate(function(t) {
        abs(deriv_central_peano_kernel_degree2(x, t, x0, x1, x2, x3)*upperbound(t))/2
    }, lower=x1, upper=x2)$value)
}

#' @title Interpolation error of the derivative for the left interval with
#' known derivatives
#'
#' @author Thomas Blanchet
#'
#' @description Gives the bound on the interpolation error of the
#' derivative for the left interval.
#'
#' @param x A value between \code{x1} and \code{x2}.
#' @param x0 The first point of the grid.
#' @param x1 The second point of the grid.
#' @param x2 The third point of the grid.
#' @param upperbound A function that majorates the third derivative.
#'
#' @return The value of the bound at x.

deriv_left_spline_error_degree2 <- function(x, x0, x1, x2, upperbound) {
    return(integrate(function(t) {
        abs(deriv_left_peano_kernel_degree2(x, t, x0, x1, x2)*upperbound(t))/2
    }, lower=x0, upper=x1)$value)
}

#' @title Interpolation error of the derivative for the right interval with
#' known derivatives
#'
#' @author Thomas Blanchet
#'
#' @description Gives the bound on the interpolation error of the
#' derivative for the right interval.
#'
#' @param x A value between \code{x1} and \code{x2}.
#' @param x0 The first point of the grid.
#' @param x1 The second point of the grid.
#' @param x2 The third point of the grid.
#' @param upperbound A function that majorates the third derivative.
#'
#' @return The value of the bound at x.

deriv_right_spline_error_degree2 <- function(x, x0, x1, x2, upperbound) {
    return(integrate(function(t) {
        abs(deriv_right_peano_kernel_degree2(x, t, x0, x1, x2)*upperbound(t))/2
    }, lower=x1, upper=x2)$value)
}

#' @title Spline interpolation error of the derivative over the entire domain
#' with unknown derivatives
#'
#' @author Thomas Blanchet
#'
#' @description Gives the bound on the spline interpolation error
#' of the derivative for any point of the interpolation domain.
#'
#' @param x A value between \code{min(xk)} and \code{max(xk)}.
#' @param xk The grid points for the spline estimation.
#' @param upperbound A function that majorates the third derivative.
#'
#' @return The value of the bound at x.

deriv_spline_error_degree2 <- function(x, xk, upperbound) {
    n <- length(xk)
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)

    return(sapply(1:length(x), function(i) {
        if (is.na(k[i]) | k[i] == n) {
            return(NA)
        } else if (k[i] == 1) {
            return(deriv_left_spline_error_degree2(x[i], xk[1], xk[2], xk[3], upperbound))
        } else if (k[i] == (n - 1)) {
            return(deriv_right_spline_error_degree2(x[i], xk[n - 2], xk[n - 1], xk[n], upperbound))
        } else {
            return(deriv_central_spline_error_degree2(x[i], xk[k[i] - 1], xk[k[i]], xk[k[i] + 1], xk[k[i] + 2], upperbound))
        }
    }))
}
