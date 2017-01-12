#' @title Peano kernel
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description The Peano kernel is the term inside the integral remainder in
#' the Taylor formula.
#'
#' @param x The interpolation variable.
#' @param t The integration variable.
#'
#' @return The value of the kernel.
#'
#' @rdname kernel
#' @name kernel
#' @aliases kernel deriv_kernel

kernel <- function(x, t) {
    return(pmax((x - t)^2, 0))
}

deriv_kernel <- function(x, t) {
    return(pmax(2*(x - t), 0))
}

#' @title Parameters of the spline interpolating the Peano kernel
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Parameters of the spline interpolating the Peano kernel.
#'
#' @param xk The position of the interpolation points.
#' @param t The integration variable.
#'
#' @return A list with the following components:
#' \itemize{
#'     \item{\code{xk}} The position of the interpolation points.
#'     \item{\code{yk}} The value of the spline at the interpolation points.
#'     \item{\code{sk}} The value of the derivative of the spline at the interpolation points.
#'     \item{\code{ak}} The value of the second derivative of the spline at the interpolation points.
#' }

kernel_interpolation_param <- function(xk, t) {
    # Value of the kernel at the interpolation points
    yk <- kernel(xk, t)
    # Value of the derivative of the kernel at the interpolation points
    sk <- deriv_kernel(xk, t)
    # Estimate the second derivative at the last point
    n <- length(xk)
    an <- (sk[n] - sk[n - 1])/(xk[n] - xk[n - 1])
    # Estimate the second derivative at the other points
    ak <- clamped_quintic_spline(xk, yk, sk, an)

    return(list(xk=xk, yk=yk, sk=sk, ak=ak))
}

#' @title Interpolation error on the kernel
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Interpolation error on the kernel and its derivative.
#'
#' @param xk The position of the interpolation points.
#' @param x The point at which to give the error.
#' @param t The integration variable in the Peano kernel.
#'
#' @return The value of the interpolation error on the kernel.
#'
#' @rdname kernel_value_error
#' @name kernel_value_error
#' @aliases kernel_value_error kernel_deriv_error

kernel_value_error <- function(xk, x, t) {
    param <- kernel_interpolation_param(xk, t)
    return(quintic_spline(x, param$xk, param$yk, param$sk, param$ak) - kernel(x, t))
}

kernel_deriv_error <- function(xk, x, t) {
    param <- kernel_interpolation_param(xk, t)
    return(deriv_quintic_spline(x, param$xk, param$yk, param$sk, param$ak) - deriv_kernel(x, t))
}

#' @title Bound on the interpolation error
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Give an absolute bound on the error of the interpolation and
#' its derivative, given the supremum of the third derivative of the
#' interpolated function.
#'
#' @param x A vector of points at which to estimate the error.
#' @param xk The position of the interpolation points.
#' @param norm_deriv3 The infinite norm of the third derivative of the
#' interpolated function.
#'
#' @return The bound for each value of \code{x}.

interpolation_value_error <- function(x, xk, norm_deriv3) {
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)

    return(sapply(seq_along(x), function(i) {
        f <- Vectorize(function(u) abs(kernel_value_error(xk, x[i], u)*norm_deriv3/2))
        return(integrate(f, lower=xk[k[i]], upper=xk[k[i] + 1])$value)
    }))
}

interpolation_deriv_error <- function(x, xk, norm_deriv3) {
    k <- cut(x, breaks=xk, include.lowest=TRUE, labels=FALSE)

    return(sapply(seq_along(x), function(i) {
        f <- Vectorize(function(u) abs(kernel_deriv_error(xk, x[i], u)*norm_deriv3/2))
        return(integrate(f, lower=xk[k[i]], upper=xk[k[i] + 1])$value)
    }))
}
