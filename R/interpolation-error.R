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
    y <- pmax(x - t, 0)^2
    return(y)
}

deriv_kernel <- function(x, t) {
    y <- 2*pmax(x - t, 0)
    return(y)
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

#' @title Bound on the interpolation error for a constant phi'''
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
#'
#' @export

interpolation_value_error_bound_cons <- function(x, xk, norm_deriv3) {
    return(sapply(x, function(x) {
        f <- Vectorize(function(u) abs(kernel_value_error(xk, x, u)*norm_deriv3/2))
        return(integrate(f, lower=min(xk), upper=max(xk))$value)
    }))
}
 #' @export
interpolation_deriv_error_bound_cons <- function(x, xk, norm_deriv3) {
    return(sapply(x, function(x) {
        f <- Vectorize(function(u) abs(kernel_deriv_error(xk, x, u)*norm_deriv3/2))
        return(integrate(f, lower=min(xk), upper=max(xk))$value)
    }))
}

#' @title Bound on the interpolation error for a non constant phi'''
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Give the value of the error for the interpolation and
#' its derivative, given the third derivative of the
#' interpolated function.
#'
#' @param x A vector of points at which to estimate the error.
#' @param xk The position of the interpolation points.
#' @param phid3 The third derivative of the interpolated function.
#'
#' @return The bound for each value of \code{x}.
#'
#' @export

interpolation_value_error_bound_noncons <- function(x, xk, phid3) {
    return(sapply(x, function(x) {
        f <- Vectorize(function(u) abs(kernel_value_error(xk, x, u)*phid3(u)/2))
        n <- length(xk)
        return(sum(sapply(1:(n - 1), function(i) {
            return(integrate(f, lower=xk[i], upper=xk[i + 1], stop.on.error=FALSE)$value)
        })))
    }))
}

#' @export
interpolation_deriv_error_bound_noncons <- function(x, xk, phid3) {
    return(sapply(x, function(x) {
        f <- Vectorize(function(u) abs(kernel_deriv_error(xk, x, u)*phid3(u)/2))
        n <- length(xk)
        return(sum(sapply(1:(n - 1), function(i) {
            return(integrate(f, lower=xk[i], upper=xk[i + 1], stop.on.error=FALSE)$value)
        })))
    }))
}

#' @title Value of the interpolation error for a non constant phi'''
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Give the value of the error for the interpolation and
#' its derivative, given the third derivative of the
#' interpolated function.
#'
#' @param x A vector of points at which to estimate the error.
#' @param xk The position of the interpolation points.
#' @param phid3 The third derivative of the interpolated function.
#'
#' @return The bound for each value of \code{x}.
#'
#' @export

interpolation_value_error_noncons <- function(x, xk, phid3) {
    return(sapply(x, function(x) {
        f <- Vectorize(function(u) kernel_value_error(xk, x, u)*phid3(u)/2)
        n <- length(xk)
        return(sum(sapply(1:(n - 1), function(i) {
            return(integrate(f, lower=xk[i], upper=xk[i + 1], stop.on.error=FALSE)$value)
        })))
    }))
}

#' @export
interpolation_deriv_error_noncons <- function(x, xk, phid3) {
    return(sapply(x, function(x) {
        f <- Vectorize(function(u) kernel_deriv_error(xk, x, u)*phid3(u)/2)
        n <- length(xk)
        return(sum(sapply(1:(n - 1), function(i) {
            return(integrate(f, lower=xk[i], upper=xk[i + 1], stop.on.error=FALSE)$value)
        })))
    }))
}
