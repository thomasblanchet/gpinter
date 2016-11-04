#' @title Interpolation function
#'
#' @author Thomas Blanchet
#'
#' @description Full interpolation function, combining the quintic spline
#' at the bottom and the generalized Pareto distribution at the top.
#'
#' @param x The function evaluation point.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#' @param mu_top The location parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param sigma_top The scale parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param xi_top The shape parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param mu_bottom The location parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param sigma_bottom The scale parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param xi_bottom The shape parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#'
#' @return The value of the interpolation function at \code{x}.
#'
#' @importFrom utils tail head

phi <- function(x, xk, yk, sk, ak,
    mu_top, sigma_top, xi_top,
    mu_bottom, sigma_bottom, xi_bottom) {

    xn <- tail(xk, n=1)
    x1 <- head(xk, n=1)
    y1 <- head(yk, n=1)

    return(ifelse(x > xn, {
        gpd_top_phi(x, 1 - exp(-xn), mu_top, sigma_top, xi_top)
    }, ifelse(x < x1, {
        gpd_bottom_phi(x, 1 - exp(-x1), y1, mu_bottom, sigma_bottom, xi_bottom)
    }, {
        quintic_spline(x, xk, yk, sk, ak)
    })))
}

#' @title Derivative of the interpolation function
#'
#' @author Thomas Blanchet
#'
#' @description Full interpolation function, combining the quintic spline
#' at the bottom and the generalized Pareto distribution at the top.
#'
#' @param x The function evaluation point.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#' @param mu_top The location parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param sigma_top The scale parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param xi_top The shape parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param mu_bottom The location parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param sigma_bottom The scale parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param xi_bottom The shape parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#'
#' @return The value of the derivative of the interpolation at \code{x}.
#'
#' @importFrom utils tail head

deriv_phi <- function(x, xk, yk, sk, ak,
    mu_top, sigma_top, xi_top,
    mu_bottom, sigma_bottom, xi_bottom) {

    xn <- tail(xk, n=1)
    x1 <- head(xk, n=1)
    y1 <- head(yk, n=1)

    return(ifelse(x > xn, {
        gpd_top_deriv_phi(x, 1 - exp(-xn), mu_top, sigma_top, xi_top)
    }, ifelse(x < x1, {
        gpd_bottom_deriv_phi(x, 1 - exp(-x1), y1, mu_bottom, sigma_bottom, xi_bottom)
    }, {
        deriv_quintic_spline(x, xk, yk, sk, ak)
    })))
}

#' @title Quantile function
#'
#' @author Thomas Blanchet
#'
#' @description Full quantile function, combining the quintic spline
#' at the bottom and the generalized Pareto distribution at the top.
#'
#' @param p The function evaluation point.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#' @param mu_top The location parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param sigma_top The scale parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param xi_top The shape parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param mu_bottom The location parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param sigma_bottom The scale parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param xi_bottom The shape parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#'
#' @return The value of the quantile at \code{p} from below.

fitted_quantile <- function(p, xk, yk, sk, ak,
    mu_top, sigma_top, xi_top,
    mu_bottom, sigma_bottom, xi_bottom) {
    x <- -log(1 - p)
    y <- phi(x,
        xk, yk, sk, ak,
        mu_top, sigma_top, xi_top,
        mu_bottom, sigma_bottom, xi_bottom
    )
    dydx <- deriv_phi(x,
        xk, yk, sk, ak,
        mu_top, sigma_top, xi_top,
        mu_bottom, sigma_bottom, xi_bottom
    )
    return(exp(x - y)*dydx)
}

#' @title Density function
#'
#' @author Thomas Blanchet
#'
#' @description Full density function, combining the quintic spline
#' in the middle and the generalized Pareto distribution at the extremes.
#'
#' @param q The function evaluation point.
#' @param pk The probabilities of the tabulation.
#' @param qk The associated thresholds of the tabulation.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#' @param mu_top The location parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param sigma_top The scale parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param xi_top The shape parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param mu_bottom The location parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param sigma_bottom The scale parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param xi_bottom The shape parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#'
#' @return The value of the density at \code{q}.
#'
#' @importFrom stats uniroot

fitted_density <- function(q, pk, qk, xk, yk, sk, ak,
    mu_top, sigma_top, xi_top,
    mu_bottom, sigma_bottom, xi_bottom) {

    n <- length(pk)
    xn <- tail(xk, n=1)
    x1 <- head(xk, n=1)
    y1 <- head(yk, n=1)

    # Determine the distribution support
    if (pk[1] == 0) {
        lower <- qk[1]
    } else if (xi_bottom < 0) {
        lower <- mu_bottom + sigma_bottom/xi_bottom
    } else {
        lower <- -Inf
    }
    if (pk[n] == 1) {
        upper <- qk[n]
    } else if (xi_top < 0) {
        upper <- mu_top - sigma_top/xi_top
    } else {
        upper <- +Inf
    }

    # Calculate the density
    return(ifelse((q <= lower) | (q >= upper), {
        # We are outside of the distribution support
        0
    }, ifelse(q >= qk[n], {
        # We are in the top part of the distribution (GPD model)
        gpd_top_density(q, pk[n], mu_top, sigma_top, xi_top)
    }, ifelse(q <= qk[1], {
        # We are in the bottom part of the distribution (GPD model)
        gpd_bottom_density(q, pk[1], mu_bottom, sigma_bottom, xi_bottom)
    }, {
        # We are in the middle of the distribution (interpolation region)
        # Get the value of the probability associated to the quantile
        p <- sapply(q, function(q) {
            tryCatch(uniroot(
                f = function(p) {
                    x <- -log(1 - p)
                    y <- quintic_spline(x, xk, yk, sk, ak)
                    dydx <- deriv_quintic_spline(x, xk, yk, sk, ak)
                    return(exp(x - y)*dydx - q)
                },
                lower = pk[1],
                upper = pk[n]
            )$root, error = function(e) {
                return(NA)
            })
        })
        x <- -log(1 - p)
        y <- quintic_spline(x, xk, yk, sk, ak)
        dydx <- deriv_quintic_spline(x, xk, yk, sk, ak)
        d2ydx2 <- deriv2_quintic_spline(x, xk, yk, sk, ak)

        1/(exp(2*x - y)*(d2ydx2 + dydx*(1 - dydx)))
    }))))
}

#' @title Top share function
#'
#' @author Thomas Blanchet
#'
#' @description Top share function, combining the quintic spline
#' at the bottom and the generalized Pareto distribution at the top.
#'
#' @param p The function evaluation point.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#' @param average The average over the entire population.
#' @param mu_top The location parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param sigma_top The scale parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param xi_top The shape parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param mu_bottom The location parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param sigma_bottom The scale parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param xi_bottom The shape parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#'
#' @return The value of the share of the top 100*(1 - p)%.

fitted_top_share <- function(p, xk, yk, sk, ak, average,
    mu_top, sigma_top, xi_top,
    mu_bottom, sigma_bottom, xi_bottom) {
    x <- -log(1 - p)
    y <- phi(x,
        xk, yk, sk, ak,
        mu_top, sigma_top, xi_top,
        mu_bottom, sigma_bottom, xi_bottom
    )
    return(ifelse(p == 1, 0, ifelse(p == 0, 1, exp(-y)/average)))
}

#' @title Generalized inverted Pareto coefficient
#'
#' @author Thomas Blanchet
#'
#' @description Generalized inverted Pareto coefficient, combining the quintic spline
#' at the bottom and the generalized Pareto distribution at the top.
#'
#' @param p The function evaluation point.
#' @param xk A vector of interpolation points.
#' @param yk A vector of values at each interpolation point.
#' @param sk A vector of slopes at each interpolation point.
#' @param ak A vector of second derivatives at each interpolation point.
#' @param mu_top The location parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param sigma_top The scale parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param xi_top The shape parameter for the generalized Pareto distribution at
#' the top of the distribution.
#' @param mu_bottom The location parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param sigma_bottom The scale parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#' @param xi_bottom The shape parameter for the generalized Pareto distribution at
#' the bottom of the distribution.
#'
#' @return The value of the share of the top 100*(1 - p)%.

fitted_invpareto <- function(p, xk, yk, sk, ak,
    mu_top, sigma_top, xi_top,
    mu_bottom, sigma_bottom, xi_bottom) {
    x <- -log(1 - p)
    dydx <- deriv_phi(x,
        xk, yk, sk, ak,
        mu_top, sigma_top, xi_top,
        mu_bottom, sigma_bottom, xi_bottom
    )
    return(ifelse(p == 1, 1/(1 - xi_top), 1/dydx))
}
