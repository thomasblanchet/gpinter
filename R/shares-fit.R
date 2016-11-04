#' @title Fit a nonparametric distribution on fractile shares
#'
#' @author Thomas Blanchet
#'
#' @description Estimate a distribution nonparametrically based on tabulations
#' of a distribution which include:
#' \itemize{
#'     \item fractiles of the data,
#'     \item the share of each bracket
#' }
#'
#' @param p A vector of values in [0, 1].
#' @param average The average over the entire distribution.
#' @param bracketshare The corresponding bracket share.
#' @param topshare The corresponding top share.
#' @param bracketavg The corresponding bracket average.
#' @param topavg The corresponding top average.
#' @param ci Should confidence interval (identification region) be computed?
#' Default is \code{TRUE}.
#' @param samplesize The size of the underlying sample.
#' @param deriv3max A functional bound of the third derivative of the
#' interpolation function.
#'
#' @importFrom stats integrate
#'
#' @export

shares_fit <- function(p, average, bracketshare=NULL, topshare=NULL,
                       bracketavg=NULL, topavg=NULL, ci=TRUE,
                       samplesize=NULL, deriv3max=NULL) {

    # Number of interpolation points
    n <- length(p)
    if (n < 3) {
        stop("The method requires at least three interpolation points.")
    }
    # Sort the input data
    ord <- order(p)
    p <- p[ord]

    # Put the information on average in the right format (truncated average)
    if (!is.null(bracketshare)) {
        if (length(bracketshare) != n) {
            stop("'p' and 'bracketshare' must have the same length.")
        }
        bracketshare <- bracketshare[ord]
        m <- rev(cumsum(rev(bracketshare*average)))
    } else if (!is.null(topshare)) {
        if (length(bracketshare) != n) {
            stop("'p' and 'topshare' must have the same length.")
        }
        topshare <- topshare[ord]
        m <- average*topshare
    } else if (!is.null(bracketavg)) {
        if (length(bracketavg) != n) {
            stop("'p' and 'bracketavg' must have the same length.")
        }
        bracketavg <- bracketavg[ord]
        m <- rev(cumsum(rev(diff(c(p, 1))*bracketavg)))
    } else if (!is.null(topavg)) {
        if (length(topavg) != n) {
            stop("'p' and 'topavg' must have the same length.")
        }
        topavg <- topavg[ord]
        m <- (1 - p)*topavg
    } else {
        stop("You must specify one of 'bracketshare', 'topshare', 'bracketavg' or 'topavg'.")
    }

    # Sanity check of the data
    # Truncated mean function is decreasing
    if (any(diff(m) >= 0)) {
        stop("Truncated average must be strictly decreasing.")
    }
    # Truncated mean function is concanve
    for (i in 2:(n - 1)) {
        chord <- m[i - 1] + (m[i + 1] - m[i - 1])*(p[i] - p[i - 1])/(p[i +1] - p[i - 1])
        if (m[i] <= chord) {
            stop("Truncated average must be concave.")
        }
    }
    # Percentiles between 0 and 1
    if (any(p >= 1) | any(p < 0)) {
        stop("The elements of 'p' must be >=0 and <1.")
    }

    # Log-transform of the data
    xk <- -log(1 - p)
    yk <- -log(m)

    # Estimate the first derivative
    sk <- c(
        right_derivative(
            xk[1], xk[2], xk[3],
            yk[1], yk[2], yk[3]
        ),
        central_derivative(
            xk[1:(n - 2)], xk[2:(n - 1)], xk[3:n],
            yk[1:(n - 2)], yk[2:(n - 1)], yk[3:n]
        ),
        left_derivative(
            xk[n - 2], xk[n - 1], xk[n],
            yk[n - 2], yk[n - 1], yk[n]
        )
    )

    # Estimate the second derivative
    ak <- c(
        second_derivative(
            xk[1], xk[2], xk[3],
            yk[1], yk[2], yk[3]
        ),
        second_derivative(
            xk[1:(n - 2)], xk[2:(n - 1)], xk[3:n],
            yk[1:(n - 2)], yk[2:(n - 1)], yk[3:n]
        ),
        second_derivative(
            xk[n - 2], xk[n - 1], xk[n],
            yk[n - 2], yk[n - 1], yk[n]
        )
    )

    # Estimate the thresholds
    threshold <- exp(xk - yk)*sk

    # Object to return
    result <- list()
    class(result) <- "fitted_shares"
    result$pk <- p
    result$xk <- xk
    result$yk <- yk
    result$sk <- sk
    result$ak <- ak
    result$qk <- threshold
    result$mk <- m
    result$bk <- m/((1 - p)*threshold)
    result$average <- average

    # Estimate the parameters of the generalized Pareto distribution at the top
    param_top <- gpd_top_parameters(xk[n], yk[n], sk[n], ak[n])
    result$mu_top    <- param_top$mu
    result$sigma_top <- param_top$sigma
    result$xi_top    <- param_top$xi

    # Estimate the parameters of the generalized Pareto distribution at the
    # bottom, if necessary
    if (min(p) > 0) {
        param_bottom <- gpd_bottom_parameters(xk[1], yk[1], sk[1], ak[1], average)
    } else {
        param_bottom <- list(mu=NA, sigma=NA, xi=NA)
    }
    result$mu_bottom    <- param_bottom$mu
    result$sigma_bottom <- param_bottom$sigma
    result$xi_bottom    <- param_bottom$xi

    result$phi <- function(x) {
        return(phi(x,
            xk, yk, sk, ak,
            param_top$mu, param_top$sigma, param_top$xi,
            param_bottom$mu, param_bottom$sigma, param_bottom$xi
        ))
    }
    result$deriv_phi <- function(x) {
        return(deriv_phi(x,
            xk, yk, sk, ak,
            param_top$mu, param_top$sigma, param_top$xi,
            param_bottom$mu, param_bottom$sigma, param_bottom$xi
        ))
    }
    result$fitted_quantile <- function(p) {
        return(fitted_quantile(p,
            xk, yk, sk, ak,
            param_top$mu, param_top$sigma, param_top$xi,
            param_bottom$mu, param_bottom$sigma, param_bottom$xi
        ))
    }
    result$fitted_density <- function(q) {
        return(fitted_density(q,
            p, threshold, xk, yk, sk, ak,
            param_top$mu, param_top$sigma, param_top$xi,
            param_bottom$mu, param_bottom$sigma, param_bottom$xi
        ))
    }
    result$fitted_top_share <- function(p) {
        return(fitted_top_share(p,
            xk, yk, sk, ak, average,
            param_top$mu, param_top$sigma, param_top$xi,
            param_bottom$mu, param_bottom$sigma, param_bottom$xi
        ))
    }
    result$fitted_invpareto <- function(p) {
        return(fitted_invpareto(p,
            xk, yk, sk, ak,
            param_top$mu, param_top$sigma, param_top$xi,
            param_bottom$mu, param_bottom$sigma, param_bottom$xi
        ))
    }

    # Errors
    if (is.null(deriv3max)) {
        deriv4max <- function(x) {
            return(1 + 99/(1 + 10*x^2))
        }
    }

    if (ci) {
        result$ci_phi <- function(x) {
            y <- result$phi(x)
            e <- spline_error_degree2(x, xk, deriv3max)
            return(list(lower=(y - e), upper=(y + e)))
        }
        result$ci_deriv_phi <- function(x) {
            dydx <- result$deriv_phi(x)
            e <- deriv_spline_error_degree2(x, xk, deriv3max)
            return(list(lower=(dydx - e), upper=(dydx + e)))
        }
        result$ci_quantile <- function(p) {
            x <- -log(1 - p)
            y <- result$phi(x)
            dydx <- result$deriv_phi(x)
            q <- dydx*exp(x - y)
            e1 <- spline_error_degree2(x, xk, deriv3max)
            e2 <- deriv_spline_error_degree2(x, xk, deriv3max)
            y_lower <- y - e1
            y_upper <- y + e1
            dydx_lower <- dydx - e2
            dydx_upper <- dydx + e2
            lower <- dydx_lower*exp(x - y_upper)
            upper <- dydx_upper*exp(x - y_lower)
            return(list(lower=lower, upper=upper))
        }
        result$ci_top_share <- function(p) {
            x <- -log(1 - p)
            y <- result$phi(x)
            e <- spline_error_degree2(x, xk, deriv3max)
            y_lower <- y - e
            y_upper <- y + e
            lower <- exp(-y_upper)/average
            upper <- exp(-y_lower)/average
            return(list(lower=lower, upper=upper))
        }
        result$ci_invpareto <- function(p) {
            x <- -log(1 - p)
            dydx <- result$deriv_phi(x)
            invpareto <- 1/dydx
            e <- spline_error_degree2(x, xk, deriv3max)
            dydx_lower <- dydx - e
            dydx_upper <- dydx + e
            lower <- 1/dydx_upper
            upper <- 1/dydx_lower
            return(list(lower=lower, upper=upper))
        }
    } else {
        # Include the function, but have them return NA (facilitate the
        # use of the function in the Shiny app)
        result$ci_phi <- function(x) {
            return(NA)
        }
        result$ci_deriv_phi <- function(x) {
            return(NA)
        }
        result$ci_quantile <- function(p) {
            return(NA)
        }
        result$ci_top_share <- function(p) {
            return(NA)
        }
        result$ci_invpareto <- function(p) {
            return(NA)
        }
    }

    return(result)
}

