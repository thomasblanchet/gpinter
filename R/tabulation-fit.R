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

#' @title Fit a nonparametric distribution on tabulated data
#'
#' @author Thomas Blanchet
#'
#' @description Estimate a distribution nonparametrically based on tabulations
#' of a distribution which include:
#' \itemize{
#'     \item fractiles of the data,
#'     \item the corresponding brackets,
#'     \item the share of each bracket.
#' }
#'
#' @param p A vector of values in [0, 1].
#' @param threshold The quantiles corresponding to \code{p}.
#' @param average The average over the entire distribution.
#' @param bracketshare The corresponding bracket share.
#' @param topshare The corresponding top share.
#' @param bracketavg The corresponding bracket average.
#' @param topavg The corresponding top average.
#' @param citype The type of confidence interval to compute. 0 = no confidence
#' interval, 1 = spline interpolation error, 2 = spline interpolation error and
#' sampling error (may be slow).
#' @param cilevel The confidence level. Default is 0.05.
#' @param samplesize The size of the underlying sample.
#' @param deriv4max A function majorant of the fourth derivative of the
#' interpolation function.
#' @param nrep The number of resampling to perform in order to estimate the
#' sampling error.
#'
#' @importFrom graphics lines plot points polygon
#' @importFrom stats integrate runif quantile
#'
#' @export

tabulation_fit <- function(p, threshold, average, bracketshare=NULL, topshare=NULL,
                           bracketavg=NULL, topavg=NULL, citype=1, cilevel=0.05,
                           samplesize=NULL, deriv4max=NULL, nrep=1000) {
    # Number of interpolation points
    n <- length(p)
    if (n < 3) {
        stop("The method requires at least three interpolation points.")
    }
    if (length(threshold) != n) {
        stop("'p' and 'threshold' must have the same length.")
    }
    # Sort the input data
    ord <- order(p)
    p <- p[ord]
    threshold <- threshold[ord]

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
    # Quantile function is increasing
    if (any(diff(threshold) <= 0)) {
        stop("Threshold must be strictly increasing.")
    }
    # Percentiles between 0 and 1
    if (any(p >= 1) | any(p < 0)) {
        stop("The elements of 'p' must be >=0 and <1.")
    }
    # The average between each bracket is within the bracket
    bracketavg <- -diff(c(m, 0))/diff(c(p, 1))
    if (any(bracketavg < threshold) | any(bracketavg[1:(n - 1)] > threshold[2:n])) {
        stop("Input data on quantiles and moments is inconsistent.")
    }

    # Log-transform of the data
    xk <- -log(1 - p)
    yk <- -log(m)
    sk <- (1 - p)*threshold/m

    # Calculate the second derivative
    ak <- c(
        right_derivative(
            xk[1], xk[2], xk[3],
            sk[1], sk[2], sk[3]
        ),
        central_derivative(
            xk[1:(n - 2)], xk[2:(n - 1)], xk[3:n],
            sk[1:(n - 2)], sk[2:(n - 1)], sk[3:n]
        ),
        left_derivative(
            xk[n - 2], xk[n - 1], xk[n],
            sk[n - 2], sk[n - 1], sk[n]
        )
    )

    # Object to return
    result <- list()
    class(result) <- "fitted_tabulation"
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
    if (is.null(deriv4max)) {
        deriv4max <- function(x) {
            return(1 + 99/(1 + 10*x^2))
        }
    }

    if (citype == 2) {
        # Check that the sample size is at least equal to the number of brackets
        if (is.null(samplesize) | is.na(samplesize)) {
            stop("'samplesize' must not be NULL or NA if citype == 2")
        } else if (samplesize < n){
            stop("'samplesize' too low")
        }

        # Estimate sampling error via resampling
        cl <- parallel::makeCluster(parallel::detectCores())
        parallel::clusterExport(cl, envir=environment(), varlist=c(
            "fitted_quantile", "phi", "deriv_phi", "gpd_top_phi", "gpd_bottom_phi",
            "gpd_top_deriv_phi", "gpd_bottom_deriv_phi", "gpd_top_mean",
            "gpd_bottom_mean", "gpd_top_quantile", "gpd_bottom_quantile",
            "quintic_spline", "deriv_quintic_spline", "h00", "h01", "h10",
            "h11", "h20", "h21", "h00d1", "h01d1", "h10d1", "h11d1",
            "h20d1", "h21d1", "p", "n", "samplesize", "xk", "right_derivative",
            "central_derivative", "left_derivative", "gpd_top_parameters",
            "gpd_bottom_parameters", "result"
        ))
        resamp_param <- parallel::parLapply(cl, 1:nrep, function(i) {
            sample <- sort(result$fitted_quantile(runif(samplesize)))
            truncmean <- rev(cumsum(rev(sample)))/samplesize
            threshold <- sample[floor(p*samplesize)]
            m <- truncmean[floor(p*samplesize)]

            yk <- -log(m)
            sk <- (1 - p)*threshold/m
            ak <- c(
                right_derivative(
                    xk[1], xk[2], xk[3],
                    sk[1], sk[2], sk[3]
                ),
                central_derivative(
                    xk[1:(n - 2)], xk[2:(n - 1)], xk[3:n],
                    sk[1:(n - 2)], sk[2:(n - 1)], sk[3:n]
                ),
                left_derivative(
                    xk[n - 2], xk[n - 1], xk[n],
                    sk[n - 2], sk[n - 1], sk[n]
                )
            )

            average <- mean(sample)

            param_top <- gpd_top_parameters(xk[n], yk[n], sk[n], ak[n])
            if (min(p) > 0) {
                param_bottom <- gpd_bottom_parameters(xk[1], yk[1], sk[1], ak[1], average)
            } else {
                param_bottom <- list(mu=NA, sigma=NA, xi=NA)
            }

            return(list(yk=yk, sk=sk, ak=ak, average=average,
                mu_top=param_top$mu, sigma_top=param_top$sigma, xi_top=param_top$xi,
                mu_bottom=param_bottom$mu, sigma_bottom=param_bottom$sigma, xi_bottom=param_bottom$xi))
        })
        parallel::stopCluster(cl)

        # Confidence interval functions
        result$ci_phi <- function(x) {
            # Estimate sampling error
            replication <- sapply(resamp_param, function(sample) {
                return(phi(x,
                    xk, sample$yk, sample$sk, sample$ak,
                    sample$mu_top, sample$sigma_top, sample$xi_top,
                    sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
                ))
            })
            if (nrow(replication) > 1) {
                lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
                upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
            } else {
                lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
                upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
            }
            # Estimate non-sampling error
            e <- spline_error(x, xk, deriv4max)
            e[is.na(e)] <- 0

            return(list(lower=(lower_resamp - e), upper=(upper_resamp + e)))
        }
        result$ci_deriv_phi <- function(x) {
            # Estimate sampling error
            replication <- sapply(resamp_param, function(sample) {
                return(deriv_phi(x,
                    xk, sample$yk, sample$sk, sample$ak,
                    sample$mu_top, sample$sigma_top, sample$xi_top,
                    sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
                ))
            })
            if (nrow(replication) > 1) {
                lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
                upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
            } else {
                lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
                upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
            }
            # Estimate non-sampling error
            e <- deriv_spline_error(x, xk, deriv4max)
            e[is.na(e)] <- 0

            return(list(lower=(lower_resamp - e), upper=(upper_resamp + e)))
        }
        result$ci_quantile <- function(p) {
            x <- -log(1 - p)
            # Estimate sampling error
            replication <- sapply(resamp_param, function(sample) {
                return(fitted_quantile(p,
                    xk, sample$yk, sample$sk, sample$ak,
                    sample$mu_top, sample$sigma_top, sample$xi_top,
                    sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
                ))
            })
            if (nrow(replication) > 1) {
                lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
                upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
            } else {
                lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
                upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
            }
            y <- result$phi(x)
            dydx <- result$deriv_phi(x)
            q <- dydx*exp(x - y)
            u1 <- q - lower_resamp
            u2 <- upper_resamp - q
            # Estimate non-sampling error
            e1 <- spline_error(x, xk, deriv4max)
            e2 <- deriv_spline_error(x, xk, deriv4max)
            e1[is.na(e1)] <- 0
            e2[is.na(e2)] <- 0

            y_lower <- y - e1
            y_upper <- y + e1
            dydx_lower <- dydx - e2
            dydx_upper <- dydx + e2

            lower <- dydx_lower*exp(x - y_upper)
            upper <- dydx_upper*exp(x - y_lower)

            return(list(lower=(lower - u1), upper=(upper + u2)))
        }
        result$ci_top_share <- function(p) {
            x <- -log(1 - p)
            # Estimate sampling error
            replication <- sapply(resamp_param, function(sample) {
                return(fitted_top_share(p,
                    xk, sample$yk, sample$sk, sample$ak, sample$average,
                    sample$mu_top, sample$sigma_top, sample$xi_top,
                    sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
                ))
            })
            if (nrow(replication) > 1) {
                lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
                upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
            } else {
                lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
                upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
            }
            y <- result$phi(x)
            ts <- exp(-y)/average
            u1 <- ts - lower_resamp
            u2 <- upper_resamp - ts
            # Estimate non-sampling error
            e <- spline_error(x, xk, deriv4max)
            e[is.na(e)] <- 0
            y_lower <- y - e
            y_upper <- y + e

            lower <- exp(-y_upper)/average
            upper <- exp(-y_lower)/average

            return(list(lower=(lower - u1), upper=(upper + u2)))
        }
        result$ci_invpareto <- function(p) {
            x <- -log(1 - p)
            # Estimate sampling error
            replication <- sapply(resamp_param, function(sample) {
                return(fitted_invpareto(p,
                    xk, sample$yk, sample$sk, sample$ak,
                    sample$mu_top, sample$sigma_top, sample$xi_top,
                    sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
                ))
            })
            if (nrow(replication) > 1) {
                lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
                upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
            } else {
                lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
                upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
            }
            dydx <- result$deriv_phi(x)
            invpareto <- 1/dydx
            u1 <- invpareto - lower_resamp
            u2 <- upper_resamp - invpareto
            # Estimate non-sampling error
            e <- spline_error(x, xk, deriv4max)
            e[is.na(e)] <- 0

            dydx_lower <- dydx - e
            dydx_upper <- dydx + e

            lower <- 1/dydx_upper
            upper <- 1/dydx_lower

            return(list(lower=(lower - u1), upper=(upper + u2)))
        }
    } else if (citype == 1) {
        result$ci_phi <- function(x) {
            y <- result$phi(x)
            e <- spline_error(x, xk, deriv4max)
            return(list(lower=(y - e), upper=(y + e)))
        }
        result$ci_deriv_phi <- function(x) {
            dydx <- result$deriv_phi(x)
            e <- deriv_spline_error(x, xk, deriv4max)
            return(list(lower=(dydx - e), upper=(dydx + e)))
        }
        result$ci_quantile <- function(p) {
            x <- -log(1 - p)
            y <- result$phi(x)
            dydx <- result$deriv_phi(x)
            q <- dydx*exp(x - y)
            e1 <- spline_error(x, xk, deriv4max)
            e2 <- deriv_spline_error(x, xk, deriv4max)
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
            e <- spline_error(x, xk, deriv4max)
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
            e <- spline_error(x, xk, deriv4max)
            dydx_lower <- dydx - e
            dydx_upper <- dydx + e
            lower <- 1/dydx_upper
            upper <- 1/dydx_lower
            return(list(lower=lower, upper=upper))
        }
    } else {
        stop("'citype' must be 1 or 2")
    }

    return(result)
}
