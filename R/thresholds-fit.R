#' @title Fit a nonparametric distribution on tabulated data with thresholds only
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Estimate a distribution nonparametrically based on tabulations
#' of a distribution which include:
#' \itemize{
#'     \item fractiles of the data,
#'     \item the corresponding thresholds.
#' }
#'
#' @param p A vector of values in [0, 1].
#' @param threshold The quantiles corresponding to \code{p}.
#' @param average The average over the entire distribution.
#' @param bottom_model Which model to use at the bottom of the distribution?
#' Only relevant if \code{min(p) >= 0}. Either \code{"gpd"} for the generalized
#' Pareto distribution, \code{"hist"} for histogram density, or \code{"dirac"}.
#' Default is \code{"hist"} if \code{min(threshold) > 0}, \code{"dirac"} if
#' \code{min(threshold) == 0} and \code{"gpd"} otherwise.
#' @param hist_lower_bound Lower bound of the histogram in the bottom of the
#' distribution. Only relevant if \code{min(p) > 0} and
#' \code{bottom_model == "hist"}. Default is \code{0}.
#' @param binf The asymptotic value of the inverted Pareto coefficient. If
#' \code{NULL}, it is estimated from the data (recommended, unless the
#' estimated value implies infinite mean). Default is \code{NULL}.
#'
#' @return An object of class \code{gpinter_dist_orig}.
#'
#' @export

thresholds_fit <- function(p, threshold, average, bottom_model=NULL,
                           hist_lower_bound=0, binf=NULL) {
    # Number of interpolation points
    n <- length(p)
    if (n < 3) {
        stop("The method requires at least three interpolation points.")
    }
    # Sort the input data
    ord <- order(p)
    p <- p[ord]
    threshold <- threshold[ord]

    # Model for the bottom
    if (p[1] > 0 && is.null(bottom_model)) {
        if (threshold[1] > 0) {
            bottom_model <- "hist"
        } else if (threshold[1] == 0) {
            bottom_model <- "dirac"
        } else {
            bottom_model <- "pareto"
        }
    }
    if (!is.null(bottom_model) && !bottom_model %in% c("hist", "gpd", "dirac")) {
        stop("'bottom_model' must be one of 'hist', 'pareto', 'dirac', or NULL.")
    }
    if (!is.null(bottom_model) && bottom_model == "hist" && hist_lower_bound > threshold[1]) {
        stop("'hist_lower_bound' must be smaller than min(threshold).")
    }

    # Calculate the tail function
    pk <- p
    qk <- threshold
    xk <- -log(1 - pk)
    yk <- log(qk)
    xk <- xk[qk > 0]
    yk <- yk[qk > 0]

    # Ensure that we have at least three point in the positive part of the
    # distribution
    n <- length(xk)
    if (n < 3) {
        stop("The method requires at least three positive thresholds.")
    }
    # Estimate the derivative at the last point
    if (is.null(binf)) {
        sn <- (yk[n] - yk[n - 1])/(xk[n] - xk[n - 1])
    } else {
        if (binf > 1) {
            sn <- (binf - 1)/binf
        } else {
            stop("Asymptotic inverted Pareto coefficient must be below 1.")
        }
    }
    # sn correspond the tail index index: make sure it is lower than 1
    if (sn >= 1) {
        sn <- 1/1.5
        warning(paste("The asymptotic b(p) implied by the data is infinite.",
            "Using b(p) = 3 instead. You may want to set a different value yourself",
            "using the parameter 'binf'."), .immediate=TRUE)
    }
    # Calculate the other derivatives
    sk <- clamped_cubic_spline(xk, yk, sn)
    # Ensure that the function is increasing using the PCHIP algorithm
    delta <- (yk[2:n] - yk[1:(n - 1)])/(sk[2:n] - sk[1:(n - 1)])
    alpha <- sk[1:(n - 1)]/delta
    beta <- sk[2:n]/delta
    tau <- 3/sqrt(alpha^2 + beta^2)
    for (i in 1:(n - 1)) {
        if (alpha[i]^2 + beta[i]^2 > 9) {
            sk[i] <- tau[i]*alpha[i]*delta[i]
            sk[i + 1] <- tau[i]*beta[i]*delta[i]
        }
    }
    # Re-add the non positive points
    xk <- c(xk[threshold <= 0], xk)
    yk <- c(yk[threshold <= 0], yk)
    sk <- c(rep(NA, sum(threshold <= 0)), sk)
    n <- length(xk)

    # Deal with the part of the distribution that are zero of negative: use a
    # simple histogram density here (ie. linear interpolation of the quantile)
    use_hist <- NULL
    fk <- NULL
    for (i in 1:(n - 1)) {
        if (qk[i + 1] <= 0) {
            fk <- c(fk, (qk[i + 1] - qk[i])/(pk[i + 1] - pk[i]))
            use_hist <- c(use_hist, TRUE)
        } else {
            fk <- c(fk, NA)
            use_hist <- c(use_hist, FALSE)
        }
    }

    # Calculate truncated average with each bracket
    mk <- NULL
    for (i in 1:(n - 1)) {
        if (use_hist[i]) {
            mk <- c(mk, (qk[i + 1] + qk[i])*(pk[i + 1] - pk[i])/2)
        } else {
            mk <- c(mk, integrate(function(p) {
                x <- -log(1 - p)
                y <- g(x, xk[i], xk[i + 1], yk[i], yk[i + 1], sk[i], sk[i + 1])
                return(exp(y))
            }, lower=p[i], upper=p[i + 1])$value)
        }
    }
    mk <- c(mk, (1 - pk[n])*qk[n]/(1 - sn))

    if (pk[1] > 0) {
        m0 <- pk[1]*(hist_lower_bound + qk[1])/2

        mk[n] <- mk[n] + average - sum(mk) - m0

        bracketavg <- mk/diff(c(pk, 1))
        return(tabulation_fit(pk, qk, average, bracketavg=bracketavg,
            bottom_model=bottom_model, hist_lower_bound=hist_lower_bound))
    } else {
        missing_income <- (average - sum(mk))
        first_bracket_avg <- (mk[1] + missing_income)/pk[2]
        # Add missing income to the last bracket
        mk[n] <- mk[n] + missing_income

        bracketavg <- mk/diff(c(pk, 1))
        return(tabulation_fit(pk, qk, average, bracketavg=bracketavg,
            bottom_model=bottom_model, hist_lower_bound=hist_lower_bound))
    }
}
