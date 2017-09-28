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
#' @param average The average over the entire distribution. Use \code{NULL} for
#' unknown. (Default is \code{NULL}.)
#' @param last_bracketshare The share of the last bracket. Use \code{NULL} for
#' unknown. (Default is \code{NULL}.)
#' @param last_bracketavg The average in the last bracket. Use \code{NULL} for
#' unknown. (Default is \code{NULL}.)
#' @param last_invpareto The inverted Pareto coefficient at the last threshold.
#' Use \code{NULL} for unknown. (Default is \code{NULL}.)
#' @param bottom_model Which model to use at the bottom of the distribution?
#' Only relevant if \code{min(p) >= 0}. Either \code{"gpd"} for the generalized
#' Pareto distribution, \code{"hist"} for histogram density, or \code{"dirac"}.
#' Default is \code{"hist"} if \code{min(threshold) > 0}, \code{"dirac"} if
#' \code{min(threshold) == 0} and \code{"gpd"} otherwise.
#' @param lower_bound Lower bound of the distribution. Only relevant if
#' \code{min(p) > 0}. Default is \code{0}.
#' @param binf The asymptotic value of the inverted Pareto coefficient. If
#' \code{NULL}, it is estimated from the data (recommended, unless the
#' estimated value implies infinite mean). Default is \code{NULL}.
#' @param fast Use a faster but less precise method (split-histogram)?
#' Default is \code{FALSE}.
#'
#' @return An object of class \code{gpinter_dist_orig}.
#'
#' @export

thresholds_fit <- function(p, threshold, average=NULL, last_bracketshare=NULL,
                           last_bracketavg=NULL, last_invpareto=NULL,
                           bottom_model=NULL, lower_bound=0, binf=NULL, fast=FALSE) {

    input <- clean_input_thresholds(p, threshold, average,
        last_bracketavg, last_invpareto,
        bottom_model, lower_bound
    )

    p <- input$p
    n <- input$n
    bottom_model <- input$bottom_model
    lower_bound  <- input$lower_bound
    threshold    <- input$threshold
    last_m       <- input$last_m

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
    sn <- (yk[n] - yk[n - 1])/(xk[n] - xk[n - 1])
    if (is.null(binf) || is.na(binf)) {
        # sn correspond the tail index index: make sure it is lower than 1
        if (sn >= 1) {
            sn <- (3 - 1)/3
            warning(paste("The asymptotic b(p) implied by the data is infinite.",
                "Using b(p) = 3 instead. You may want to set a different value yourself",
                "using the parameter 'binf'."), .immediate=TRUE)
        }
    }
    # Calculate the other derivatives
    # Ensure that the function is increasing using the PCHIP algorithm
    delta <- (yk[2:n] - yk[1:(n - 1)])/(xk[2:n] - xk[1:(n - 1)])
    # Using the right derivative leads to more robust results
    sk <- c(
        delta[1:(n - 1)],
        sn
    )
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
    xk <- c(rep(NA, sum(threshold <= 0)), xk)
    yk <- c(rep(NA, sum(threshold <= 0)), yk)
    sk <- c(rep(NA, sum(threshold <= 0)), sk)
    n <- length(xk)

    # Deal with the part of the distribution that are zero of negative: use a
    # simple histogram density here (ie. linear interpolation of the quantile)
    use_hist <- NULL
    fk <- NULL
    for (i in 1:(n - 1)) {
        if (qk[i] <= 0) {
            fk <- c(fk, (qk[i + 1] - qk[i])/(pk[i + 1] - pk[i]))
            use_hist <- c(use_hist, TRUE)
        } else {
            fk <- c(fk, NA)
            use_hist <- c(use_hist, FALSE)
        }
    }

    # Calculate truncated average within each bracket
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

    # Calculate average in first bracket if necessary
    if (pk[1] > 0) {
        m0 <- pk[1]*(lower_bound + qk[1])/2
        mk <- c(m0, mk)
        pk <- c(0, pk)
        qk <- c(lower_bound, qk)
        n <- n + 1
    }

    # Set the aveage/Pareto coef in the last bracket if required
    if (!is.null(last_m)) {
        mk[n] <- last_m
    }

    # Adjust mk to match the average is it was given
    if (!is.null(average) && !is.na(average)) {
        missing_income <- average - sum(mk)

        dk <- diff(c(pk, 1))

        if (missing_income >= 0 & is.null(last_m)) {
            # If missing income is positive, and average at the top is not
            # predetermined, add it to the top
            mk[n] <- mk[n] + missing_income
        } else if (missing_income >= 0 & !is.null(last_m)) {
            # Here average in last bracket is predetermined, so we adjust
            # the other brackets
            target_avg <- average - mk[n]
            lambda <- (target_avg - sum(mk[1:(n - 1)]))/(sum(qk[2:n]*dk[1:(n - 1)]) - sum(mk[1:(n - 1)]))
            if (lambda >= 1 || lambda <= 0) {
                stop("input thresholds are inconsistent with the average")
            }
            mk[1:(n - 1)] <- mk[1:(n - 1)] + lambda*(qk[2:n]*dk[1:(n - 1)] - mk[1:(n - 1)])
        } else if (missing_income < 0 & is.null(last_m)) {
            # We lower the average in every bracket
            lambda <- -(average - sum(mk))/(sum(mk) - sum(qk*dk))
            if (lambda >= 1 || lambda <= 0) {
                stop("input thresholds are inconsistent with the average")
            }
            mk <- mk - lambda*(mk - qk*dk)
        } else if (missing_income < 0 & !is.null(last_m)) {
            # We lower the average in every bracket except the last
            target_avg <- average - mk[n]
            lambda <- -(target_avg - sum(mk[1:(n - 1)]))/(sum(mk[1:(n - 1)]) - sum(qk[1:(n - 1)]*dk[1:(n - 1)]))
            if (lambda >= 1 || lambda <= 0) {
                stop("input thresholds are inconsistent with the average")
            }
            mk[1:(n - 1)] <- mk[1:(n - 1)] - lambda*(mk[1:(n - 1)] - qk[1:(n - 1)]*dk[1:(n - 1)])
        }
    }

    average <- sum(mk)
    bracketavg <- mk/diff(c(pk, 1))

    # Remove first bracket for more robust results
    if (pk[1] == 0) {
        lower_bound <- qk[1]
        qk <- qk[-1]
        pk <- pk[-1]
        bracketavg <- bracketavg[-1]
    }

    return(tabulation_fit(pk, qk, average, bracketavg=bracketavg,
        bottom_model=bottom_model, lower_bound=lower_bound, binf=binf, fast=fast))
}
