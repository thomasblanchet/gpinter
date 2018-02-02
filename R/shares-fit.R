#' @title Fit a nonparametric distribution on tabulated data without thresholds
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Estimate a distribution nonparametrically based on tabulations
#' of a distribution which include:
#' \itemize{
#'     \item fractiles of the data,
#'     \item the share of each bracket.
#' }
#'
#' @param p A vector of values in [0, 1].
#' @param average The average over the entire distribution.
#' @param bracketshare The corresponding bracket share.
#' @param topshare The corresponding top share.
#' @param bracketavg The corresponding bracket average.
#' @param topavg The corresponding top average.
#' @param first_threshold The value of the first threshold. If \code{NULL}, it
#' is estimated from the data. Default is \code{NULL}.
#' @param last_invpareto The inverted Pareto coefficient at the last threshold.
#' Use \code{NULL} for unknown. (Default is \code{NULL}.)
#' @param bottom_model Which model to use at the bottom of the distribution?
#' Only relevant if \code{min(p) > 0}. Either \code{"gpd"} for the generalized
#' Pareto distribution, or \code{"hist"} for histogram density. Default is
#' \code{"hist"} if \code{min(threshold) > 0}, and \code{"gpd"} otherwise.
#' @param lower_bound Lower bound of the distribution. Only relevant if
#' \code{min(p) > 0}. Default is \code{0}.
#' @param binf Asymptotic Pareto coefficient. If \code{NULL} or \code{NA},
#' it is directly estimated from the data. Default is \code{NULL}.
#' @param fast Use a faster but less precise method (split-histogram).
#' Default is \code{FALSE}.
#'
#' @return An object of class \code{gpinter_dist_orig}.
#'
#' @importFrom stats integrate optim
#' @importFrom nloptr nloptr
#'
#' @export

shares_fit <- function(p, average=NULL, bracketshare=NULL, topshare=NULL,
                       bracketavg=NULL, topavg=NULL, first_threshold=NULL,
                       last_invpareto=NULL, bottom_model=NULL, lower_bound=0,
                       binf=NULL, fast=FALSE) {

    input <- clean_input_shares(p, average, bracketshare, topshare, bracketavg,
        topavg, first_threshold, bottom_model, lower_bound)

    p <- input$p
    m <- input$m
    n <- input$n
    average <- input$average
    bottom_model <- input$bottom_model
    lower_bound <- input$lower_bound

    # Log-transform of the data
    pk <- p
    mk <- m
    xk <- -log(1 - pk)
    yk <- -log(mk)

    # Estimate the second derivative at the last point
    an <- second_derivative(xk[n - 2], xk[n - 1], xk[n], yk[n - 2], yk[n - 1], yk[n])

    # Fit the quintic spline
    if (pk[1] == 0 & (mk[1] - mk[2]) > 0) {
        if (is.null(first_threshold)) {
            first_threshold <- 0
        }
    }
    if (is.null(first_threshold)) {
        param_spline <- clamped1_quintic_spline_noderiv(xk, yk, an)
        ak <- param_spline$ak
        sk <- param_spline$sk
    } else {
        s1 <- exp(yk[1] - xk[1])*first_threshold
        param_spline <- clamped2_quintic_spline_noderiv(xk, yk, s1, an)
        ak <- param_spline$ak
        sk <- param_spline$sk
    }

    # Check if the estimated function satisfies the constraints: if not, adjust
    # the estimated thresholds before passing it tabulation_fit
    constraints_ok <- TRUE
    threshold <- exp(xk - yk)*sk
    bracketavg <- -diff(c(m, 0))/diff(c(p, 1))
    if (threshold[1] >= bracketavg[1]) {
        constraints_ok <- FALSE
        if (bracketavg[1] > 0) {
            # Assume the distribution starts at zero
            if (pk[1] == 0) {
                threshold[1] <- 0
            } else {
                bottomavg <- (average - sum(m))/pk[1]
                threshold[1] <- 2*bottomavg
            }
        } else {
            threshold[1] <- 2*bracketavg[1]
        }
    }
    for (i in 2:n) {
        if (bracketavg[i - 1] >= threshold[i] || bracketavg[i] <= threshold[i]) {
            constraints_ok <- FALSE
            threshold[i] <- 0.5*(bracketavg[i - 1] + bracketavg[i])
        }
        if (!is_increasing(xk[i - 1], xk[i], yk[i - 1], yk[i], sk[i - 1], sk[i], ak[i - 1], ak[i])) {
            constraints_ok <- FALSE
        }
    }

    if (!is.null(last_invpareto) && !is.na(last_invpareto)) {
        last_threshold <- bracketavg[length(bracketavg)]/last_invpareto
        if (last_invpareto <= 1) {
            stop("the inverted Pareto coefficient must be > 1")
        } else if (last_threshold < threshold[length(threshold) - 1]) {
            max_last_invpareto <- bracketavg[length(bracketavg)]/threshold[length(threshold) - 1]
            stop(sprintf("given your data, the last Pareto coefficient must be below %.2f", max_last_invpareto))
        }
        threshold[length(threshold)] <- last_threshold
    }

    # Pass the estimated thresholds to tabulation_fit
    return(tabulation_fit(p=pk, threshold=threshold, average=average,
        bracketavg=bracketavg, bottom_model=bottom_model, lower_bound=lower_bound, binf=binf, fast=fast))
}
