#' @title Fit a nonparametric distribution on tabulated data
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
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
#' @param invpareto The inverted Pareto coefficient.
#' @param bottom_model Which model to use at the bottom of the distribution?
#' Only relevant if \code{min(p) > 0}. Either \code{"gpd"} for the generalized
#' Pareto distribution, \code{"hist"} for histogram density, or \code{"dirac"}.
#' Default is \code{"hist"} if \code{min(threshold) > 0}, \code{"dirac"} if
#' \code{min(threshold) == 0} and \code{"gpd"} otherwise.
#' @param lower_bound Lower bound of the distribution. Only relevant if
#' \code{min(p) > 0}. Default is \code{0}.
#'
#' @return An object of class \code{gpinter_dist_orig}.
#'
#' @importFrom stats integrate optim
#' @importFrom nloptr nloptr
#'
#' @export

tabulation_fit <- function(p, threshold, average, bracketshare=NULL, topshare=NULL,
                           bracketavg=NULL, topavg=NULL, invpareto=NULL,
                           bottom_model=NULL, lower_bound=0) {
    # Check and clean the input
    input <- clean_input(p, threshold, average, bracketshare, topshare,
        bracketavg, topavg, invpareto, bottom_model, lower_bound)

    p            <- input$p
    m            <- input$m
    threshold    <- input$threshold
    bottom_model <- input$bottom_model
    lower_bound  <- input$lower_bound
    n            <- input$n

    # Log-transform of the data
    pk <- p
    qk <- threshold
    mk <- m
    xk <- -log(1 - pk)
    yk <- -log(mk)
    sk <- (1 - pk)*qk/mk

    # Estimate the second derivative at the last point
    an <- (sk[n] - sk[n - 1])/(xk[n] - xk[n - 1])

    # Calculate the second derivative
    ak <- clamped_quintic_spline(xk, yk, sk, an)

    # Keep non-constrained parameter values in memory
    xk_nc <- xk
    yk_nc <- yk
    sk_nc <- sk
    ak_nc <- ak

    # Enforce monotonicity constraint at the interpolation points
    ak <- ifelse(ak + sk*(1 - sk) < 0, -sk*(1 - sk), ak)

    # Enforce monotonicity constraint over the entire function
    use_hist <- NULL
    fk_cns <- NULL
    pk_cns <- pk[1]
    qk_cns <- qk[1]
    mk_cns <- mk[1]
    xk_cns <- xk[1]
    yk_cns <- yk[1]
    sk_cns <- sk[1]
    ak_cns <- ak[1]
    for (i in 1:(n - 1)) {
        x0 <- xk[i]
        x1 <- xk[i + 1]
        y0 <- yk[i]
        y1 <- yk[i + 1]
        s0 <- sk[i]
        s1 <- sk[i + 1]
        a0 <- ak[i]
        a1 <- ak[i + 1]
        p0 <- pk[i]
        p1 <- pk[i + 1]
        q0 <- qk[i]
        q1 <- qk[i + 1]
        m0 <- mk[i]
        m1 <- mk[i + 1]
        bracketavg <- (m0 - m1)/(p1 - p0)
        # Check if constraint is violated
        if (!is_increasing(x0, x1, y0, y1, s0, s1, a0, a1)) {
            # First, try to enforce the constraint by adding one point
            new_pt <- add_one_point(x0, x1, y0, y1, s0, s1, a0, a1)
            # Check that the algorithm converged
            if (!is.null(new_pt)) {
                # Check that the constraint is now satisfied
                cond <- is_increasing(
                    x0, new_pt$x_new,
                    y0, new_pt$y_new,
                    s0, new_pt$s_new,
                    a0, new_pt$a_new
                ) & is_increasing(
                    new_pt$x_new, x1,
                    new_pt$y_new, y1,
                    new_pt$s_new, s1,
                    new_pt$a_new, a1
                )
                if (cond) {
                    # If so, add the point and move on to the next bracket
                    use_hist <- c(use_hist, FALSE, FALSE)
                    fk_cns <- c(fk_cns, NA, NA)
                    pk_cns <- c(pk_cns, 1 - exp(-new_pt$x_new), p1)
                    mk_cns <- c(mk_cns, exp(-new_pt$y_new), m1)
                    qk_cns <- c(qk_cns, new_pt$s_new*exp(new_pt$x_new - new_pt$y_new), q1)
                    xk_cns <- c(xk_cns, new_pt$x_new, x1)
                    yk_cns <- c(yk_cns, new_pt$y_new, y1)
                    sk_cns <- c(sk_cns, new_pt$s_new, s1)
                    ak_cns <- c(ak_cns, new_pt$a_new, a1)
                    next
                }
            }

            # If adding one point failed, try adding two points
            new_pts <- add_two_points(x0, x1, y0, y1, s0, s1, a0, a1)
            # Check that the algorithm converged
            if (!is.null(new_pts)) {
                # Check that the constraint is now satisfied
                cond <- is_increasing(
                    x0, new_pts$x_new1,
                    y0, new_pts$y_new1,
                    s0, new_pts$s_new1,
                    a0, new_pts$a_new1
                ) & is_increasing(
                    new_pts$x_new1, new_pts$x_new2,
                    new_pts$y_new1, new_pts$y_new2,
                    new_pts$s_new1, new_pts$s_new2,
                    new_pts$a_new1, new_pts$a_new2
                ) & is_increasing(
                    new_pts$x_new2, x1,
                    new_pts$y_new2, y1,
                    new_pts$s_new2, s1,
                    new_pts$a_new2, a1
                )
                if (cond) {
                    # If so, add the two new points and move on to the next bracket
                    use_hist <- c(use_hist, FALSE, FALSE, FALSE)
                    fk_cns <- c(fk_cns, NA, NA, NA)
                    pk_cns <- c(
                        pk_cns,
                        1 - exp(-new_pts$x_new1),
                        1 - exp(-new_pts$x_new2),
                        p1
                    )
                    qk_cns <- c(
                        qk_cns,
                        new_pts$s_new1*exp(new_pts$x_new1 - new_pts$y_new1),
                        new_pts$s_new2*exp(new_pts$x_new2 - new_pts$y_new2),
                        q1
                    )
                    mk_cns <- c(mk_cns, exp(-new_pts$y_new1), exp(-new_pts$y_new2), m1)
                    xk_cns <- c(xk_cns, new_pts$x_new1, new_pts$x_new2, x1)
                    yk_cns <- c(yk_cns, new_pts$y_new1, new_pts$y_new2, y1)
                    sk_cns <- c(sk_cns, new_pts$s_new1, new_pts$s_new2, s1)
                    ak_cns <- c(ak_cns, new_pts$a_new1, new_pts$a_new2, a1)
                    next
                }
            }

            # If adding two points also failed, we fall back to an histogram density
            use_hist <- c(use_hist, TRUE, TRUE)
            hist <- hist_interpol(p0, p1, q0, q1, bracketavg)
            fk_cns <- c(fk_cns, hist$f0, hist$f1)
            pk_cns <- c(pk_cns, hist$pstar, p1)
            qk_cns <- c(qk_cns, hist$qstar, q1)
            mk_cns <- c(mk_cns,
                hist_lorenz(hist$pstar,
                    hist$pstar, p1,
                    hist$qstar, q1,
                    m1, hist$f1
                ),
                m1
            )
            xk_cns <- c(xk_cns, -log(1 - hist$pstar), x1)
            yk_cns <- c(yk_cns, NA, y1)
            sk_cns <- c(sk_cns, NA, s1)
            ak_cns <- c(ak_cns, NA, a1)
        } else {
            # Leave the coefficients untouched if the constraint isn't violated
            use_hist <- c(use_hist, FALSE)
            fk_cns <- c(fk_cns, NA)
            pk_cns <- c(pk_cns, p1)
            qk_cns <- c(qk_cns, q1)
            mk_cns <- c(mk_cns, m1)
            xk_cns <- c(xk_cns, x1)
            yk_cns <- c(yk_cns, y1)
            sk_cns <- c(sk_cns, s1)
            ak_cns <- c(ak_cns, a1)
        }
    }

    # Estimate the parameters of the generalized Pareto distribution at the top
    param_top <- gpd_top_parameters(xk[n], yk[n], sk[n], ak[n])
    # If we get xi >= 1 or sigma <= 0, the GPD is not valid, so use a
    # standard Pareto instead (which breaks derivability of the quantile
    # function)
    if (param_top$sigma <= 0 || param_top$xi >= 1) {
        param_top$mu_top    <- qk[n]
        param_top$xi_top    <- 1 - sk[n]
        param_top$sigma_top <- param_top$mu_top*param_top$xi_top
    }

    # Estimate the parameters of the model for the bottom
    bracketavg <- (average - mk_cns[1])/pk_cns[1] # Estimate the average in the bottom
    if (p[1] > 0) {
        if (bottom_model == "gpd" && bracketavg > 0) {
            q1 <- qk[1]
            param_bottom <- list(
                mu_bottom    = q1,
                sigma_bottom = ((bracketavg - q1)*(lower_bound - q1))/(bracketavg - lower_bound),
                xi_bottom    = (bracketavg - q1)/(bracketavg - lower_bound),
                delta        = NA
            )
        } else if (bottom_model == "gpd" || bracketavg <= 0) {
            param_bottom <- gpd_bottom_parameters(xk[1], yk[1], sk[1], ak[1], average)
            # Same thing as for the top if xi >= 1 or sigma <= 0
            if (param_bottom$sigma <= 0 || param_bottom$xi >= 1) {
                param_bottom$mu_bottom <- qk[n]
                param_bottom$xi_bottom <- 1 - sk[n]
                param_bottom$sigma_bottom <- param_bottom$mu_top*param_bottom$xi_top
            }
            param_bottom$delta <- NA
        } else if (bottom_model == "dirac") {
            param_bottom <- list(mu=NA, sigma=NA, xi=NA, delta=p[1])
        } else if (bottom_model == "hist") {
            use_hist <- c(TRUE, TRUE, use_hist)
            hist <- hist_interpol(0, pk_cns[1], lower_bound, qk_cns[1], bracketavg)
            fk_cns <- c(hist$f0, hist$f1, fk_cns)
            pk_cns <- c(0, hist$pstar, pk_cns)
            qk_cns <- c(lower_bound, hist$qstar, qk_cns)
            mk_cns <- c(
                average,
                hist_lorenz(hist$pstar,
                    hist$pstar, hist$p1,
                    hist$qstar, hist$q1,
                    mk_cns[1], hist$f1
                ),
                mk_cns
            )
            xk_cns <- c(0, -log(1 - hist$pstar), xk_cns)
            yk_cns <- c(-log(average), NA, yk_cns)
            sk_cns <- c(lower_bound/average, NA, sk_cns)
            ak_cns <- c(NA, NA, ak_cns)

            param_bottom <- list(mu=NA, sigma=NA, xi=NA, delta=NA)
        }
    } else {
        param_bottom <- list(mu=NA, sigma=NA, xi=NA, delta=NA)
    }

    # Object to return
    result <- list()
    class(result) <- c("gpinter_dist_orig", "gpinter_dist")

    result$use_hist <- use_hist

    result$pk <- pk_cns
    result$qk <- qk_cns
    result$xk <- xk_cns
    result$yk <- yk_cns
    result$sk <- sk_cns
    result$ak <- ak_cns
    result$fk <- fk_cns
    result$mk <- mk_cns

    result$pk_nc <- p
    result$xk_nc <- xk_nc
    result$yk_nc <- yk_nc
    result$sk_nc <- sk_nc
    result$ak_nc <- ak_nc
    result$qk_nc <- threshold
    result$mk_nc <- m
    result$bk_nc <- m/((1 - p)*threshold)

    result$average <- average

    # Estimate the parameters of the generalized Pareto distribution at the top
    param_top <- gpd_top_parameters(xk[n], yk[n], sk[n], ak[n])
    # If we get xi == 1 or sigma == 0, the GPD is not valid, so use a
    # standard Pareto instead (which breaks derivability of the quantile
    # function)
    if (param_top$sigma == 0 || param_top$xi == 1) {
        result$mu_top    <- qk[n]
        result$xi_top    <- 1 - sk[n]
        result$sigma_top <- result$mu_top*result$xi_top
    } else {
        result$mu_top    <- param_top$mu
        result$sigma_top <- param_top$sigma
        result$xi_top    <- param_top$xi
    }

    result$mu_bottom    <- param_bottom$mu
    result$sigma_bottom <- param_bottom$sigma
    result$xi_bottom    <- param_bottom$xi
    result$delta_bottom <- param_bottom$delta

    return(result)
}
