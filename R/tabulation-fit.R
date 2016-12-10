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
#'
#' @return An object of class \code{gpinter_dist_orig}.
#'
#' @importFrom stats integrate optim
#' @importFrom nloptr nloptr
#'
#' @export

tabulation_fit <- function(p, threshold, average, bracketshare=NULL, topshare=NULL,
                           bracketavg=NULL, topavg=NULL, invpareto=NULL) {
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
    } else if (!is.null(invpareto)) {
        if (length(invpareto) != n) {
            stop("'p' and 'invpareto' must have the same length.")
        }
        invpareto <- invpareto[ord]
        m <- (1 - p)*threshold*invpareto
        # The inverted Pareto may not be defined for the first threshold
        if (is.na(invpareto[1]) & p[1] == 0) {
            m[1] <- average
        }
    } else {
        stop("You must specify one of 'bracketshare', 'topshare', 'bracketavg', 'topavg' or 'invpareto'.")
    }

    # Sanity check of the data
    # Quantile function is increasing
    if (any(diff(threshold) <= 0)) {
        stop("Thresholds must be strictly increasing.")
    }
    if (any(diff(m) >= 0)) {
        stop("Truncated average must be strictly decreasing.")
    }
    # Truncated mean function is concave
    for (i in 2:(n - 1)) {
        chord <- m[i - 1] + (m[i + 1] - m[i - 1])*(p[i] - p[i - 1])/(p[i +1] - p[i - 1])
        if (m[i] < chord) {
            stop("Truncated average must be concave.")
        }
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
    pk <- p
    qk <- threshold
    mk <- m
    xk <- -log(1 - pk)
    yk <- -log(mk)
    sk <- (1 - pk)*qk/mk

    # Calculate the second derivative
    ak <- natural_quintic_spline(xk, yk, sk)

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

    return(result)
}
