#' @title Interpolation function from generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Full interpolation function. It includes the part of the
#' distribution estimated by spline interpolation, and the parts estimated
#' via the generalized Pareto distribution. This method can only be applied
#' to objects of class \code{gpinter_dist_orig}, which are returned by
#' \code{tabulation_fit} and \code{share_fit}.
#'
#' @details If \eqn{X} follows the distribution that was interpolated using
#' \code{tabulation_fit} or \code{share_fit}, then the interpolation function
#' correspond to:
#' \deqn{p \mapsto -\log\left(\int_{1 - \exp(-x)}^{1} Q(u) du\right)}
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param x The function evaluation point(s).
#' @param ... Ignored.
#'
#' @return The value of the interpolation function at \code{x}.
#'
#' @importFrom utils tail head
#'
#' @export

phi <- function(dist, x, ...) UseMethod("phi")

#' @export
phi.gpinter_dist_orig <- function(dist, x, ...) {
    xn <- tail(dist$xk, n=1)
    x1 <- head(dist$xk, n=1)
    y1 <- head(dist$yk, n=1)

    # Identify the bracket for each value of x
    k <- cut(x, breaks=dist$xk, labels=FALSE, include.lowest=TRUE)

    return(ifelse(x > xn, {
        gpd_top_phi(x, 1 - exp(-xn), dist$mu_top, dist$sigma_top, dist$xi_top)
    }, ifelse(x < x1, {
        if (is.na(dist$delta_bottom)) {
            gpd_bottom_phi(x, 1 - exp(-x1), y1, dist$mu_bottom, dist$sigma_bottom, dist$xi_bottom)
        } else {
            y1
        }
    }, ifelse(dist$use_hist[k], {
        hist_phi(x,
            dist$pk[k], dist$pk[k + 1],
            dist$qk[k], dist$qk[k + 1],
            dist$mk[k + 1], dist$fk[k]
        )
    }, {
        h(x,
            dist$xk[k], dist$xk[k + 1],
            dist$yk[k], dist$yk[k + 1],
            dist$sk[k], dist$sk[k + 1],
            dist$ak[k], dist$ak[k + 1]
        )
    }))))
}

#' @export
phi.gpinter_dist <- function(dist, x, ...) {
    p <- 1 - exp(-x)
    return(-log(dist$average*top_share(dist, p)))
}

#' @title Derivative of the interpolation function from generalized Pareto
#' interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description This function is the first derivative of \code{phi} applied
#' to objects of class \code{gpinter_dist_orig}.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param x The function evaluation point(s).
#' @param ... Ignored.
#'
#' @return The value of the derivative of the interpolation at \code{x}.
#'
#' @importFrom utils tail head
#'
#' @export

deriv_phi <- function(dist, x, ...) UseMethod("deriv_phi")

#' @export
deriv_phi.gpinter_dist_orig <- function(dist, x, ...) {
    xn <- tail(dist$xk, n=1)
    x1 <- head(dist$xk, n=1)
    y1 <- head(dist$yk, n=1)

    # Identify the bracket for each value of x
    k <- cut(x, breaks=dist$xk, labels=FALSE, include.lowest=TRUE)

    return(ifelse(x > xn, {
        gpd_top_deriv_phi(x, 1 - exp(-xn), dist$mu_top, dist$sigma_top, dist$xi_top)
    }, ifelse(x < x1, {
        if (is.na(dist$delta_bottom)) {
            gpd_bottom_deriv_phi(x, 1 - exp(-x1), y1, dist$mu_bottom, dist$sigma_bottom, dist$xi_bottom)
        } else {
            0
        }
    }, ifelse(dist$use_hist[k], {
        hist_deriv_phi(x,
            dist$pk[k], dist$pk[k + 1],
            dist$qk[k], dist$qk[k + 1],
            dist$mk[k + 1], dist$fk[k]
        )
    }, {
        hd1(x,
            dist$xk[k], dist$xk[k + 1],
            dist$yk[k], dist$yk[k + 1],
            dist$sk[k], dist$sk[k + 1],
            dist$ak[k], dist$ak[k + 1]
        )
    }))))
}

#' @export
deriv_phi.gpinter_dist <- function(dist, x, ...) {
    p <- 1 - exp(-x)
    q <- fitted_quantile(dist, p)
    m <- dist$average*threshold_share(dist, q)/(1 - p)
    return(q/m)
}

#' @title Quantile function for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the quantile estimated via generalized
#' Pareto interpolation, for any value in [0, 1].
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param probs A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @export

fitted_quantile <- function(dist, probs, ...) UseMethod("fitted_quantile")

#' @export
fitted_quantile.gpinter_dist_orig <- function(dist, probs, ...) {
    # Identify the bracket for each value of probs
    k <- cut(probs, breaks=dist$pk, labels=FALSE, include.lowest=TRUE)

    x <- -log(1 - probs)
    y <- phi(dist, x)
    dydx <- deriv_phi(dist, x)
    return(exp(x - y)*dydx)
}

#' @export
fitted_quantile.gpinter_dist_qmc <- function(dist, probs, ...) {
    k <- cut(probs, breaks=dist$cw, include.lowest=TRUE, labels=FALSE)
    q <- dist$x[k]

    if (any(probs == 0) || any(probs == 1)) {
        supp <- support(dist)
        q[probs == 0] <- supp$lower
        q[probs == 1] <- supp$upper
    }

    return(q)
}

#' @export
fitted_quantile.gpinter_dist <- function(dist, probs, ...) {
    supp <- support(dist)
    if (is.finite(supp$lower)) {
        lb <- supp$lower
    } else {
        lb <- 0
    }

    if (is.finite(supp$upper)) {
        ub <- supp$upper
    } else {
        ub <- 10*dist$average
    }

    # Invert the CDF
    return(sapply(probs, function(p) {
        if (p == 0) (
            return(supp$lower)
        ) else if (p == 1) {
            return(supp$upper)
        } else {
            eq <- uniroot(function(q) {
                return(fitted_cdf(dist, q) - p)
            }, lower=lb, upper=ub, extendInt="upX", tol=sqrt(.Machine$double.eps))

            return(eq$root)
        }
    }))
}

#' @title Support of a distribution estimated via generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Give the support of a distribution estimated via generalized
#' Pareto distribution.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param ... Ignored.
#'
#' @return A list with two components:
#' \describe{
#'     \item{lower}{The lower bound of the distribution (possibly \code{-Inf}).}
#'     \item{upper}{The upper bound of the distribution (possibly \code{+Inf}).}
#' }
#'
#' @export

support <- function(dist, ...) UseMethod("support")

#' @export
support.gpinter_dist_orig <- function(dist, ...) {
    # The generalized Pareto distribution has finite support if xi < 0
    if (dist$pk[1] == 0) {
        lower <- dist$qk[1]
    } else if (!is.na(dist$delta_bottom)) {
        lower <- 0
    } else if (dist$xi_bottom < 0) {
        lower <- dist$mu_bottom + dist$sigma_bottom/dist$xi_bottom
    } else {
        lower <- -Inf
    }
    if (dist$xi_top < 0) {
        upper <- dist$mu_top - dist$sigma_top/dist$xi_top
    } else {
        upper <- +Inf
    }

    return(list(lower=lower, upper=upper))
}

#' @export
support.gpinter_dist_merge <- function(dist, ...) {
    bounds <- sapply(dist$parent_dist, function(dist) {
        support <- support(dist)
        return(c(support$lower, support$upper))
    })
    # Take the lowest lower bound and the highest upper bound
    return(list(lower=min(bounds[1, ]), upper=max(bounds[2, ])))
}

#' @export
support.gpinter_dist_indiv <- function(dist, ...) {
    support_orig <- support(dist$singles$dist)
    return(list(
        lower = min(support_orig$lower/2, support_orig$lower),
        upper = support_orig$upper
    ))
}

#' @export
support.gpinter_dist_addup <- function(dist, ...) {
    supp1 <- support(dist$parent1)
    supp2 <- support(dist$parent2)

    return(list(
        lower = supp1$lower + supp2$lower,
        upper = supp1$upper + supp2$upper
    ))
}

#' @title Cumulative distribution function for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the cumulative distribution function for a
#' distribution estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param x The evaluation point(s).
#' @param ... Ignored.
#'
#' @return The value of the CDF at \code{x}.
#'
#' @importFrom stats uniroot
#'
#' @export

fitted_cdf <- function(dist, x, ...) UseMethod("fitted_cdf")

#' @export
fitted_cdf.gpinter_dist_orig <- function(dist, x, ...) {
    n <- length(dist$pk)
    supp <- support(dist)

    if (length(x) == 0) {
        return(numeric(0))
    }

    # Identify the bracket for each value of x
    k <- cut(x, breaks=dist$qk, labels=FALSE, include.lowest=TRUE)

    return(sapply(seq_along(x), function(i) {
        q <- x[i]
        j <- k[i]

        if (q <= supp$lower) {
            # We are below the support: CDF is zero by definition
            return(0)
        } else if (q >= supp$upper) {
            # We are above the support: CDF is one by definition
            return(1)
        } else if (q <= dist$qk[1]) {
            # GPD model for the bottom part
            return(gpd_bottom_cdf(q, dist$pk[1], dist$mu_bottom, dist$sigma_bottom, dist$xi_bottom))
        } else if (q >= dist$qk[n]) {
            # GPD model for the top part
            return(gpd_top_cdf(q, dist$pk[n], dist$mu_top, dist$sigma_top, dist$xi_top))
        } else if (dist$use_hist[j]) {
            # Histogram density: we have an explicit formula for the CDF
            return(hist_cdf(q, dist$pk[j], dist$qk[j], dist$fk[j]))
        } else if (q == dist$qk[j]) {
            # We are exactly at the lower bound of the bracket
            return(dist$pk[j])
        } else if (q == dist$qk[j + 1]) {
            # We are exactly at the upper bound
            return(dist$pk[j + 1])
        } else {
            # Pareto interpolation: numeric inversion of the quantile function
            # in the relevant bracket
            return(uniroot(
                f = function(p) {
                    x <- -log(1 - p)
                    y <- h(x,
                        dist$xk[j], dist$xk[j + 1],
                        dist$yk[j], dist$yk[j + 1],
                        dist$sk[j], dist$sk[j + 1],
                        dist$ak[j], dist$ak[j + 1]
                    )
                    dydx <- hd1(x,
                        dist$xk[j], dist$xk[j + 1],
                        dist$yk[j], dist$yk[j + 1],
                        dist$sk[j], dist$sk[j + 1],
                        dist$ak[j], dist$ak[j + 1]
                    )
                    return(exp(x - y)*dydx - q)
                },
                lower = dist$pk[j],
                upper = dist$pk[j + 1],
                tol = sqrt(.Machine$double.eps)
            )$root)
        }
    }))
}

#' @export
fitted_cdf.gpinter_dist_qmc <- function(dist, x, ...) {
    sumw <- sum(dist$w)
    return(sapply(x, function(q) {
        return(sum(dist$w[dist$x <= q])/sumw)
    }))
}

#' @export
fitted_cdf.gpinter_dist_merge <- function(dist, x, ...) {
    # Calculate the CDF for each parent distribution
    cdfs <- t(sapply(dist$parent_dist, function(dist) fitted_cdf(dist, x)))
    # Return mean weighted by population sizes
    if (length(x) == 0) {
        return(numeric(0))
    } else if (length(x) == 1) {
        return(sum(cdfs*dist$relsize))
    } else {
        return(colSums(cdfs*dist$relsize))
    }
}

#' @export
fitted_cdf.gpinter_dist_indiv <- function(dist, x, ...) {
    # Get the fractile associated to x (for singles) and 2*x (for couples)
    p1 <- fitted_cdf(dist$singles$dist, x)
    p2 <- fitted_cdf(dist$couples$dist, 2*x)

    return(((1 - dist$couple_share)*p1 + 2*dist$couple_share*p2)/(1 + dist$couple_share))
}

#' @title Probability density function for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the density for a distribution estimated
#' via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param x The density evaluation point(s).
#' @param ... Ignored.
#'
#' @return The value of the density at \code{x}.
#'
#' @importFrom utils tail head
#' @importFrom stats density
#'
#' @export

fitted_density <- function(dist, x, ...) UseMethod("fitted_density")

#' @export
fitted_density.gpinter_dist_orig <- function(dist, x, ...) {
    n <- length(dist$pk)
    xn <- tail(dist$xk, n=1)
    x1 <- head(dist$xk, n=1)
    y1 <- head(dist$yk, n=1)

    supp <- support(dist)

    # Identify the bracket for each value of x
    k <- cut(x, breaks=dist$qk, labels=FALSE, include.lowest=TRUE)

    # Calculate the density
    return(ifelse((x <= supp$lower) | (x >= supp$upper), {
        # We are outside of the distribution support
        0
    }, ifelse(x >= dist$qk[n], {
        # We are in the top part of the distribution (GPD model)
        gpd_top_density(x, dist$pk[n], dist$mu_top, dist$sigma_top, dist$xi_top)
    }, ifelse(x <= dist$qk[1], {
        # We are in the bottom part of the distribution (GPD model)
        gpd_bottom_density(x, dist$pk[1], dist$mu_bottom, dist$sigma_bottom, dist$xi_bottom)
    }, ifelse(dist$use_hist[k], {
        # We are in the interpolation region, but with histogram (constant)
        # density
        dist$fk[k]
    }, {
        # We are in the interpolation region, with Pareto interpolation
        # Get the value of the probability associated to the quantile
        p <- fitted_cdf(dist, x)
        z <- -log(1 - p)
        y <- h(z,
            dist$xk[k], dist$xk[k + 1],
            dist$yk[k], dist$yk[k + 1],
            dist$sk[k], dist$sk[k + 1],
            dist$ak[k], dist$ak[k + 1]
        )
        dydx <- hd1(z,
            dist$xk[k], dist$xk[k + 1],
            dist$yk[k], dist$yk[k + 1],
            dist$sk[k], dist$sk[k + 1],
            dist$ak[k], dist$ak[k + 1]
        )
        d2ydx2 <- hd2(z,
            dist$xk[k], dist$xk[k + 1],
            dist$yk[k], dist$yk[k + 1],
            dist$sk[k], dist$sk[k + 1],
            dist$ak[k], dist$ak[k + 1]
        )

        1/(exp(2*z - y)*(d2ydx2 + dydx*(1 - dydx)))
    })))))
}

#' @export
fitted_density.gpinter_dist_merge <- function(dist, x, ...) {
    # Calculate the PDF for each parent distribution
    pdfs <- t(sapply(dist$parent_dist, function(parent) fitted_density(parent, x)))
    # Return mean weighted by population sizes
    if (length(x) == 1) {
        return(sum(pdfs*dist$relsize))
    } else {
        return(colSums(pdfs*dist$relsize))
    }
}

#' @export
fitted_density.gpinter_dist_indiv <- function(dist, x, ...) {
    f1 <- fitted_density(dist$singles$dist, x)
    f2 <- fitted_density(dist$couples$dist, 2*x)

    return(((1 - dist$couple_share)*f1 + 4*dist$couple_share*f2)/(1 + dist$couple_share))
}

#' @title Share above a threshold
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculate the share a threshold. It is mathematically equivalent
#' to \code{top_share(dist, fitted_cdf(dist, q))}, but can be much faster.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param q A vector of real numbers.
#' @param ... Ignored.
#'
#' @export

threshold_share <- function(dist, q, ...) UseMethod("threshold_share")

#' @export
threshold_share.gpinter_dist_orig <- function(dist, q, ...) {
    p <- fitted_cdf(dist, q)
    return(top_share(dist, p))
}

#' @export
threshold_share.gpinter_dist_qmc <- function(dist, q, ...) {
    total <- sum(dist$x*dist$w)
    return(sapply(q, function(q) {
        subset <- (dist$x >= q)
        return(sum(dist$x[subset]*dist$w[subset])/total)
    }))
}

#' @export
threshold_share.gpinter_dist_merge <- function(dist, q, ...) {
    # Calculate the truncated means for individual distributions
    truncmeans <- t(sapply(dist$parent_dist, function(parent) parent$average*threshold_share(parent, q)))
    if (length(q) == 0) {
        return(numeric(0))
    } else if (length(q) == 1) {
        return(sum(truncmeans*dist$relsize/dist$average))
    } else {
        return(colSums(truncmeans*dist$relsize/dist$average))
    }
}

#' @export
threshold_share.gpinter_dist_indiv <- function(dist, q, ...) {
    # Overall singles income share
    overall_single_share <- (1 - dist$couple_share)*dist$singles$average/((1 + dist$couple_share)*dist$average)
    # Overall couples income share
    overall_couple_share <- dist$couple_share*dist$couples$average/((1 + dist$couple_share)*dist$average)

    # Share of singles income above q
    top_single_share <- threshold_share(dist$singles$dist, q)
    # Share of couples income above 2*q
    top_couple_share <- threshold_share(dist$couples$dist, 2*q)

    return(overall_single_share*top_single_share + overall_couple_share*top_couple_share)
}

#' @title Top share for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the top share for a distribution estimated
#' via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @return The value of the share of the top 100*(1 - p)%.
#'
#' @export

top_share <- function(dist, p, ...) UseMethod("top_share")

#' @export
top_share.gpinter_dist_orig <- function(dist, p, ...) {
    x <- -log(1 - p)
    y <- phi(dist, x)
    ts <- exp(-y)/dist$average
    ts[p == 1] <- 0
    ts[p == 0] <- 1
    return(ts)
}

#' @export
top_share.gpinter_dist <- function(dist, p, ...) {
    q <- fitted_quantile(dist, p)
    return(threshold_share(dist, q))
}

#' @title Bottom share for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the bottom share for a distribution estimated
#' via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @return The value of the share of the bottom 100*p%.
#'
#' @export

bottom_share <- function(dist, p, ...) UseMethod("bottom_share")

#' @export
bottom_share.gpinter_dist <- function(dist, p, ...) {
    return(1 - top_share(dist, p))
}

#' @title Bracket share for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the share of the bracket [\code{p1}, \code{p2}]
#' for a distribution estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p1 A vector of probabilities in [0, 1].
#' @param p2 A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @return The value of the share of the bracket [100*p1%, 100*p2%].
#'
#' @export

bracket_share <- function(dist, p1, p2, ...) UseMethod("bracket_share")

#' @export
bracket_share.gpinter_dist <- function(dist, p1, p2, ...) {
    return(top_share(dist, p1) - top_share(dist, p2))
}

#' @title Top average for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the average above the \code{p}-th quantile for a distribution
#' estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @return The value of the average of the top 100*(1 - p)%.
#'
#' @export

top_average <- function(dist, p, ...) UseMethod("top_average")

#' @export
top_average.gpinter_dist <- function(dist, p, ...) {
    return(dist$average*top_share(dist, p)/(1 - p))
}

#' @title Bottom average for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the average below the \code{p}-th quantile for a distribution
#' estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @return The value of the average of the bottom 100*p%.
#'
#' @export

bottom_average <- function(dist, p, ...) UseMethod("bottom_average")

#' @export
bottom_average.gpinter_dist <- function(dist, p, ...) {
    return(dist$average*bottom_share(dist, p)/p)
}

#' @title Bracket average for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the average between the \code{p1}-th and \code{p2}-th
#' quantiles for a distribution estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p1 A vector of probabilities in [0, 1].
#' @param p2 A vector of probabilities in [0, 1], same length as \code{p1}, with \code{p2 > p1}.
#' @param ... Ignored.
#'
#' @return The value of the average in the bracket the bracket [100*p1%, 100*p2%].
#'
#' @export

bracket_average <- function(dist, p1, p2, ...) UseMethod("bracket_average")

#' @export
bracket_average.gpinter_dist <- function(dist, p1, p2, ...) {
    return(dist$average*bracket_share(dist, p1, p2)/(p2 - p1))
}

#' @title Inverted Pareto coefficients for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of inverted Pareto coefficients for a
#' distribution estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param p A vector of probabilities in [0, 1].
#' @param ... Ignored.
#'
#' @return The value of the inverted Pareto coefficient at \code{p}.
#'
#' @export

invpareto <- function(dist, p, ...) UseMethod("invpareto")

#' @export
invpareto.gpinter_dist <- function(dist, p, ...) {
    x <- -log(1 - p)
    dydx <- deriv_phi(dist, x)
    b <- 1/dydx
    return(b)
}

#' @title Gini coefficient
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the Gini coefficient for a distribution
#' estimated via generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param ... Ignored.
#'
#' @return The value of the inverted Pareto coefficient at \code{p}.
#'
#' @importFrom stats integrate
#'
#' @export

gini <- function(dist, ...) UseMethod("gini")

#' @export
gini.gpinter_dist <- function(dist, ...) {
    return(2*integrate(function(p) {
        return(p - bottom_share(dist, p))
    }, lower=0, upper=1, abs.tol=1e-4)$value)
}

#' @export
gini.gpinter_dist_qmc <- function(dist, ...) {
    p <- dist$cw[-1]
    l <- cumsum(dist$w * dist$x)
    n <- length(l)
    l <- l/l[n]

    return(sum(l[-1] * p[-n]) - sum(l[-n] * p[-1]))
}
