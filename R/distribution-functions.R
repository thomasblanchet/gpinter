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
        gpd_bottom_phi(x, 1 - exp(-x1), y1, dist$mu_bottom, dist$sigma_bottom, dist$xi_bottom)
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
        gpd_bottom_deriv_phi(x, 1 - exp(-x1), y1, dist$mu_bottom, dist$sigma_bottom, dist$xi_bottom)
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

#' @title Cumulative density function for generalized Pareto interpolation
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Compute the value of the cumulative density function for a
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
                upper = dist$pk[j + 1]
            )$root)
        }
    }))
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
        x <- -log(1 - p)
        y <- quintic_spline(x, dist$xk, dist$yk, dist$sk, dist$ak)
        dydx <- deriv_quintic_spline(x, dist$xk, dist$yk, dist$sk, dist$ak)
        d2ydx2 <- deriv2_quintic_spline(x, dist$xk, dist$yk, dist$sk, dist$ak)

        1/(exp(2*x - y)*(d2ydx2 + dydx*(1 - dydx)))
    })))))
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
    if (dist$xi_top > 0) {
        b[p == 1] <- 1/(1 - dist$xi_top)
    } else {
        b[p == 1] <- 1
    }
    return(b)
}
