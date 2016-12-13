#' @title Individualize a distribution
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Individualize the distribution (of income or wealth) under
#' equal splitting among spouses, given the share of couples or singles
#' at different points of the distribution.
#'
#' @param dist An object of class \code{gpinter_dist}.
#' @param p A vector of fractiles in [0, 1].
#' @param singleshare The overall share of singles.
#' @param coupleshare The overall share of couples.
#' @param singletop A vector with the same length as \code{p}: the share
#' of singles in the top 100*(1 - p)\%.
#' @param coupletop A vector with the same length as \code{p}: the share
#' of couples in the top 100*(1 - p)\%.
#' @param singlebracket A vector with the same length as \code{p}: the share
#' of singles in the matching bracket.
#' @param couplebracket A vector with the same length as \code{p}: the share
#' of couples in the matching bracket.
#'
#' @return An object of class \code{gpinter_dist_indiv}.
#'
#' @export

individualize_dist <- function(dist, p, singleshare=NULL, coupleshare=NULL,
                               singletop=NULL, coupletop=NULL,
                               singlebracket=NULL, couplebracket=NULL) {

    if (any(p < 0) || any(p >= 1)) {
        stop("'p' must be between 0 and 1.")
    }
    # Order inputs
    ord <- order(p)
    p <- p[ord]

    if (!is.null(singleshare)) {
        lambda_all <- 1 - singleshare
    } else if (!is.null(coupleshare)) {
        lambda_all <- coupleshare
    } else {
        stop("You must either specify 'singleshare' or 'coupleshare'.")
    }

    if (lambda_all >= 1 || lambda_all < 0) {
        stop("'singleshare' and 'coupleshare' must be between 0 and 1.")
    }

    if (!is.null(singletop)) {
        singletop <- singletop[ord]
        lambda <- 1 - singletop
    } else if (!is.null(coupletop)) {
        coupletop <- coupletop[ord]
        lambda <- coupletop
    } else if (!is.null(singlebracket)) {
        singlebracket <- singlebracket[ord]
        lambda <- 1 - singlebracket
        lambda <- diff(c(p, 1))*lambda
        lambda <- rev(cumsum(rev(lambda)))
        lambda <- lambda/(1 - p)
    } else if (!is.null(couplebracket)) {
        couplebracket <- couplebracket[ord]
        lambda <- couplebracket
        lambda <- diff(c(p, 1))*lambda
        lambda <- rev(cumsum(rev(lambda)))
        lambda <- lambda/(1 - p)
    }

    if (any(lambda >= 1) || any(lambda < 0)) {
        stop("The share of couples must be between 0 and 1.")
    }

    # Add a value for p == 0 if necessary
    if (p[1] != 0) {
        p <- c(0, p)
        lambda <- c(lambda_all, lambda)
    }
    # Add a value for p == 1 assuming a constant share in the last threshold
    p <- c(p, 1)
    lambda <- c(lambda, tail(lambda, n=1))

    # Interpolate the share of couples via PCHIP
    n <- length(lambda)
    secant <- (lambda[2:n] - lambda[1:(n - 1)])/(p[2:n] - p[1:(n - 1)])
    tangent <- c(
        secant[1],
        (secant[1:(n - 2)] + secant[2:(n - 1)])/2,
        secant[n - 1]
    )

    for (k in 1:(n - 1)) {
        if (secant[k] == 0) {
            tangent[k] <- 0
            tangent[k + 1] <- 0
        } else if (k > 1 && secant[k - 1]*secant[k] < 0) {
            tangent[k] <- 0
        } else {
            alpha <- tangent[k]/secant[k]
            beta <- tangent[k + 1]/secant[k]
            if (alpha^2 + beta^2 > 9) {
                tau <- 3/sqrt(alpha^2 + beta^2)
                tangent[k] <- tau*tangent[k]
                tangent[k + 1] <- tau*tangent[k + 1]
            }
        }
    }

    # Return an object with the parent distribution and the interpolated couple
    # share
    new_dist <- list()
    class(new_dist) <- c("gpinter_dist", "gpinter_dist_indiv")

    new_dist$average <- dist$average/(1 + lambda_all)
    new_dist$parent <- dist
    new_dist$couple_share <- lambda_all
    new_dist$spline <- list(
        xk = p,
        yk = lambda,
        sk = tangent
    )

    return(new_dist)
}

#' @title Share of couples above a fractile
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Give the share of couples above any fractile \code{p} of the
#' parent distribution of an individualized distribution, interpolated
#' with PCHIP.
#'
#' @param dist An object of class \code{gpinter_dist_indiv}.
#' @param p A vector of fractiles.
#' @param ... Ignored.
#'
#' @return A vector with the share of couples above each value of \code{p}.
#'
#' @export

couple_share <- function(dist, p, ...) UseMethod("couple_share")

#' @export
couple_share.gpinter_dist_indiv <- function(dist, p, ...) {
    return(cubic_spline(p,
        dist$spline$xk,
        dist$spline$yk,
        dist$spline$sk
    ))
}
