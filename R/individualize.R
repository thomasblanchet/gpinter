#' @title Individualize a distribution
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Individualize the distribution (of income or wealth) under
#' equal splitting among spouses, given the share of couples or singles
#' at different points of the distribution.
#'
#' @param dist An object of class \code{gpinter_dist_orig}.
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

    # Check the class of input distribution
    if (!is(dist, "gpinter_dist_orig")) {
        stop("'dist' objects must be of class 'gpinter_dist_orig'")
    }

    if (any(p < 0) || any(p >= 1)) {
        stop("'p' must be between 0 and 1.")
    }
    # Order inputs
    ord <- order(p)
    p <- p[ord]

    if (!is.null(singleshare) && !is.na(singleshare)) {
        m <- 1 - singleshare
    } else if (!is.null(coupleshare) && !is.na(coupleshare)) {
        m <- coupleshare
    } else if (p[1] != 0) {
        stop("You must either specify 'singleshare' or 'coupleshare' if min(p) != 0.")
    }

    if (!is.null(singletop)) {
        singletop <- singletop[ord]
        ck <- 1 - singletop
        ck <- (1 - p)*ck
        if (p[1] != 0) {
            p <- c(0, p)
            ck <- c(m, ck)
        }
        ck <- -diff(c(ck, 0))/diff(c(p, 1))
    } else if (!is.null(coupletop)) {
        coupletop <- coupletop[ord]
        ck <- coupletop
        ck <- (1 - p)*ck
        if (p[1] != 0) {
            p <- c(0, p)
            ck <- c(m, ck)
        }
        ck <- -diff(c(ck, 0))/diff(c(p, 1))
    } else if (!is.null(singlebracket)) {
        singlebracket <- singlebracket[ord]
        ck <- 1 - singlebracket
        if (p[1] != 0) {
            ck <- c((m - sum(ck*diff(c(p, 1))))/p[1], ck)
            p <- c(0, p)
        }
    } else if (!is.null(couplebracket)) {
        couplebracket <- couplebracket[ord]
        ck <- couplebracket
        if (p[1] != 0) {
            ck <- c((m - sum(ck*diff(c(p, 1))))/p[1], ck)
            p <- c(0, p)
        }
    } else {
        stop("You must specify one of 'singletop', 'coupletop', 'singlebracket' or 'couplebracket'.")
    }
    # Re-calculate m in case it was not specified
    m <- sum(ck*diff(c(p, 1)))

    if (any(ck >= 1) || any(ck < 0)) {
        stop("The share of couples must be between 0 and 1.")
    }

    # Return an object with the parent distribution and the interpolated couple
    # share
    new_dist <- list()
    class(new_dist) <- c("gpinter_dist", "gpinter_dist_indiv")

    new_dist$average <- dist$average/(1 + m)
    new_dist$parent <- dist
    new_dist$couple_share <- m
    new_dist$pk <- p
    new_dist$ck <- ck

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
deriv_couple_share <- function(dist, p, ...) UseMethod("deriv_couple_share")

#' @export
couple_share.gpinter_dist_indiv <- function(dist, p, ...) {
    # Find the bracket in which p falls
    pk <- c(dist$pk, 1)
    k <- cut(p, breaks=pk, include.lowest=TRUE, labels=FALSE)

    # Mass of couples above the current bracket
    a <- rev(cumsum(rev(dist$ck*diff(pk))))
    a <- c(a[2:length(a)], 0)

    # Mass of couples above p in the current bracket
    b <- (pk[k + 1] - p)*dist$ck[k]

    c <- (b + a[k])/(1 - p)
    # Extend by continuity to p = 1
    c[p == 1] <- tail(dist$ck, n=1)

    return(c)
}

#' @export
deriv_couple_share.gpinter_dist_indiv <- function(dist, p, ...) {
    # Find the bracket in which p falls
    k <- cut(p, breaks=c(dist$pk, 1), include.lowest=TRUE, labels=FALSE)

    return(-dist$ck[k])
}
