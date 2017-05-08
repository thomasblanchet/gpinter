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
#' @param ratio A vector with the same length as \code{p}: the ratio of
#' singles average income over couples average income in each bracket. Default
#' is 1 for all brackets.
#'
#' @return An object of class \code{gpinter_dist_indiv}.
#'
#' @export

individualize_dist <- function(dist, p, singleshare=NULL, coupleshare=NULL,
                               singletop=NULL, coupletop=NULL,
                               singlebracket=NULL, couplebracket=NULL,
                               ratio=NULL) {

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

    has_zero <- (p[1] == 0)
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

    # Make a tabulation for singles and the couples
    p_singles <- c(0, cumsum((1 - ck)*diff(c(p, 1))/(1 - m)))[1:length(p)]
    p_couples <- c(0, cumsum(ck*diff(c(p, 1))/m))[1:length(p)]
    thresholds <- fitted_quantile(dist, p)
    if (is.null(ratio)) {
        ratio <- rep(1, length(p))
    }
    if (length(ratio) != length(p) || any(ratio <= 0)) {
        stop("invalid 'ratio'")
    }
    bracketavg <- bracket_average(dist, p, c(p[-1], 1))
    bracketavg_singles <- bracketavg/(1 - ck + ck/ratio)
    bracketavg_couples <- bracketavg/(ratio*(1 - ck) + ck)
    average_singles <- sum(diff(c(p_singles, 1))*bracketavg_singles)
    average_couples <- sum(diff(c(p_couples, 1))*bracketavg_couples)

    # Interpolate the distribution of singles and couples
    if (has_zero) {
        dist_singles <- tabulation_fit(p_singles, thresholds, average_singles, bracketavg=bracketavg_singles)
        dist_couples <- tabulation_fit(p_couples, thresholds, average_couples, bracketavg=bracketavg_couples)
    } else {
        dist_singles <- tabulation_fit(p_singles[-1], thresholds[-1], average_singles, bracketavg=bracketavg_singles[-1])
        dist_couples <- tabulation_fit(p_couples[-1], thresholds[-1], average_couples, bracketavg=bracketavg_couples[-1])
    }

    # Return an object with the parent distribution and the interpolated couple
    # share
    new_dist <- list()
    class(new_dist) <- c("gpinter_dist_indiv", "gpinter_dist")

    new_dist$singles <- list(
        dist = dist_singles,
        average = average_singles,
        pk = p_singles,
        threshold = thresholds,
        bracketavg = bracketavg_singles
    )
    new_dist$couples <- list(
        dist = dist_couples,
        average = average_couples,
        pk = p_couples,
        thresholds = thresholds,
        bracketavg = bracketavg_couples
    )
    new_dist$average <- dist$average/(1 + m)
    new_dist$couple_share <- m
    new_dist$pk <- p
    new_dist$ck <- ck

    return(new_dist)
}
