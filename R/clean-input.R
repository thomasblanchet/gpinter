#' @title Clean (and check) the inputs for \code{tabulation_fit}
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Check the validity and consistency of the input arguments
#' of \code{tabulation_fit}.
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
#' @export

clean_input_tabulation <- function(p, threshold, average=NULL, bracketshare=NULL, topshare=NULL,
                                   bracketavg=NULL, topavg=NULL, invpareto=NULL,
                                   bottom_model=NULL, lower_bound=0) {
    # Number of interpolation points
    n <- length(p)
    if (n < 3) {
        stop("the method requires at least three interpolation points")
    }
    if (length(threshold) != n) {
        stop("'p' and 'threshold' must have the same length")
    }
    # Sort the input data
    ord <- order(p)
    p <- p[ord]
    threshold <- threshold[ord]

    # Model for the bottom
    if (p[1] > 0 && is.null(bottom_model)) {
        if (threshold[1] != 0) {
            bottom_model <- "gpd"
        } else if (threshold[1] == 0) {
            bottom_model <- "dirac"
        }
    }
    if (!is.null(bottom_model) && !bottom_model %in% c("hist", "gpd", "dirac")) {
        stop("'bottom_model' must be one of 'hist', 'gpd', 'dirac', or NULL")
    }
    if (!is.null(bottom_model) && bottom_model == "hist" && lower_bound > threshold[1]) {
        stop("'lower_bound' must be smaller than min(threshold)")
    }

    # Put the information on average in the right format (truncated average)
    if (!is.null(bracketshare)) {
        if (is.null(average) | is.na(average)) {
            stop("you must specify a total average")
        }
        if (length(bracketshare) != n) {
            stop("'p' and 'bracketshare' must have the same length")
        }
        bracketshare <- bracketshare[ord]
        m <- rev(cumsum(rev(bracketshare*average)))
    } else if (!is.null(topshare)) {
        if (is.null(average) | is.na(average)) {
            stop("you must specify a total average")
        }
        if (length(topshare) != n) {
            stop("'p' and 'topshare' must have the same length")
        }
        topshare <- topshare[ord]
        m <- average*topshare
    } else if (!is.null(bracketavg)) {
        if (length(bracketavg) != n) {
            stop("'p' and 'bracketavg' must have the same length")
        }

        if (p[1] > 0) {
            if (is.null(average) | is.na(average)) {
                stop("you must specify a total average")
            }
        } else if (is.null(average) | is.na(average)) {
            average <- sum(diff(c(p, 1))*bracketavg)
        }

        bracketavg <- bracketavg[ord]
        m <- rev(cumsum(rev(diff(c(p, 1))*bracketavg)))
    } else if (!is.null(topavg)) {
        if (length(topavg) != n) {
            stop("'p' and 'topavg' must have the same length")
        }

        if (p[1] > 0) {
            if (is.null(average) | is.na(average)) {
                stop("you must specify a total average")
            }
        } else if (is.null(average) | is.na(average)) {
            average <- topavg[1]
        }

        topavg <- topavg[ord]
        m <- (1 - p)*topavg
    } else if (!is.null(invpareto)) {
        if (length(invpareto) != n) {
            stop("'p' and 'invpareto' must have the same length")
        }

        if (p[1] > 0) {
            if (is.null(average) | is.na(average)) {
                stop("you must specify a total average")
            }
        }

        invpareto <- invpareto[ord]
        m <- (1 - p)*threshold*invpareto

        # The inverted Pareto may not be defined for the first threshold
        if (is.na(invpareto[1]) & (threshold[1] == 0)) {
            m[1] <- average
        }
    } else {
        stop("you must specify one of 'bracketshare', 'topshare', 'bracketavg', 'topavg' or 'invpareto'")
    }

    # Sanity check of the data
    # Fractiles are strictly increasing
    if (any(diff(p) <= 0)) {
        index_error <- min(which(diff(p) <= 0))
        p1_error <- p[index_error]
        p2_error <- p[index_error + 1]
        stop(paste0("p must be strictly increasing: at rows ", index_error, " and ", index_error + 1,
            ", you have p=", p1_error, " followed by p=", p2_error))
    }
    # Quantile function is increasing
    if (any(diff(threshold) <= 0)) {
        index_error <- min(which(diff(threshold) <= 0))
        t1_error <- threshold[index_error]
        t2_error <- threshold[index_error + 1]
        stop(paste0("thresholds must be strictly increasing: at rows ", index_error, " and ", index_error + 1,
            ", you have threshold=", t1_error, " followed by threshold=", t2_error))
    }
    # Percentiles between 0 and 1
    if (any(p >= 1) | any(p < 0)) {
        stop("the elements of 'p' must be >=0 and <1.")
    }
    # The average between each bracket is within the bracket
    bracketavg <- -diff(c(m, 0))/diff(c(p, 1))
    for (i in 1:(n - 1)) {
        if (bracketavg[i] <= threshold[i] || bracketavg[i] >= threshold[i + 1]) {
            stop(sprintf(paste(
                "input data is inconsistent between p=%.4f and p=%.4f. The bracket",
                "average (%.2f) is not strictly within the bracket thresholds (%.2f and %.2f)"
            ), p[i], p[i + 1], bracketavg[i], threshold[i], threshold[i + 1]))
        }
        if (bracketavg[n] <= threshold[n]) {
            stop(sprintf(paste(
                "input data is inconsistent after p=%.4f. The bracket",
                "average (%.2f) is not strictly above the bracket threshold (%.2f)"
            ), p[n], bracketavg[n], threshold[n]))
        }
    }
    # Total average consistent with bracket averages
    if (p[1] == 0) {
        if (abs((average - m[1])/average) > 1e-2) {
            stop(sprintf(paste("the average you specified (%.2f) is inconsistent with the average",
                "implied by the brackets (%.2f)"), average, m[1]))
        }
        average <- m[1]
    }

    return(list(p=p, m=m, threshold=threshold, average=average,
        bottom_model=bottom_model, lower_bound=lower_bound, n=n))
}

#' @title Clean (and check) the inputs for \code{shares_fit}
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Check the validity and consistency of the input arguments
#' of \code{shares_fit}.
#'
#' @param p A vector of values in [0, 1].
#' @param average The average over the entire distribution.
#' @param bracketshare The corresponding bracket share.
#' @param topshare The corresponding top share.
#' @param bracketavg The corresponding bracket average.
#' @param topavg The corresponding top average.
#' @param first_threshold The value of the first threshold. If \code{NULL}, it
#' is estimated from the data. Default is \code{NULL}.
#' @param bottom_model Which model to use at the bottom of the distribution?
#' Only relevant if \code{min(p) > 0}. Either \code{"gpd"} for the generalized
#' Pareto distribution, or \code{"hist"} for histogram density. Default is
#' \code{"hist"} if \code{min(threshold) > 0}, and \code{"gpd"} otherwise.
#' @param lower_bound Lower bound of the distribution. Only relevant if
#' \code{min(p) > 0}. Default is \code{0}.
#'
#' @export

clean_input_shares <- function(p, average, bracketshare=NULL, topshare=NULL,
                               bracketavg=NULL, topavg=NULL,
                               first_threshold=NULL, bottom_model=NULL, lower_bound=0) {
    # Number of interpolation points
    n <- length(p)
    if (n < 3) {
        stop("the method requires at least three interpolation points")
    }
    # Sort the input data
    ord <- order(p)
    p <- p[ord]

    # Put the information on average in the right format (truncated average)
    if (!is.null(bracketshare)) {
        if (is.null(average) | is.na(average)) {
            stop("you must specify a total average")
        }
        if (length(bracketshare) != n) {
            stop("'p' and 'bracketshare' must have the same length")
        }
        bracketshare <- bracketshare[ord]
        m <- rev(cumsum(rev(bracketshare*average)))
    } else if (!is.null(topshare)) {
        if (is.null(average) | is.na(average)) {
            stop("you must specify a total average")
        }
        if (length(topshare) != n) {
            stop("'p' and 'topshare' must have the same length")
        }
        topshare <- topshare[ord]
        m <- average*topshare
    } else if (!is.null(bracketavg)) {
        if (length(bracketavg) != n) {
            stop("'p' and 'bracketavg' must have the same length")
        }

        if (p[1] > 0) {
            if (is.null(average) | is.na(average)) {
                stop("you must specify a total average")
            }
        } else {
            average <- sum(diff(c(p, 1))*bracketavg)
        }

        bracketavg <- bracketavg[ord]
        m <- rev(cumsum(rev(diff(c(p, 1))*bracketavg)))
    } else if (!is.null(topavg)) {
        if (length(topavg) != n) {
            stop("'p' and 'topavg' must have the same length")
        }

        if (p[1] > 0) {
            if (is.null(average) | is.na(average)) {
                stop("you must specify a total average")
            }
        } else {
            average <- topavg[1]
        }

        topavg <- topavg[ord]
        m <- (1 - p)*topavg
    } else {
        stop("You must specify one of 'bracketshare', 'topshare', 'bracketavg' or 'topavg'")
    }

    # Check that bracket averages are increasing
    bracketavg <- -diff(c(m, 0))/diff(c(p, 1))
    if (any(diff(bracketavg) <= 0)) {
        index_error <- min(which(diff(bracketavg) <= 0))
        t1_error <- bracketavg[index_error]
        t2_error <- bracketavg[index_error + 1]
        stop(paste0("bracket averages must be strictly increasing: at rows ", index_error, " and ", index_error + 1,
            ", you have bracket average=", t1_error, " followed by bracket average=", t2_error))
    }

    return(list(p=p, m=m, first_threshold=first_threshold, average=average,
        bottom_model=bottom_model, lower_bound=lower_bound, n=n))
}


#' @title Clean (and check) the inputs for \code{threshold_fit}
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Check the validity and consistency of the input arguments
#' of \code{threshold_fit}.
#'
#' @param p A vector of values in [0, 1].
#' @param threshold The quantiles corresponding to \code{p}.
#' @param average The average over the entire distribution. Use \code{NULL} for
#' unknown. (Default is \code{NULL}.)
#' @param last_bracketavg The average in the last bracket. Use \code{NULL} for
#' unknown. (Default is \code{NULL}.)
#' @param last_invpareto The inverted Pareto coefficient at the last threshold.
#' Use \code{NULL} for unknown. (Default is \code{NULL}.)
#' @param bottom_model Which model to use at the bottom of the distribution?
#' Only relevant if \code{min(p) > 0}. Either \code{"gpd"} for the generalized
#' Pareto distribution, or \code{"hist"} for histogram density. Default is
#' \code{"hist"} if \code{min(threshold) > 0}, and \code{"gpd"} otherwise.
#' @param lower_bound Lower bound of the distribution. Only relevant if
#' \code{min(p) > 0}. Default is \code{0}.
#' @param binf Asymptotic Pareto coefficient.
#'
#' @export

clean_input_thresholds <- function(p, threshold, average=NULL,
                                   last_bracketavg=NULL, last_invpareto=NULL,
                                   bottom_model=NULL, lower_bound=0, binf=NULL) {
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
    if (!is.null(bottom_model) && bottom_model == "hist" && lower_bound > threshold[1]) {
        stop("'lower_bound' must be smaller than min(threshold).")
    }

    # Quantile function is increasing
    if (any(diff(threshold) <= 0)) {
        index_error <- min(which(diff(threshold) <= 0))
        t1_error <- threshold[index_error]
        t2_error <- threshold[index_error + 1]
        stop(paste0("thresholds must be strictly increasing: at rows ", index_error, " and ", index_error + 1,
            ", you have threshold=", t1_error, " followed by threshold=", t2_error))
    }

    if (!is.null(last_bracketavg) && !is.na(last_bracketavg)) {
        if (last_bracketavg <= threshold[n]) {
            stop("The average in the last bracket is below the last threshold")
        }
        last_m <- (1 - p[n])*last_bracketavg
    } else if (!is.null(last_invpareto) && !is.na(last_invpareto)) {
        if (last_invpareto <= 1) {
            stop("The inverted Pareto coefficient at the last threhsold must be above one")
        }
        last_m <- (1 - p[n])*threshold[n]*last_invpareto
    } else {
        last_m <- NULL
    }

    return(list(p=p, threshold=threshold, last_m=last_m,
        bottom_model=bottom_model, lower_bound=lower_bound))
}

