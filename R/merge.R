#' @title Merge the distribution from different countries
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Takes an arbitrary number of distributions from different
#' countries, with their respective population sizes, and return an object
#' that characterizes the combined distribution of all countries.
#'
#' @param dist A vector of objects of class \code{gpinter_dist_orig}.
#' @param popsize A vector of the same length with the population associated
#' with each distribution.
#'
#' @return An object of class \code{gpinter_dist_merge}.
#'
#' @importFrom methods is
#'
#' @export

merge_dist <- function(dist, popsize) {
    # Check the class of input distributions
    lapply(dist, function(dist) {
        if (!is(dist, "gpinter_dist_orig")) {
            stop("'dist' objects must be of class 'gpinter_dist_orig'")
        }
    })

    ndist <- length(dist)
    if (length(popsize) != ndist) {
        stop("'dist' and 'popsize' must have the same length.")
    }
    if (anyNA(dist)) {
        stop("'dist' may not have missing values.")
    }
    if (anyNA(popsize)) {
        stop("'popsize' may not have missing values.")
    }

    poptotal <- sum(popsize)
    relsize <- popsize/poptotal
    average <- sum(relsize*sapply(dist, function(d) d$average))

    dist_merge <- list()
    class(dist_merge) <- c("gpinter_dist", "gpinter_dist_merge")

    dist_merge$poptotal <- poptotal
    dist_merge$popsize <- popsize
    dist_merge$relsize <- relsize
    dist_merge$average <- average
    dist_merge$parent_dist <- dist
    dist_merge$ndist <- ndist

    return(dist_merge)
}
