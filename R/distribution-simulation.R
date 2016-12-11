#' @title Simulate data
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Generate a sample from a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param n The size of the sample to generate.
#' @param ... Ignored.
#'
#' @return A sample of size \code{n}.
#'
#' @importFrom stats runif
#'
#' @export

simulate_gpinter <- function(dist, n, ...) UseMethod("simulate_gpinter")

#' @export
simulate_gpinter.gpinter_dist_orig <- function(dist, n, ...) {
    return(fitted_quantile(dist, runif(n)))
}

#' @export
simulate_gpinter.gpinter_dist_merge <- function(dist, n, ...) {
    # Number of distribution that were merged
    k <- length(dist$ndist)

    d <- as.vector(rmultinom(1, n, prob=dist$relsize))

    return(sapply(1:k, function(i) {
        return(simulate_gpinter(dist$parent_dist[[i]], d[i]))
    }))
}
