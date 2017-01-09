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
#' @importFrom stats runif rmultinom rbinom
#' @importFrom gumbel rgumbel
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

    # Number of observations from each country
    d <- as.vector(rmultinom(1, n, prob=dist$relsize))

    return(sapply(1:k, function(i) {
        return(simulate_gpinter(dist$parent_dist[[i]], d[i]))
    }))
}

#' @export
simulate_gpinter.gpinter_dist_indiv <- function(dist, n, ...) {
    p <- runif(n)

    # Number of couples
    nb_couples <- rbinom(1, size=n, prob=dist$couple_share)

    singles <- simulate_gpinter(dist$singles$dist, n - nb_couples)
    couples <- simulate_gpinter(dist$couples$dist, nb_couples)

    x <- sort(c(couples/2, couples/2, singles))

    return(x)
}

#' @export
simulate_gpinter.gpinter_dist_addup <- function(dist, n, ...) {
    x <- rgumbel(n, dist$theta)

    x[, 1] <- fitted_quantile(dist$parent1, x[, 1])
    x[, 2] <- fitted_quantile(dist$parent2, x[, 2])

    return(rowSums(x))
}
