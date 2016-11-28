#' @title Simulate data
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Generate a sample from a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
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
