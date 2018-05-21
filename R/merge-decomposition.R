#' @title Decompose population origin by percentile in a merged distribution
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Given an output of \code{merge_dist} and a list of fractiles,
#' gives the population composition in terms of origin distribution
#' for each bracket .
#'
#' @param dist An object of class \code{gpinter_dist_merge}.
#' @param p A numeric vector of fractiles in [0, 1[.
#' @param ... Ignored.
#'
#' @return A \code{data.frame} with the following columns:
#' \describe{
#'     \item{p}{The fractiles defining the brackets.}
#'     \item{share1}{The population share of the first merged distribution.}
#'     \item{share2}{The population share of the second merged distribution.}
#'     \item{...}{The population share of the other merged distribution.}
#' }
#'
#' @export

decompose_population <- function(dist, p, ...) UseMethod("decompose_population")

#' @export
decompose_population.gpinter_dist_merge <- function(dist, p, ...) {
    # Get the thresholds for the merged distribution
    thr <- fitted_quantile(dist, p)
    # Calculate composition
    comp <- sapply(seq_along(dist$parent_dist), function(i) {
        parent_dist <- dist$parent_dist[[i]]
        relsize <- dist$relsize[[i]]
        p_parent <- fitted_cdf(parent_dist, thr)
        return(relsize*diff(c(p_parent, 1)))
    })
    # Normalize by the bracket size
    comp <- comp/diff(c(p, 1))
    comp <- as.data.frame(cbind(p, comp))
    colnames(comp) <- c("p", paste0("share", 1:length(dist$parent_dist)))
    return(comp)
}
