#' @title Generate a tabulation of a distribution
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Generate a tabulation with shares, averages and thresholds
#' at different points of the distribution. This methods avoid redundant
#' computations compared with using the dedicated function for each quantity.
#'
#' @param dist An object of class \code{gpinter_dist_orig}, \code{gpinter_dist_indiv},
#' \code{gpinter_dist_addup} or \code{gpinter_dist_merge}.
#' @param fractiles A vector of values in [0, 1[.
#' @param ... Ignored.
#'
#' @return A data frame with a row for each fractile and a column for each
#' statistic.
#'
#' @export

generate_tabulation <- function(dist, fractiles, ...) UseMethod("generate_tabulation")

#' @export
generate_tabulation.gpinter_dist_orig <- function(dist, fractiles, ...) {
    tab <- list(
        fractile = fractiles,
        threshold = fitted_quantile(dist, fractiles),
        top_share = top_share(dist, fractiles)
    )

    tab$bottom_share  <- 1 - tab$top_share
    tab$bracket_share <- diff(c(tab$bottom_share, 1))

    tab$top_average     <- dist$average*tab$top_share/(1 - fractiles)
    tab$bottom_average  <- dist$average*tab$bottom_share/fractiles
    tab$bracket_average <- dist$average*tab$bracket_share/diff(c(fractiles, 1))

    tab$invpareto <- tab$top_average/tab$threshold

    tab$p10_average <- tab$threshold[11]/dist$average
    tab$p50_average <- tab$threshold[51]/dist$average
    tab$p90_average <- tab$threshold[90]/dist$average
    tab$p99_average <- tab$threshold[100]/dist$average

    tab$b10 <- tab$invpareto[11]
    tab$b50 <- tab$invpareto[51]
    tab$b90 <- tab$invpareto[90]
    tab$b99 <- tab$invpareto[100]

    return(tab)
}

#' @export
generate_tabulation.gpinter_dist <- function(dist, fractiles, ...) {
    tab <- list(
        fractile = fractiles,
        threshold = fitted_quantile(dist, fractiles)
    )
    tab$top_share <- threshold_share(dist, tab$threshold)

    tab$bottom_share  <- 1 - tab$top_share
    tab$bracket_share <- diff(c(tab$bottom_share, 1))

    tab$top_average     <- dist$average*tab$top_share/(1 - fractiles)
    tab$bottom_average  <- dist$average*tab$bottom_share/fractiles
    tab$bracket_average <- dist$average*tab$bracket_share/diff(c(fractiles, 1))

    tab$invpareto <- tab$top_average/tab$threshold

    tab$p10_average <- tab$threshold[11]/dist$average
    tab$p50_average <- tab$threshold[51]/dist$average
    tab$p90_average <- tab$threshold[91]/dist$average
    tab$p99_average <- tab$threshold[100]/dist$average

    tab$b10 <- tab$invpareto[11]
    tab$b50 <- tab$invpareto[51]
    tab$b90 <- tab$invpareto[91]
    tab$b99 <- tab$invpareto[100]

    return(tab)
}


