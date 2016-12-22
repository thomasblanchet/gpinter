#' @title Conditional quantile function of the Gumbel copula
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Assume (U, V) follows a Gumbel copula. This function gives
#' the quantile function of V given U = u. It is used to simulate the Gumbel
#' copula.
#'
#' @param p A number in [0, 1].
#' @param u The value of U.
#' @param theta The parameter of the Gumbel copula.
#'
#' @return The p-th quantile of V|U = u.
#'
#' @importFrom emdbook lambertW

gumbel_cond_quantile <- function(p, u, theta) {
    if (theta == 1) {
        return(p)
    } else {
        return(
            exp(-(((-1 + theta)*lambertW(1/((-1 + theta)*(-((p*u*log(u))/
            (-log(u))^theta))^(1/(-1 + theta)))))^theta - (-log(u))^theta)^(1/theta))
        )
    }
}

#' @title Add up two components of income or wealth
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Add up two components with their own distributions (say, labor
#' and capital income), assuming the dependence between both components is
#' characterized by a Gumbel copula.
#'
#' @param dist1 The distribution of the first component.
#' @param dist2 The distribution of the second component.
#' @param theta The parameter of the Gumbel copula.
#'
#' @return An object of class \code{gpinter_dist_addup}
#'
#' @importFrom randtoolbox halton
#'
#' @export

addup_dist <- function(dist1, dist2, theta) {
    if (!is(dist1, "gpinter_dist_orig") || !is(dist2, "gpinter_dist_orig")) {
        stop("'dist1' and 'dist2' must be of class gpinter_dist_orig.")
    }
    if (theta < 1) {
        stop("'theta' must be >= 1.")
    }

    # Generate a copula with importance sampling
    x1 <- halton(99e4, dim=2, init=TRUE)
    x1[, 1] <- 0.99*x1[, 1]
    w1 <- rep(0.01, 99e4)

    x2 <- halton(9e4, dim=2, init=FALSE)
    x2[, 1] <- 0.99 + 0.009*x2[, 1]
    w2 <- rep(0.001, 9e4)

    x3 <- halton(9e4, dim=2, init=FALSE)
    x3[, 1] <- 0.999 + 0.0009*x3[, 1]
    w3 <- rep(0.0001, 9e4)

    x4 <- halton(9e4, dim=2, init=FALSE)
    x4[, 1] <- 0.9999 + 0.00009*x4[, 1]
    w4 <- rep(0.00001, 9e4)

    x5 <- halton(10e4, dim=2, init=FALSE)
    x5[, 1] <- 0.99999 + 0.00001*x5[, 1]
    w5 <- rep(0.000001, 10e4)

    x <- rbind(x1, x2, x3, x4, x5)
    w <- c(w1, w2, w3, w4, w5)

    x[, 2] <- gumbel_cond_quantile(x[, 2], x[, 1], theta)

    # Apply the marginals
    x[, 1] <- fitted_quantile(dist1, x[, 1])
    x[, 2] <- fitted_quantile(dist2, x[, 2])

    # Sum the components
    x <- x[, 1] + x[, 2]

    # Order the sample once and for all
    ord <- order(x)
    x <- x[ord]
    w <- w[ord]

    # Normalize weights
    w <- w/sum(w)

    # Cumulative weights
    cw <- c(0, cumsum(w))

    result <- list()
    class(result) <- c("gpinter_dist_addup", "gpinter_dist_qmc", "gpinter_dist")

    result$parent1 <- dist1
    result$parent2 <- dist2
    result$theta <- theta
    result$x <- x
    result$w <- w
    result$cw <- cw
    result$average <- sum(x*w)

    return(result)
}
