#' @title Estimate parameters of the parameters of the generalized Pareto
#' distribution for the top of the distribution
#'
#' @author Thomas Blanchet
#'
#' @description Above the last threshold, we extrapolate the curve using a
#' generalized Pareto distribution. The three parameters of the distribution
#' are chosen so as to ensure the continuity and derivability of the quantile
#' function, and to match the average in the last bracket.
#'
#' @param xn The last interpolation point.
#' @param yn The value of the function at the last interpolation point.
#' @param sn The value of the first derivative at the last interpolation point.
#' @param an The value of the second derivative at the last interpolation point.
#'
#' @return A list with the three parameters: \code{mu}, \code{sigma} and \code{mu}.

gpd_top_parameters <- function(xn, yn, sn, an) {
    pn <- 1 - exp(-xn)
    # Identify the parameter mu via the value of the quantile function at pn
    mu <- exp(xn - yn)*sn
    # Identify the parameter sigma via the derivative of the quantile function
    sigma <- (1 - pn)*exp(2*xn - yn)*(an + sn*(1 - sn))
    # Identify the parameter xi via the average over the last threshold
    xi <- 1 - (1 - pn)*sigma/(exp(-yn) - (1 - pn)*mu)

    return(list(mu=mu, sigma=sigma, xi=xi))
}

#' @title Quantile function of the generalized Pareto distribution in the top
#' of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the quantile function of the
#' generalized Pareto distribution, but with some rescaling to model the top
#' of the distribution only.
#'
#' @param p A value in [pn, 1]. The point at which to evaluate the function.
#' @param pn A value in [0, 1]. The point of the distribution at which the
#' GDP model starts.
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the quantile at \code{p}.

gpd_top_quantile <- function(p, pn, mu, sigma, xi) {
    return(mu + sigma/xi*(((1 - pn)/(1 - p))^xi - 1))
}

#' @title Density function of the generalized Pareto distribution in the top
#' of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the density function of the
#' generalized Pareto distribution, but with some rescaling to model the top
#' of the distribution only.
#'
#' @param q Real number. The point at which to evaluate the density.
#' @param pn A value in [0, 1]. The point of the distribution at which the
#' GDP model starts
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the density at \code{q}.

gpd_top_density <- function(q, pn, mu, sigma, xi) {
    return((1 - pn)/sigma*(1 + xi*(q - mu)/sigma)^(-(1 + 1/xi)))
}

#' @title Cumulative density function of the generalized Pareto distribution
#' in the top of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the density function of the
#' generalized Pareto distribution, but with some rescaling to model the top
#' of the distribution only.
#'
#' @param q Real number. The point at which to evaluate the CDF.
#' @param pn A value in [0, 1]. The point of the distribution at which the
#' GDP model starts
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the CDF at \code{q}.

gpd_top_cdf <- function(q, pn, mu, sigma, xi) {
    return(pn + (1 - pn)*(1 - (1 + xi*(q - mu)/sigma)^(-1/xi)))
}

#' @title Truncated average function of the generalized Pareto distribution in
#' the top of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the truncated average of the
#' generalized Pareto distribution, but with some rescaling to model the
#' top of the distribution only.
#'
#' @param p A value in [pn, 1]. The point at which to evaluate the function.
#' @param pn A value in [0, 1]. The point of the distribution at which the
#' GDP model starts.
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the truncated average at \code{p}.

gpd_top_mean <- function(p, pn, mu, sigma, xi) {
    return((1 - p)*(mu - sigma/xi + sigma/(xi*(1 - xi))*((1 - pn)/(1 - p))^xi))
}

#' @title Interpolation function in the last bracket
#'
#' @author Thomas Blanchet
#'
#' @description Interpolation function based on the generalized Pareto
#' distribution used in the last bracket of the data.
#'
#' @param x A value in greater than xn. The point at which to evaluate the
#' function.
#' @param pn A value in [0, 1]. The point of the distribution at which the
#' GDP model starts.
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the interpolation function at \code{x}.

gpd_top_phi <- function(x, pn, mu, sigma, xi) {
    p <- 1 - exp(-x)
    return(-log(gpd_top_mean(p, pn, mu, sigma, xi)))
}

#' @title Derivative of the interpolation function in the last bracket
#'
#' @author Thomas Blanchet
#'
#' @description Interpolation function based on the generalized Pareto
#' distribution used in the last bracket of the data.
#'
#' @param x A value greater than xn. The point at which to evaluate the
#' function.
#' @param pn A value in [0, 1]. The point of the distribution at which the
#' GDP model starts.
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the derivative of the interpolation function at
#' \code{x}.

gpd_top_deriv_phi <- function(x, pn, mu, sigma, xi) {
    p <- 1 - exp(-x)
    return((1 - p)*gpd_top_quantile(p, pn, mu, sigma, xi)/gpd_top_mean(p, pn, mu, sigma, xi))
}

#' @title Estimate parameters of the parameters of the generalized Pareto
#' distribution for the bottom of the distribution
#'
#' @author Thomas Blanchet
#'
#' @description Below the first threshold, we extrapolate the curve using a
#' generalized Pareto distribution. The three parameters of the distribution
#' are chosen so as to ensure the continuity and derivability of the quantile
#' function, and to match the average.
#'
#' @param x1 The first interpolation point.
#' @param y1 The value of the function at the first interpolation point.
#' @param s1 The value of the first derivative at the first interpolation point.
#' @param a1 The value of the second derivative at the first interpolation point.
#' @param average The average over the entire distribution.
#'
#' @return A list with the three parameters: \code{mu}, \code{sigma} and \code{mu}.

gpd_bottom_parameters <- function(x1, y1, s1, a1, average) {
    p1 <- 1 - exp(-x1)
    # Identify the parameter mu via the value of the quantile function at p1
    mu <- exp(x1 - y1)*s1
    # Identify the parameter sigma via the derivative of the quantile function
    sigma <- p1*exp(2*x1 - y1)*(a1 + s1*(1 - s1))
    # Identify the parameter xi via the average below the last threshold
    xi <- 1 - p1*sigma/(p1*mu - average + exp(-y1))

    return(list(mu=mu, sigma=sigma, xi=xi))
}

#' @title Quantile function of the generalized Pareto distribution in the bottom
#' of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the quantile function of the
#' generalized Pareto distribution, but with some rescaling to model the bottom
#' of the distribution only.
#'
#' @param p A value in [0, p1]. The point at which to evaluate the function.
#' @param p1 A value in [0, 1]. The point of the distribution at which the
#' GDP model ends
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the quantile at \code{p}.

gpd_bottom_quantile <- function(p, p1, mu, sigma, xi) {
    return(mu + sigma/xi*(1 - (p1/p)^xi))
}

#' @title Density function of the generalized Pareto distribution in the bottom
#' of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the density function of the
#' generalized Pareto distribution, but with some rescaling to model the bottom
#' of the distribution only.
#'
#' @param q Real number. The point at which to evaluate the density.
#' @param p1 A value in [0, 1]. The point of the distribution at which the
#' GDP model ends
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the quantile at \code{p}.

gpd_bottom_density <- function(q, p1, mu, sigma, xi) {
    return(p1/sigma*(1 - xi*(q - mu)/sigma)^(-(1 + 1/xi)))
}

#' @title Cumulative density function of the generalized Pareto distribution
#' in the bottom of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the density function of the
#' generalized Pareto distribution, but with some rescaling to model the bottom
#' of the distribution only.
#'
#' @param q A value in [0, p1]. The point at which to evaluate the function.
#' @param p1 A value in [0, 1]. The point of the distribution at which the
#' GDP model ends
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the CDF at \code{p}.

gpd_bottom_cdf <- function(q, p1, mu, sigma, xi) {
    return(p1*(1 - xi*(q - mu)/sigma)^(-1/xi))
}

#' @title Truncated average function of the generalized Pareto distribution in
#' the bottom of the distribution.
#'
#' @author Thomas Blanchet
#'
#' @description This function correspond to the truncated average of the
#' generalized Pareto distribution, plus the value of the truncated average
#' above.
#'
#' @param p A value in [0, p1]. The point at which to evaluate the function.
#' @param p1 A value in [0, 1]. The point of the distribution at which the
#' GDP model ends.
#' @param y1 The value of the interpolation function at x1
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the truncated average at \code{p}.

gpd_bottom_mean <- function(p, p1, y1, mu, sigma, xi) {
    return(mu*(p1 - p) + sigma/xi*(p1 - p) + sigma/(xi*(1 - xi))*(p*(p1/p)^xi - p1) + exp(-y1))
}

#' @title Interpolation function below the first bracket
#'
#' @author Thomas Blanchet
#'
#' @description Interpolation function based on the generalized Pareto
#' distribution used in the last bracket of the data.
#'
#' @param x A value lower than x1. The point at which to evaluate the
#' function.
#' @param p1 A value in [0, 1]. The point of the distribution at which the
#' GDP model ends
#' @param y1 The value of the interpolation function at x1
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the interpolation function at \code{x}.

gpd_bottom_phi <- function(x, p1, y1, mu, sigma, xi) {
    p <- 1 - exp(-x)
    return(-log(gpd_bottom_mean(p, p1, y1, mu, sigma, xi)))
}

#' @title Derivative of the interpolation function below the first bracket
#'
#' @author Thomas Blanchet
#'
#' @description Interpolation function based on the generalized Pareto
#' distribution used in the last bracket of the data.
#'
#' @param x A value lower than x1. The point at which to evaluate the
#' function.
#' @param p1 A value in [0, 1]. The point of the distribution at which the
#' GDP model starts.
#' @param y1 The value of the interpolation function at x1
#' @param mu The location parameter.
#' @param sigma The scale parameter.
#' @param xi The shape parameter.
#'
#' @return The value of the derivative of the interpolation function at
#' \code{x}.

gpd_bottom_deriv_phi <- function(x, p1, y1, mu, sigma, xi) {
    p <- 1 - exp(-x)
    return((1 - p)*gpd_bottom_quantile(p, p1, mu, sigma, xi)/gpd_bottom_mean(p, p1, y1, mu, sigma, xi))
}
