#' @title Lorenz plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the Lorenz curve of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_lorenz <- function(dist, xlim, ...) UseMethod("plot_lorenz")

#' @export
plot_lorenz.gpinter_dist <- function(dist, xlim, ...) {
    p1 <- min(dist$pk_nc)
    pn <- max(dist$pk_nc)
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_inter <- seq(max(p1, pmin), min(pn, pmax), length.out=100)

    if (pmax >= pn) {
        p_above <- seq(pn, pmax, length.out=100)
    } else {
        p_above <- numeric(0)
    }

    if (pmin <= p1) {
        p_below <- seq(pmin, p1, length.out=100)
    } else {
        p_below <- numeric(0)
    }

    p_point <- dist$pk_nc[(dist$pk_nc >= pmin) & (dist$pk_nc <= pmax)]
    if (pmax == 1 & max(p_point) < 1) {
        p_point <- c(p_point, 1)
    }
    if (pmin == 0 & min(p_point) > 0) {
        p_point <- c(0, p_point)
    }

    df_inter <- data.frame(
        p = p_inter,
        y = bottom_share(dist, p_inter)
    )
    df_below <- data.frame(
        p = p_below,
        y = bottom_share(dist, p_below)
    )
    df_above <- data.frame(
        p = p_above,
        y = bottom_share(dist, p_above)
    )
    df_point <- data.frame(
        p = p_point,
        y = bottom_share(dist, p_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="p", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="p", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="p", y="y")) +
        ggplot2::geom_abline(slope=1, linetype="dotted") +
        ggplot2::xlab("fraction of the population") +
        ggplot2::ylab("cumulative share")

    return(plot)
}

#' @title Generalized Pareto curve plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the generalized Pareto curve of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_gpc <- function(dist, xlim, ...) UseMethod("plot_gpc")

#' @export
plot_gpc.gpinter_dist <- function(dist, xlim, ...) {
    p1 <- min(dist$pk_nc)
    pn <- max(dist$pk_nc)
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_inter <- seq(max(p1, pmin), min(pn, pmax), length.out=100)

    if (pmax >= pn) {
        p_above <- seq(pn, pmax, length.out=100)
    } else {
        p_above <- numeric(0)
    }

    if (pmin <= p1) {
        p_below <- seq(pmin, p1, length.out=100)
    } else {
        p_below <- numeric(0)
    }

    p_point <- dist$pk_nc[(dist$pk_nc >= pmin) & (dist$pk_nc <= pmax)]
    if (pmax == 1 & max(p_point) < 1) {
        p_point <- c(p_point, 1)
    }
    if (pmin == 0 & min(p_point) > 0) {
        p_point <- c(0, p_point)
    }

    df_inter <- data.frame(
        p = p_inter,
        y = invpareto(dist, p_inter)
    )
    df_below <- data.frame(
        p = p_below,
        y = invpareto(dist, p_below)
    )
    df_above <- data.frame(
        p = p_above,
        y = invpareto(dist, p_above)
    )
    df_point <- data.frame(
        p = p_point,
        y = invpareto(dist, p_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="p", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="p", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="p", y="y")) +
        ggplot2::xlab("p") +
        ggplot2::ylab("inverted Pareto coefficient")

    return(plot)
}

#' @title Probability density plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the density of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_density <- function(dist, xlim, ...) UseMethod("plot_density")

#' @export
plot_density.gpinter_dist <- function(dist, xlim, ...) {
    x <- seq(xlim[1], xlim[2], length.out=200)

    df <- data.frame(
        x = x,
        y = fitted_density(dist, x)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::xlab("x") +
        ggplot2::ylab("density")

    return(plot)
}

#' @title Cumulative density plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the density of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_cdf <- function(dist, xlim, ...) UseMethod("plot_cdf")

#' @export
plot_cdf.gpinter_dist <- function(dist, xlim, ...) {
    q1 <- min(dist$qk_nc)
    qn <- max(dist$qk_nc)
    qmin <- xlim[1]
    qmax <- xlim[2]
    q_inter <- seq(max(q1, qmin), min(qn, qmax), length.out=100)

    if (qmax >= qn) {
        q_above <- seq(qn, qmax, length.out=100)
    } else {
        q_above <- numeric(0)
    }

    if (qmin <= q1) {
        q_below <- seq(qmin, q1, length.out=100)
    } else {
        q_below <- numeric(0)
    }

    q_point <- dist$qk_nc[(dist$qk_nc >= qmin) & (dist$qk_nc <= qmax)]


    df_inter <- data.frame(
        x = q_inter,
        y = fitted_cdf(dist, q_inter)
    )
    df_below <- data.frame(
        x = q_below,
        y = fitted_cdf(dist, q_below)
    )
    df_above <- data.frame(
        x = q_above,
        y = fitted_cdf(dist, q_above)
    )
    df_point <- data.frame(
        x = q_point,
        y = fitted_cdf(dist, q_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("x") +
        ggplot2::ylab("cumulaitve density")

    return(plot)
}

#' @title Quantile density plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the quantile of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_quantile <- function(dist, xlim, ...) UseMethod("plot_quantile")

#' @export
plot_quantile.gpinter_dist <- function(dist, xlim, ...) {
    p1 <- min(dist$pk_nc)
    pn <- max(dist$pk_nc)
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_inter <- seq(max(p1, pmin), min(pn, pmax), length.out=100)

    if (pmax >= pn) {
        p_above <- seq(pn, pmax, length.out=100)
    } else {
        p_above <- numeric(0)
    }

    if (pmin <= p1) {
        p_below <- seq(pmin, p1, length.out=100)
    } else {
        p_below <- numeric(0)
    }

    p_point <- dist$pk_nc[(dist$pk_nc >= pmin) & (dist$pk_nc <= pmax)]
    if (pmax == 1 & max(p_point) < 1) {
        p_point <- c(p_point, 1)
    }
    if (pmin == 0 & min(p_point) > 0) {
        p_point <- c(0, p_point)
    }

    df_inter <- data.frame(
        p = p_inter,
        y = fitted_quantile(dist, p_inter)
    )
    df_below <- data.frame(
        p = p_below,
        y = fitted_quantile(dist, p_below)
    )
    df_above <- data.frame(
        p = p_above,
        y = fitted_quantile(dist, p_above)
    )
    df_point <- data.frame(
        p = p_point,
        y = fitted_quantile(dist, p_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="p", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="p", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="p", y="y")) +
        ggplot2::xlab("p") +
        ggplot2::ylab("quantile")

    return(plot)
}

#' @title Tail function plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the tail function of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_tail <- function(dist, xlim, ...) UseMethod("plot_tail")

#' @export
plot_tail.gpinter_dist <- function(dist, xlim, ...) {
    x1 <- min(dist$xk_nc)
    xn <- max(dist$xk_nc)
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_inter <- seq(max(x1, xmin), min(xn, xmax), length.out=100)

    if (xmax >= xn) {
        x_above <- seq(xn, xmax, length.out=100)
    } else {
        x_above <- numeric(0)
    }

    if (xmin <= x1) {
        x_below <- seq(xmin, x1, length.out=100)
    } else {
        x_below <- numeric(0)
    }

    x_point <- dist$xk_nc[(dist$xk_nc >= xmin) & (dist$xk_nc <= xmax)]

    df_inter <- data.frame(
        x = x_inter,
        y = log(fitted_quantile(dist, 1 - exp(-x_inter)))
    )
    df_below <- data.frame(
        x = x_below,
        y = log(fitted_quantile(dist, 1 - exp(-x_below)))
    )
    df_above <- data.frame(
        x = x_above,
        y = log(fitted_quantile(dist, 1 - exp(-x_above)))
    )
    df_point <- data.frame(
        x = x_point,
        y = log(fitted_quantile(dist, 1 - exp(-x_point)))
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("-log(1 - p)") +
        ggplot2::ylab("log(Q(p))")

    return(plot)
}

#' @title Interpolation function plot
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the interpolation function of a distribution estimated via
#' generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_phi <- function(dist, xlim, ...) UseMethod("plot_phi")

#' @export
plot_phi.gpinter_dist <- function(dist, xlim, ...) {
    x1 <- min(dist$xk_nc)
    xn <- max(dist$xk_nc)
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_inter <- seq(max(x1, xmin), min(xn, xmax), length.out=100)

    if (xmax >= xn) {
        x_above <- seq(xn, xmax, length.out=100)
    } else {
        x_above <- numeric(0)
    }

    if (xmin <= x1) {
        x_below <- seq(xmin, x1, length.out=100)
    } else {
        x_below <- numeric(0)
    }

    x_point <- dist$xk_nc[(dist$xk_nc >= xmin) & (dist$xk_nc <= xmax)]
    if (min(x_point) > 0 & xmin == 0) {
        x_point <- c(0, x_point)
    }

    df_inter <- data.frame(
        x = x_inter,
        y = phi(dist, x_inter)
    )
    df_below <- data.frame(
        x = x_below,
        y = phi(dist, x_below)
    )
    df_above <- data.frame(
        x = x_above,
        y = phi(dist, x_above)
    )
    df_point <- data.frame(
        x = x_point,
        y = phi(dist, x_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("x") +
        ggplot2::ylab("interpolation function")

    return(plot)
}

#' @title Plot of the derivative of the interpolation function
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Plots the derivative of the interpolation function of a
#' distribution estimated via generalized Pareto interpolation.
#'
#' @param dist A \code{gpinter_dist_orig} object, as returned by
#' \code{tabulation_fit} or \code{share_fit}.
#' @param xlim The range of the curve.
#' @param ... Ignored.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line
#'
#' @export

plot_deriv_phi <- function(dist, xlim, ...) UseMethod("plot_deriv_phi")

#' @export
plot_deriv_phi.gpinter_dist <- function(dist, xlim, ...) {
    x1 <- min(dist$xk_nc)
    xn <- max(dist$xk_nc)
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_inter <- seq(max(x1, xmin), min(xn, xmax), length.out=100)

    if (xmax >= xn) {
        x_above <- seq(xn, xmax, length.out=100)
    } else {
        x_above <- numeric(0)
    }

    if (xmin <= x1) {
        x_below <- seq(xmin, x1, length.out=100)
    } else {
        x_below <- numeric(0)
    }

    x_point <- dist$xk_nc[(dist$xk_nc >= xmin) & (dist$xk_nc <= xmax)]
    if (min(x_point) > 0 & xmin == 0) {
        x_point <- c(0, x_point)
    }

    df_inter <- data.frame(
        x = x_inter,
        y = deriv_phi(dist, x_inter)
    )
    df_below <- data.frame(
        x = x_below,
        y = deriv_phi(dist, x_below)
    )
    df_above <- data.frame(
        x = x_above,
        y = deriv_phi(dist, x_above)
    )
    df_point <- data.frame(
        x = x_point,
        y = deriv_phi(dist, x_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_inter, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_line(data=df_below, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_line(data=df_above, ggplot2::aes_string(x="x", y="y"), linetype="dashed") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("x") +
        ggplot2::ylab("interpolation function")

    return(plot)
}


