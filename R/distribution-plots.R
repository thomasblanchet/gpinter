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
plot_lorenz.gpinter_dist_orig <- function(dist, xlim, ...) {
    pmin <- xlim[1]
    pmax <- xlim[2]
    # Puts many point at the end if pmax == 1 to correctly draw the vertical
    # tangent near the end
    if (pmax == 1) {
        p_curve <- c(
            seq(pmin, 0.01*pmin + 0.99*pmax, length.out=180),
            seq(0.01*pmin + 0.99*pmax, pmax, length.out=20)
        )
    } else {
        p_curve <- seq(pmin, pmax, length.out=200)
    }
    p_point <- dist$pk_nc[(dist$pk_nc >= pmin) & (dist$pk_nc <= pmax)]

    if (pmax == 1 & max(p_point) < 1) {
        p_point <- c(p_point, 1)
    }
    if (pmin == 0 & min(p_point) > 0) {
        p_point <- c(0, p_point)
    }

    p_curve <- unique(c(p_curve, p_point))

    df_curve <- data.frame(
        p = p_curve,
        y = bottom_share(dist, p_curve)
    )
    df_point <- data.frame(
        p = p_point,
        y = bottom_share(dist, p_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="p", y="y")) +
        ggplot2::geom_abline(slope=1, linetype="dashed") +
        ggplot2::xlab("fraction of the population") +
        ggplot2::ylab("cumulative share")

    return(plot)
}

#' @export
plot_lorenz.gpinter_dist <- function(dist, xlim, ...) {
    pmin <- xlim[1]
    pmax <- xlim[2]
    # Puts many point at the end if pmax == 1 to correctly draw the vertical
    # tangent near the end
    if (pmax == 1) {
        p_curve <- c(
            seq(pmin, 0.01*pmin + 0.99*pmax, length.out=180),
            seq(0.01*pmin + 0.99*pmax, pmax, length.out=20)
        )
    } else {
        p_curve <- seq(pmin, pmax, length.out=200)
    }

    df_curve <- data.frame(
        p = p_curve,
        y = bottom_share(dist, p_curve)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_abline(slope=1, linetype="dashed") +
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
plot_gpc.gpinter_dist_orig <- function(dist, xlim, ...) {
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_curve <- seq(pmin, pmax, length.out=200)
    p_curve <- p_curve[p_curve != 1]
    p_point <- dist$pk_nc[(dist$pk_nc >= pmin) & (dist$pk_nc <= pmax)]

    p_curve <- unique(c(p_curve, p_point))

    df_curve <- data.frame(
        p = p_curve,
        y = invpareto(dist, p_curve)
    )
    df_point <- data.frame(
        p = p_point,
        y = invpareto(dist, p_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="p", y="y")) +
        ggplot2::xlab("p") +
        ggplot2::ylab("inverted Pareto coefficient")

    return(plot)
}

#' @export
plot_gpc.gpinter_dist <- function(dist, xlim, ...) {
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_curve <- seq(pmin, pmax, length.out=100)
    p_curve <- p_curve[p_curve != 1]

    df_curve <- data.frame(
        p = p_curve,
        y = invpareto(dist, p_curve)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
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
plot_cdf.gpinter_dist_orig <- function(dist, xlim, ...) {
    qmin <- xlim[1]
    qmax <- xlim[2]
    q_curve <- seq(qmin, qmax, length.out=200)
    q_point <- dist$qk_nc[(dist$qk_nc >= qmin) & (dist$qk_nc <= qmax)]
    q_curve <- unique(q_curve, q_point)

    df_curve <- data.frame(
        x = q_curve,
        y = fitted_cdf(dist, q_curve)
    )
    df_point <- data.frame(
        x = q_point,
        y = fitted_cdf(dist, q_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("x") +
        ggplot2::ylab("cumulative distribution function")

    return(plot)
}

#' @export
plot_cdf.gpinter_dist <- function(dist, xlim, ...) {
    qmin <- xlim[1]
    qmax <- xlim[2]
    q_curve <- seq(qmin, qmax, length.out=200)

    df_curve <- data.frame(
        x = q_curve,
        y = fitted_cdf(dist, q_curve)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::xlab("x") +
        ggplot2::ylab("cumulative distribution function")

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
plot_quantile.gpinter_dist_orig <- function(dist, xlim, ...) {
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_curve <- seq(pmin, pmax, length.out=200)
    p_point <- dist$pk_nc[(dist$pk_nc >= pmin) & (dist$pk_nc <= pmax)]

    if (pmax == 1 & max(p_point) < 1) {
        p_point <- c(p_point, 1)
    }
    if (pmin == 0 & min(p_point) > 0) {
        p_point <- c(0, p_point)
    }

    p_curve <- unique(c(p_curve, p_point))

    df_curve <- data.frame(
        p = p_curve,
        y = fitted_quantile(dist, p_curve)
    )
    df_point <- data.frame(
        p = p_point,
        y = fitted_quantile(dist, p_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="p", y="y")) +
        ggplot2::xlab("p") +
        ggplot2::ylab("quantile")

    return(plot)
}

#' @export
plot_quantile.gpinter_dist <- function(dist, xlim, ...) {
    pmin <- xlim[1]
    pmax <- xlim[2]
    p_curve <- seq(pmin, pmax, length.out=200)

    df_curve <- data.frame(
        p = p_curve,
        y = fitted_quantile(dist, p_curve)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="p", y="y"), linetype="solid") +
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
plot_tail.gpinter_dist_orig <- function(dist, xlim, ...) {
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_curve <- seq(xmin, xmax, length.out=200)
    x_point <- dist$xk_nc[(dist$xk_nc >= xmin) & (dist$xk_nc <= xmax)]
    x_curve <- unique(c(x_curve, x_point))

    df_curve <- data.frame(
        x = x_curve,
        y = log(fitted_quantile(dist, 1 - exp(-x_curve)))
    )
    df_point <- data.frame(
        x = x_point,
        y = log(fitted_quantile(dist, 1 - exp(-x_point)))
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("-log(1 - p)") +
        ggplot2::ylab("log(Q(p))")

    return(plot)
}

#' @export
plot_tail.gpinter_dist <- function(dist, xlim, ...) {
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_curve <- seq(xmin, xmax, length.out=200)

    df_curve <- data.frame(
        x = x_curve,
        y = log(fitted_quantile(dist, 1 - exp(-x_curve)))
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
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
plot_phi.gpinter_dist_orig <- function(dist, xlim, ...) {
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_curve <- seq(xmin, xmax, length.out=200)
    x_point <- dist$xk_nc[(dist$xk_nc >= xmin) & (dist$xk_nc <= xmax)]
    x_curve <- unique(c(x_curve, x_point))

    df_curve <- data.frame(
        x = x_curve,
        y = phi(dist, x_curve)
    )
    df_point <- data.frame(
        x = x_point,
        y = phi(dist, x_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("x") +
        ggplot2::ylab("interpolation function")

    return(plot)
}

#' @export
plot_phi.gpinter_dist <- function(dist, xlim, ...) {
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_curve <- seq(xmin, xmax, length.out=200)

    df_curve <- data.frame(
        x = x_curve,
        y = phi(dist, x_curve)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
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
plot_deriv_phi.gpinter_dist_orig <- function(dist, xlim, ...) {
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_curve <- seq(xmin, xmax, length.out=200)
    x_point <- dist$xk_nc[(dist$xk_nc >= xmin) & (dist$xk_nc <= xmax)]
    x_curve <- unique(c(x_curve, x_point))

    df_curve <- data.frame(
        x = x_curve,
        y = deriv_phi(dist, x_curve)
    )
    df_point <- data.frame(
        x = x_point,
        y = deriv_phi(dist, x_point)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::geom_point(data=df_point, ggplot2::aes_string(x="x", y="y")) +
        ggplot2::xlab("x") +
        ggplot2::ylab("derivative of interpolation function")

    return(plot)
}

#' @export
plot_deriv_phi.gpinter_dist <- function(dist, xlim, ...) {
    xmin <- xlim[1]
    xmax <- xlim[2]
    x_curve <- seq(xmin, xmax, length.out=200)

    df_curve <- data.frame(
        x = x_curve,
        y = deriv_phi(dist, x_curve)
    )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=df_curve, ggplot2::aes_string(x="x", y="y"), linetype="solid") +
        ggplot2::xlab("x") +
        ggplot2::ylab("derivative of interpolation function")

    return(plot)
}



