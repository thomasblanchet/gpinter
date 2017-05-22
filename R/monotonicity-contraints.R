#' @title Fast check of monotonicity constraint
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Check that a spline correspond to an increasing quantile
#' function using a fast to compute (but slightly over-restrictive) set of
#' sufficient conditions.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.

is_increasing_fast <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(all(mono_cns(x0, x1, y0, y1, s0, s1, a0, a1) > 0))
}

#' @title “Slow” check of monotonicity constraint
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Check that a spline correspond to an increasing quantile
#' function using a precise but somewhat computationally intensive method.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.
#' @param m The number of evaluation points of the polynomial: the bigger,
#' the slower and the tighter the bounds.

is_increasing_slow <- function(x0, x1, y0, y1, s0, s1, a0, a1, m=1e4) {
    x_eval <- seq(x0, x1, length.out=m)
    phid2 <- hd2(x_eval, x0, x1, y0, y1, s0, s1, a0, a1)
    phid1 <- hd1(x_eval, x0, x1, y0, y1, s0, s1, a0, a1)
    return(all(phid2 + phid1*(1 - phid1) > 0))
}

#' @title Check of monotonicity constraint
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Check that a spline correspond to an increasing quantile
#' function, first using the fast condition, then the slower one if necessary.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.
#' @param m The number of evaluation points of the polynomial: the bigger,
#' the slower and the tighter the bounds.

is_increasing <- function(x0, x1, y0, y1, s0, s1, a0, a1, m=1e4) {
    if (is_increasing_fast(x0, x1, y0, y1, s0, s1, a0, a1)) {
        return(TRUE)
    } else if (is_increasing_slow(x0, x1, y0, y1, s0, s1, a0, a1)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @title Distance between spline and spline perturbated by one point
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description These functions calculate the distance between:
#' \itemize{
#'     \item A first spline characterized by two points.
#'     \item A second set of two splines characterized by the same two points,
#'     plus an additional point between the first two.
#' }
#' They also calculate the derivative of that distance with respect to the
#' coordinates of the additional point.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.
#' @param x_new The position of the additional point.
#' @param y_new The value of the second spline at the additional point.
#' @param s_new The slope of the second spline at the additional point.
#' @param a_new The second derivative of the second spline at the additional point.
#' @param y_ini The value of the first spline at \code{x_new}.
#' @param s_ini The slope of the first spline at \code{x_new}.
#' @param a_ini The second derivative of the first spline at \code{x_new}.

distance_one_point <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                               x_new, y_new, s_new, a_new, y_ini, s_ini, a_ini) {
    return(
        distance_spline(x0, x_new,
            y0, y_ini, s0, s_ini, a0, a_ini,
            y0, y_new, s0, s_new, a0, a_new
        ) +
        distance_spline(x_new, x1,
            y_ini, y1, s_ini, s1, a_ini, a1,
            y_new, y1, s_new, s1, a_new, a1
        )
    )
}

distance_one_point_grad <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                    x_new, y_new, s_new, a_new, y_ini, s_ini, a_ini) {
    return(c(
        distance_spline_deriv_y1new(x0, x_new,
            y0, y_ini, s0, s_ini, a0, a_ini,
            y0, y_new, s0, s_new, a0, a_new
        ) +
        distance_spline_deriv_y0new(x_new, x1,
            y_ini, y1, s_ini, s1, a_ini, a1,
            y_new, y1, s_new, s1, a_new, a1
        ),

        distance_spline_deriv_s1new(x0, x_new,
            y0, y_ini, s0, s_ini, a0, a_ini,
            y0, y_new, s0, s_new, a0, a_new
        ) +
        distance_spline_deriv_s0new(x_new, x1,
            y_ini, y1, s_ini, s1, a_ini, a1,
            y_new, y1, s_new, s1, a_new, a1
        ),

        distance_spline_deriv_a1new(x0, x_new,
            y0, y_ini, s0, s_ini, a0, a_ini,
            y0, y_new, s0, s_new, a0, a_new
        ) +
        distance_spline_deriv_a0new(x_new, x1,
            y_ini, y1, s_ini, s1, a_ini, a1,
            y_new, y1, s_new, s1, a_new, a1
        )
    ))
}

#' @title Distance between spline and spline perturbated by two points
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description These functions calculate the distance between:
#' \itemize{
#'     \item A first spline characterized by two points.
#'     \item A second set of three splines characterized by the same two points,
#'     plus two additional points between the first two.
#' }
#' They also calculate the derivative of that distance with respect to the
#' coordinates of the additionals points.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.
#' @param x_new1 The position of the first additional point.
#' @param y_new1 The value of the second spline at the first additional point.
#' @param s_new1 The slope of the second spline at the first additional point.
#' @param a_new1 The second derivative of the second spline at the first additional point.
#' @param y_ini1 The value of the first spline at \code{x_new1}.
#' @param s_ini1 The slope of the first spline at \code{x_new1}.
#' @param a_ini1 The second derivative of the first spline at \code{x_new1}.
#' @param x_new2 The position of the second additional point.
#' @param y_new2 The value of the second spline at the second additional point.
#' @param s_new2 The slope of the second spline at the second additional point.
#' @param a_new2 The second derivative of the second spline at the second additional point.
#' @param y_ini2 The value of the first spline at \code{x_new2}.
#' @param s_ini2 The slope of the first spline at \code{x_new2}.
#' @param a_ini2 The second derivative of the first spline at \code{x_new2}.

distance_two_points <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                x_new1, y_new1, s_new1, a_new1, y_ini1, s_ini1, a_ini1,
                                x_new2, y_new2, s_new2, a_new2, y_ini2, s_ini2, a_ini2) {
    return(
        distance_spline(x0, x_new1,
            y0, y_ini1, s0, s_ini1, a0, a_ini1,
            y0, y_new1, s0, s_new1, a0, a_new1
        ) +
        distance_spline(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ) +
        distance_spline(x_new2, x1,
            y_ini2, y1, s_ini2, s1, a_ini2, a1,
            y_new2, y1, s_new2, s1, a_new2, a1
        )
    )
}

distance_two_points_grad <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                     x_new1, y_new1, s_new1, a_new1, y_ini1, s_ini1, a_ini1,
                                     x_new2, y_new2, s_new2, a_new2, y_ini2, s_ini2, a_ini2) {
    return(c(
        distance_spline_deriv_y1new(x0, x_new1,
            y0, y_ini1, s0, s_ini1, a0, a_ini1,
            y0, y_new1, s0, s_new1, a0, a_new1
        ) +
        distance_spline_deriv_y0new(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ),

        distance_spline_deriv_s1new(x0, x_new1,
            y0, y_ini1, s0, s_ini1, a0, a_ini1,
            y0, y_new1, s0, s_new1, a0, a_new1
        ) +
        distance_spline_deriv_s0new(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ),

        distance_spline_deriv_a1new(x0, x_new1,
            y0, y_ini1, s0, s_ini1, a0, a_ini1,
            y0, y_new1, s0, s_new1, a0, a_new1
        ) +
        distance_spline_deriv_a0new(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ),

        distance_spline_deriv_y1new(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ) +
        distance_spline_deriv_y0new(x_new2, x1,
            y_ini2, y1, s_ini2, s1, a_ini2, a1,
            y_new2, y1, s_new2, s1, a_new2, a1
        ),

        distance_spline_deriv_s1new(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ) +
        distance_spline_deriv_s0new(x_new2, x1,
            y_ini2, y1, s_ini2, s1, a_ini2, a1,
            y_new2, y1, s_new2, s1, a_new2, a1
        ),

        distance_spline_deriv_a1new(x_new1, x_new2,
            y_ini1, y_ini2, s_ini1, s_ini2, a_ini1, a_ini2,
            y_new1, y_new2, s_new1, s_new2, a_new1, a_new2
        ) +
        distance_spline_deriv_a0new(x_new2, x1,
            y_ini2, y1, s_ini2, s1, a_ini2, a1,
            y_new2, y1, s_new2, s1, a_new2, a1
        )
    ))
}

#' @title Monotonicity constraints with one additional point
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Set of constraints which must all be negative to ensure
#' an increasing quantile function, when adding one additional point to a
#' spline.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.
#' @param x_new The position of the additional point.
#' @param y_new The value of the second spline at the additional point.
#' @param s_new The slope of the second spline at the additional point.
#' @param a_new The second derivative of the second spline at the additional point.

cns_one_point <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                          x_new, y_new, s_new, a_new) {
    return(c(
        -mono_cns(
            x0, x_new,
            y0, y_new,
            s0, s_new,
            a0, a_new
        ),
        -mono_cns(
            x_new, x1,
            y_new, y1,
            s_new, s1,
            a_new, a1
        )
    ))
}

cns_one_point_jac <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                              x_new, y_new, s_new, a_new) {
    return(cbind(
        c(
            -mono_cns_deriv_y1(
                x0, x_new,
                y0, y_new,
                s0, s_new,
                a0, a_new
            ),
            -mono_cns_deriv_y0(
                x_new, x1,
                y_new, y1,
                s_new, s1,
                a_new, a1
            )
        ),
        c(
            -mono_cns_deriv_s1(
                x0, x_new,
                y0, y_new,
                s0, s_new,
                a0, a_new
            ),
            -mono_cns_deriv_s0(
                x_new, x1,
                y_new, y1,
                s_new, s1,
                a_new, a1
            )
        ),
        c(
            -mono_cns_deriv_a1(
                x0, x_new,
                y0, y_new,
                s0, s_new,
                a0, a_new
            ),
            -mono_cns_deriv_a0(
                x_new, x1,
                y_new, y1,
                s_new, s1,
                a_new, a1
            )
        )
    ))
}

#' @title Monotonicity constraints with two additional points
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Set of constraints which must all be negative to ensure
#' an increasing quantile function, when adding two additional points to a
#' spline.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the first spline at \code{x0}.
#' @param y1 Value of the first spline at \code{x1}.
#' @param s0 Slope of the first spline at \code{x0}.
#' @param s1 Slope of the first spline at \code{x1}.
#' @param a0 Second derivative of the first spline at \code{x0}.
#' @param a1 Second derivative of the first spline at \code{x1}.
#' @param x_new1 The position of the first additional point.
#' @param y_new1 The value of the second spline at the first additional point.
#' @param s_new1 The slope of the second spline at the first additional point.
#' @param a_new1 The second derivative of the second spline at the first additional point.
#' @param x_new2 The position of the second additional point.
#' @param y_new2 The value of the second spline at the second additional point.
#' @param s_new2 The slope of the second spline at the second additional point.
#' @param a_new2 The second derivative of the second spline at the second additional point.

cns_two_points <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                           x_new1, y_new1, s_new1, a_new1,
                           x_new2, y_new2, s_new2, a_new2) {
    return(c(
        -mono_cns(
            x0, x_new1,
            y0, y_new1,
            s0, s_new1,
            a0, a_new1
        ),
        -mono_cns(
            x_new1, x_new2,
            y_new1, y_new2,
            s_new1, s_new2,
            a_new1, a_new2
        ),
        -mono_cns(
            x_new2, x1,
            y_new2, y1,
            s_new2, s1,
            a_new2, a1
        )
    ))
}

cns_two_points_jac <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                               x_new1, y_new1, s_new1, a_new1,
                               x_new2, y_new2, s_new2, a_new2) {
    return(cbind(
        c(
            -mono_cns_deriv_y1(
                x0, x_new1,
                y0, y_new1,
                s0, s_new1,
                a0, a_new1
            ),
            -mono_cns_deriv_y0(
                x_new1, x_new2,
                y_new1, y_new2,
                s_new1, s_new2,
                a_new1, a_new2
            ),
            rep(0, 9)
        ),
        c(
            -mono_cns_deriv_s1(
                x0, x_new1,
                y0, y_new1,
                s0, s_new1,
                a0, a_new1
            ),
            -mono_cns_deriv_s0(
                x_new1, x_new2,
                y_new1, y_new2,
                s_new1, s_new2,
                a_new1, a_new2
            ),
            rep(0, 9)
        ),
        c(
            -mono_cns_deriv_a1(
                x0, x_new1,
                y0, y_new1,
                s0, s_new1,
                a0, a_new1
            ),
            -mono_cns_deriv_a0(
                x_new1, x_new2,
                y_new1, y_new2,
                s_new1, s_new2,
                a_new1, a_new2
            ),
            rep(0, 9)
        ),
        c(
            rep(0, 9),
            -mono_cns_deriv_y1(
                x_new1, x_new2,
                y_new1, y_new2,
                s_new1, s_new2,
                a_new1, a_new2
            ),
            -mono_cns_deriv_y0(
                x_new2, x1,
                y_new2, y1,
                s_new2, s1,
                a_new2, a1
            )
        ),
        c(
            rep(0, 9),
            -mono_cns_deriv_s1(
                x_new1, x_new2,
                y_new1, y_new2,
                s_new1, s_new2,
                a_new1, a_new2
            ),
            -mono_cns_deriv_s0(
                x_new2, x1,
                y_new2, y1,
                s_new2, s1,
                a_new2, a1
            )
        ),
        c(
            rep(0, 9),
            -mono_cns_deriv_a1(
                x_new1, x_new2,
                y_new1, y_new2,
                s_new1, s_new2,
                a_new1, a_new2
            ),
            -mono_cns_deriv_a0(
                x_new2, x1,
                y_new2, y1,
                s_new2, s1,
                a_new2, a1
            )
        )
    ))
}

#' @title Add one point in order to satisfy monotonicity constraint
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Add an additional point to a spline to ensure monotonicity
#' of the quantile function.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0}.
#' @param y1 Value of the spline at \code{x1}.
#' @param s0 Slope of the spline at \code{x0}.
#' @param s1 Slope of the spline at \code{x1}.
#' @param a0 Second derivative of the spline at \code{x0}.
#' @param a1 Second derivative of the spline at \code{x1}.
#'
#' @return A list with four components:
#' \describe{
#'     \item{\code{x_new}}{The position of the new point.}
#'     \item{\code{y_new}}{The value of the spline at the new point.}
#'     \item{\code{s_new}}{The slope of the spline at the new point.}
#'     \item{\code{a_new}}{The second derivative of the spline at the new point.}
#' }

add_one_point <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    x_new <- (x0 + x1)/2

    y_ini <- h(x_new, x0, x1, y0, y1, s0, s1, a0, a1)
    s_ini <- hd1(x_new, x0, x1, y0, y1, s0, s1, a0, a1)
    a_ini <- hd2(x_new, x0, x1, y0, y1, s0, s1, a0, a1)

    new_pt <- nloptr::nloptr(
        opts = list(
            maxeval = 1e4,
            algorithm = "NLOPT_LD_SLSQP",
            xtol_rel = sqrt(.Machine$double.eps)
        ),
        x0 = c(y_ini, s_ini, a_ini),
        eval_f = function(theta) {
            return(distance_one_point(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, theta[1], theta[2], theta[3],
                y_ini, s_ini, a_ini
            ))
        },
        eval_grad_f = function(theta) {
            return(distance_one_point_grad(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, theta[1], theta[2], theta[3],
                y_ini, s_ini, a_ini
            ))
        },
        eval_g_ineq = function(theta) {
            return(cns_one_point(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, theta[1], theta[2], theta[3]
            ))
        },
        eval_jac_g_ineq = function(theta) {
            return(cns_one_point_jac(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, theta[1], theta[2], theta[3]
            ))
        }
    )

    # Only return the solution if the algorithm converged
    if (new_pt$status == 4) {
        return(list(
            x_new = x_new,
            y_new = new_pt$solution[1],
            s_new = new_pt$solution[2],
            a_new = new_pt$solution[3]
        ))
    } else {
        return(NULL)
    }
}

#' @title Add two points in order to satisfy monotonicity constraint
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Add two additional points to a spline to ensure monotonicity
#' of the quantile function.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0}.
#' @param y1 Value of the spline at \code{x1}.
#' @param s0 Slope of the spline at \code{x0}.
#' @param s1 Slope of the spline at \code{x1}.
#' @param a0 Second derivative of the spline at \code{x0}.
#' @param a1 Second derivative of the spline at \code{x1}.
#'
#' @return A list with eight components:
#' \describe{
#'     \item{\code{x_new1}}{The position of the first new point.}
#'     \item{\code{y_new1}}{The value of the spline at \code{x_new1}.}
#'     \item{\code{s_new1}}{The slope of the spline at \code{x_new1}.}
#'     \item{\code{a_new1}}{The second derivative of the spline at \code{x_new1}.}
#'     \item{\code{x_new2}}{The position of the second new point.}
#'     \item{\code{y_new2}}{The value of the spline at \code{x_new2}.}
#'     \item{\code{s_new2}}{The slope of the spline at \code{x_new2}.}
#'     \item{\code{a_new2}}{The second derivative of the spline at \code{x_new2}.}
#' }

add_two_points <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    x_new1 <- (2*x0 + x1)/3
    x_new2 <- (x0 + 2*x1)/3

    y_ini1 <- h(x_new1, x0, x1, y0, y1, s0, s1, a0, a1)
    y_ini2 <- h(x_new2, x0, x1, y0, y1, s0, s1, a0, a1)
    s_ini1 <- hd1(x_new1, x0, x1, y0, y1, s0, s1, a0, a1)
    s_ini2 <- hd1(x_new2, x0, x1, y0, y1, s0, s1, a0, a1)
    a_ini1 <- hd2(x_new1, x0, x1, y0, y1, s0, s1, a0, a1)
    a_ini2 <- hd2(x_new2, x0, x1, y0, y1, s0, s1, a0, a1)

    init <- c(
        y_ini1, s_ini1, a_ini1,
        y_ini2, s_ini2, a_ini2
    )

    new_pts <- nloptr::nloptr(
        opts = list(
            maxeval = 1e4,
            algorithm = "NLOPT_LD_SLSQP",
            xtol_rel = sqrt(.Machine$double.eps)
        ),
        x0 = c(y_ini1, s_ini1, a_ini1, y_ini2, s_ini2, a_ini2),
        eval_f = function(theta) {
            return(distance_two_points(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, theta[1], theta[2], theta[3], y_ini1, s_ini1, a_ini1,
                x_new2, theta[4], theta[5], theta[6], y_ini2, s_ini2, a_ini2
            ))
        },
        eval_grad_f = function(theta) {
            return(distance_two_points_grad(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, theta[1], theta[2], theta[3], y_ini1, s_ini1, a_ini1,
                x_new2, theta[4], theta[5], theta[6], y_ini2, s_ini2, a_ini2
            ))
        },
        eval_g_ineq = function(theta) {
            return(cns_two_points(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, theta[1], theta[2], theta[3],
                x_new2, theta[4], theta[5], theta[6]
            ))
        },
        eval_jac_g_ineq = function(theta) {
            return(cns_two_points_jac(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, theta[1], theta[2], theta[3],
                x_new2, theta[4], theta[5], theta[6]
            ))
        }
    )
    if (new_pts$status == 4) {
        return(list(
            x_new1 = x_new1,
            y_new1 = new_pts$solution[1],
            s_new1 = new_pts$solution[2],
            a_new1 = new_pts$solution[3],
            x_new2 = x_new2,
            y_new2 = new_pts$solution[4],
            s_new2 = new_pts$solution[5],
            a_new2 = new_pts$solution[6]
        ))
    } else {
        return(NULL)
    }
}
