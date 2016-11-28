#' @title Monotonicity constraint for the quantile function
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description This set of functions provide constraints on the parameters
#' of the quintic spline that ensure an increasing quantile function. The
#' function \code{mono_cns} returns nine values that must all be non-negative.
#' The other are the derivatives of those constraints with respect to each
#' parameters. Those conditions were derived using the Bernstein polynomial
#' form of the constraint (see Mathematica workbook for details).
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
#' @return The value of the nine constraints or their derivative.
#'
#' @rdname mono_cns
#' @name mono_cns
#' @aliases mono_cns mono_cns_deriv_y0 mono_cns_deriv_y1 mono_cns_deriv_s0
#' mono_cns_deriv_s1 mono_cns_deriv_a0 mono_cns_deriv_a1

mono_cns <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        a0 + s0 - s0^2,
        a0 + s0 - s0^2 + ((x0 - x1)*(24*s1 + 2*s0*(18 + a0*(x0 - x1)^2) + (x0 - x1)*(3*a1 + a0*(-9 - x0 + x1))) + 60*(-y0 + y1))/(8*(x0 - x1)^2),
        (-480*(y0 - y1) + (x0 - x1)*(16*s0^2*(x0 - x1) - (a0*(34 + 5*x0 - 5*x1) + 3*a1*(-6 + x0 - x1) + 2*a0^2*(x0 - x1)^2)*(x0 - x1) + 24*s1*(7 - x0 + x1) + 60*(y0 - y1) + 2*s0*(156 + (5*a0 + 3*a1)*x0^2 + 2*x0*(5 + 12*s1 - 5*a0*x1 - 3*a1*x1) + x1*(-10 - 24*s1 + 5*a0*x1 + 3*a1*x1) - 60*y0 + 60*y1)))/(56*(x0 - x1)^2),
        (-300*(y0 - y1) + (x0 - x1)*(96*s0^2*(x0 - x1) + 4*s1*(15 - 6*a0*x0^2 + x1*(11 - 6*a0*x1) + x0*(-11 + 12*a0*x1)) + 2*s0*((-18*a0 + 5*a1)*x0^2 + 2*x0*(-5 + 22*s1 + 18*a0*x1 - 5*a1*x1) + x1*(10 - 44*s1 - 18*a0*x1 + 5*a1*x1) - 120*(-1 + y0 - y1)) + 120*(y0 - y1) + (x0 - x1)*(a1 - 5*a1*x0 + 3*a0^2*(x0 - x1)^2 + 5*a1*x1 - a0*(35 + 3*a1*(x0 - x1)^2 - 60*y0 + 60*y1))))/(56*(x0 - x1)^2),
        -(100*a0*x0^2 + 100*a1*x0^2 - 20*a0*x0^3 + 20*a1*x0^3 + 9*a0^2*x0^4 - 26*a0*a1*x0^4 + 9*a1^2*x0^4 + 576*s0^2*(x0 - x1)^2 + 576*s1^2*(x0 - x1)^2 - 200*a0*x0*x1 - 200*a1*x0*x1 + 60*a0*x0^2*x1 - 60*a1*x0^2*x1 - 36*a0^2*x0^3*x1 + 104*a0*a1*x0^3*x1 - 36*a1^2*x0^3*x1 + 100*a0*x1^2 + 100*a1*x1^2 - 60*a0*x0*x1^2 + 60*a1*x0*x1^2 + 54*a0^2*x0^2*x1^2 - 156*a0*a1*x0^2*x1^2 + 54*a1^2*x0^2*x1^2 + 20*a0*x1^3 - 20*a1*x1^3 - 36*a0^2*x0*x1^3 + 104*a0*a1*x0*x1^3 - 36*a1^2*x0*x1^3 + 9*a0^2*x1^4 - 26*a0*a1*x1^4 + 9*a1^2*x1^4 - 720*x0*y0 + 360*a0*x0^2*y0 - 360*a1*x0^2*y0 + 720*x1*y0 - 720*a0*x0*x1*y0 + 720*a1*x0*x1*y0 + 360*a0*x1^2*y0 - 360*a1*x1^2*y0 + 3600*y0^2 - 4*s1*(x0 - x1)*(4*(11*a0 - 9*a1)*x0^2 + x1*(55 + 44*a0*x1 - 36*a1*x1) + x0*(-55 - 88*a0*x1 + 72*a1*x1) + 120*(-1 + 6*y0 - 6*y1)) - 4*s0*(x0 - x1)*(4*(9*a0 - 11*a1)*x0^2 - x0*(55 + 322*s1 + 72*a0*x1 - 88*a1*x1) + x1*(55 + 322*s1 + 36*a0*x1 - 44*a1*x1) + 120*(1 + 6*y0 - 6*y1)) - 360*((-2 + (a0 - a1)*(x0 - x1))*(x0 - x1) + 20*y0)*y1 + 3600*y1^2)/(280*(x0 - x1)^2),
        ((x0 - x1)*(96*s1^2*(x0 - x1) + 4*s0*(-15 + 6*a1*x0^2 + x0*(-11 + 22*s1 - 12*a1*x1) + x1*(11 - 22*s1 + 6*a1*x1)) - (x0 - x1)*(a0*(-1 + x0*(-5 + 3*a1*x0) + 5*x1 - 6*a1*x0*x1 + 3*a1*x1^2) + a1*(35 - 3*a1*(x0 - x1)^2 + 60*y0 - 60*y1)) - 2*s1*((5*a0 - 18*a1)*x0^2 + x1*(-10 + 5*a0*x1 - 18*a1*x1) + 2*x0*(5 - 5*a0*x1 + 18*a1*x1) + 120*(1 + y0 - y1)) + 120*(y0 - y1)) + 300*(y0 - y1))/(56*(x0 - x1)^2),
        (480*(y0 - y1) - (x0 - x1)*((34*a1 + a1*(-5 + 2*a1*(x0 - x1))*(x0 - x1) - 3*a0*(6 + x0 - x1))*(x0 - x1) + 16*s1^2*(-x0 + x1) - 24*s0*(-7 + (-1 + 2*s1)*x0 + x1 - 2*s1*x1) + 2*s1*((3*a0 + 5*a1)*x0^2 - 2*x0*(5 + 3*a0*x1 + 5*a1*x1) + x1*(10 + 3*a0*x1 + 5*a1*x1) + 12*(13 + 5*y0 - 5*y1)) + 60*(-y0 + y1)))/(56*(x0 - x1)^2),
        (-((x0 - x1)*(24*s0 + 8*s1^2*(x0 - x1) - (3*a0 + a1*(-1 + x0 - x1))*(x0 - x1) + 2*s1*(18 + a1*x0^2 - 2*x0*(2 + a1*x1) + x1*(4 + a1*x1)))) + 60*(y0 - y1))/(8*(x0 - x1)^2),
        a1 + s1 - s1^2
    ))
}

mono_cns_deriv_y0 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        0,
        -15/(2*(x0 - x1)^2),
        (-15*(8 + (-1 + 2*s0)*x0 + x1 - 2*s0*x1))/(14*(x0 - x1)^2),
        (15*(-5 + (2 - 4*s0 + a0*(x0 - x1))*(x0 - x1)))/(14*(x0 - x1)^2),
        (-9*((a0 - a1)*x0^2 + (2 + 8*s0 + 8*s1)*x1 + (a0 - a1)*x1^2 - 2*x0*(1 + 4*s0 + 4*s1 + a0*x1 - a1*x1) + 20*(y0 - y1)))/(7*(x0 - x1)^2),
        (300 - 60*(-2 + 4*s1 + a1*(x0 - x1))*(x0 - x1))/(56*(x0 - x1)^2),
        (-15*(-8 + (-1 + 2*s1)*x0 + x1 - 2*s1*x1))/(14*(x0 - x1)^2),
        15/(2*(x0 - x1)^2),
        0
    ))
}

mono_cns_deriv_y1 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        0,
        15/(2*(x0 - x1)^2),
        (15*(8 + (-1 + 2*s0)*x0 + x1 - 2*s0*x1))/(14*(x0 - x1)^2),
        (15*(5 + (x0 - x1)*(-2 + 4*s0 + a0*(-x0 + x1))))/(14*(x0 - x1)^2),
        (9*((a0 - a1)*x0^2 + (2 + 8*s0 + 8*s1)*x1 + (a0 - a1)*x1^2 - 2*x0*(1 + 4*s0 + 4*s1 + a0*x1 - a1*x1) + 20*(y0 - y1)))/(7*(x0 - x1)^2),
        (15*(-5 + (-2 + 4*s1 + a1*(x0 - x1))*(x0 - x1)))/(14*(x0 - x1)^2),
        (15*(-8 + (-1 + 2*s1)*x0 + x1 - 2*s1*x1))/(14*(x0 - x1)^2),
        -15/(2*(x0 - x1)^2),
        0
    ))
}

mono_cns_deriv_s0 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        1 - 2*s0,
        1 - 2*s0 + (18 + a0*(x0 - x1)^2)/(4*(x0 - x1)),
        (156 + (5*a0 + 3*a1)*x0^2 - 2*(5 + 8*s0 + 12*s1)*x1 + (5*a0 + 3*a1)*x1^2 + 2*x0*(5 + 8*s0 + 12*s1 - 5*a0*x1 - 3*a1*x1) - 60*y0 + 60*y1)/(28*(x0 - x1)),
        ((-18*a0 + 5*a1)*x0^2 - 2*(-5 + 48*s0 + 22*s1)*x1 + (-18*a0 + 5*a1)*x1^2 + 2*x0*(-5 + 48*s0 + 22*s1 + 18*a0*x1 - 5*a1*x1) - 120*(-1 + y0 - y1))/(28*(x0 - x1)),
        (4*(9*a0 - 11*a1)*x0^2 + (55 + 288*s0 + 322*s1)*x1 + 4*(9*a0 - 11*a1)*x1^2 - x0*(55 + 288*s0 + 322*s1 + 72*a0*x1 - 88*a1*x1) + 120*(1 + 6*y0 - 6*y1))/(70*(x0 - x1)),
        (-15 + 6*a1*x0^2 + x0*(-11 + 22*s1 - 12*a1*x1) + x1*(11 - 22*s1 + 6*a1*x1))/(14*(x0 - x1)),
        (3*(-7 + (-1 + 2*s1)*x0 + x1 - 2*s1*x1))/(7*(x0 - x1)),
        -3/(x0 - x1),
        0
    ))
}

mono_cns_deriv_s1 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        0,
        3/(x0 - x1),
        (3*(7 + (-1 + 2*s0)*x0 + x1 - 2*s0*x1))/(7*(x0 - x1)),
        (15 - 6*a0*x0^2 + (11 - 22*s0)*x1 - 6*a0*x1^2 + x0*(-11 + 22*s0 + 12*a0*x1))/(14*(x0 - x1)),
        (4*(11*a0 - 9*a1)*x0^2 + (55 + 322*s0 + 288*s1)*x1 + 4*(11*a0 - 9*a1)*x1^2 - x0*(55 + 322*s0 + 288*s1 + 88*a0*x1 - 72*a1*x1) + 120*(-1 + 6*y0 - 6*y1))/(70*(x0 - x1)),
        ((-5*a0 + 18*a1)*x0^2 - 2*(-5 + 22*s0 + 48*s1)*x1 + (-5*a0 + 18*a1)*x1^2 + 2*x0*(-5 + 22*s0 + 48*s1 + 5*a0*x1 - 18*a1*x1) - 120*(1 + y0 - y1))/(28*(x0 - x1)),
        -((3*a0 + 5*a1)*x0^2 + 2*(5 + 12*s0 + 8*s1)*x1 + (3*a0 + 5*a1)*x1^2 - 2*x0*(5 + 12*s0 + 8*s1 + 3*a0*x1 + 5*a1*x1) + 12*(13 + 5*y0 - 5*y1))/(28*(x0 - x1)),
        -(18 + a1*x0^2 + 8*s1*(x0 - x1) - 2*x0*(2 + a1*x1) + x1*(4 + a1*x1))/(4*(x0 - x1)),
        1 - 2*s1
    ))
}

mono_cns_deriv_a0 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        1,
        (-1 + (-1 + 2*s0)*x0 + x1 - 2*s0*x1)/8,
        (-34 - 4*a0*x0^2 + (5 - 10*s0)*x1 - 4*a0*x1^2 + x0*(-5 + 10*s0 + 8*a0*x1))/56,
        (-35 + 6*a0*x0^2 - 3*a1*x0^2 - 36*s0*(x0 - x1) - 24*s1*(x0 - x1) - 12*a0*x0*x1 + 6*a1*x0*x1 + 6*a0*x1^2 - 3*a1*x1^2 + 60*y0 - 60*y1)/56,
        ((-9*a0 + 13*a1)*x0^2 - 2*(5 + 36*s0 + 44*s1)*x1 + (-9*a0 + 13*a1)*x1^2 + 2*x0*(5 + 36*s0 + 44*s1 + 9*a0*x1 - 13*a1*x1) - 10*(5 + 18*y0 - 18*y1))/140,
        (1 - 3*a1*x0^2 + 5*(-1 + 2*s1)*x1 - 3*a1*x1^2 + x0*(5 - 10*s1 + 6*a1*x1))/56,
        (-3*(-6 + (-1 + 2*s1)*x0 + x1 - 2*s1*x1))/56,
        3/8,
        0
    ))
}

mono_cns_deriv_a1 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(c(
        0,
        3/8,
        (3*(6 + (-1 + 2*s0)*x0 + x1 - 2*s0*x1))/56,
        (1 - 3*a0*x0^2 + (5 - 10*s0)*x1 - 3*a0*x1^2 + x0*(-5 + 10*s0 + 6*a0*x1))/56,
        ((13*a0 - 9*a1)*x0^2 + 2*(5 + 44*s0 + 36*s1)*x1 + (13*a0 - 9*a1)*x1^2 - 2*x0*(5 + 44*s0 + 36*s1 + 13*a0*x1 - 9*a1*x1) + 10*(-5 + 18*y0 - 18*y1))/140,
        (-35 - 3*a0*x0^2 + 6*a1*x0^2 + 24*s0*(x0 - x1) + 36*s1*(x0 - x1) + 6*a0*x0*x1 - 12*a1*x0*x1 - 3*a0*x1^2 + 6*a1*x1^2 - 60*y0 + 60*y1)/56,
        (-34 - 4*a1*x0^2 + 5*(-1 + 2*s1)*x1 - 4*a1*x1^2 + x0*(5 - 10*s1 + 8*a1*x1))/56,
        (-1 + x0 - 2*s1*x0 + (-1 + 2*s1)*x1)/8,
        1
    ))
}

#' @title Distance between two quintic splines
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculate the distance between two quintic splines, as
#' measured by the integral of the square of the difference. Also calculates
#' the derivatives of that distance with respect to the spline parameters of
#' the second function.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0} for the first spline.
#' @param y1 Value of the spline at \code{x1} for the first spline.
#' @param s0 Slope of the spline at \code{x0} for the first spline.
#' @param s1 Slope of the spline at \code{x1} for the first spline.
#' @param a0 Second derivative of the spline at \code{x0} for the first spline.
#' @param a1 Second derivative of the spline at \code{x1} for the first spline.
#' @param y0new Value of the spline at \code{x0} for the first spline.
#' @param y1new Value of the spline at \code{x1} for the first spline.
#' @param s0new Slope of the spline at \code{x0} for the first spline.
#' @param s1new Slope of the spline at \code{x1} for the first spline.
#' @param a0new Second derivative of the spline at \code{x0} for the first spline.
#' @param a1new Second derivative of the spline at \code{x1} for the first spline.
#'
#' @return The value of the distance or its derivatives.
#'
#' @rdname distance_spline
#' @name distance_spline
#' @aliases distance_spline
#' distance_spline_deriv_y0new distance_spline_deriv_y1new
#' distance_spline_deriv_s0new distance_spline_deriv_s1new
#' distance_spline_deriv_a0new distance_spline_deriv_a1new

distance_spline <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                            y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        -((x0 - x1)*(416*s1^2*x0^2 - 832*s1*s1new*x0^2 + 416*s1new^2*x0^2 + 52*a0*s1*x0^3 -
        52*a0new*s1*x0^3 + 69*a1*s1*x0^3 - 69*a1new*s1*x0^3 - 52*a0*s1new*x0^3 +
        52*a0new*s1new*x0^3 - 69*a1*s1new*x0^3 + 69*a1new*s1new*x0^3 + 3*a0^2*x0^4 -
        6*a0*a0new*x0^4 + 3*a0new^2*x0^4 + 5*a0*a1*x0^4 - 5*a0new*a1*x0^4 + 3*a1^2*x0^4 -
        5*a0*a1new*x0^4 + 5*a0new*a1new*x0^4 - 6*a1*a1new*x0^4 + 3*a1new^2*x0^4 +
        416*s0^2*(x0 - x1)^2 + 416*s0new^2*(x0 - x1)^2 - 832*s1^2*x0*x1 +
        1664*s1*s1new*x0*x1 - 832*s1new^2*x0*x1 - 156*a0*s1*x0^2*x1 +
        156*a0new*s1*x0^2*x1 - 207*a1*s1*x0^2*x1 + 207*a1new*s1*x0^2*x1 +
        156*a0*s1new*x0^2*x1 - 156*a0new*s1new*x0^2*x1 + 207*a1*s1new*x0^2*x1 -
        207*a1new*s1new*x0^2*x1 - 12*a0^2*x0^3*x1 + 24*a0*a0new*x0^3*x1 -
        12*a0new^2*x0^3*x1 - 20*a0*a1*x0^3*x1 + 20*a0new*a1*x0^3*x1 - 12*a1^2*x0^3*x1 +
        20*a0*a1new*x0^3*x1 - 20*a0new*a1new*x0^3*x1 + 24*a1*a1new*x0^3*x1 -
        12*a1new^2*x0^3*x1 + 416*s1^2*x1^2 - 832*s1*s1new*x1^2 + 416*s1new^2*x1^2 +
        156*a0*s1*x0*x1^2 - 156*a0new*s1*x0*x1^2 + 207*a1*s1*x0*x1^2 -
        207*a1new*s1*x0*x1^2 - 156*a0*s1new*x0*x1^2 + 156*a0new*s1new*x0*x1^2 -
        207*a1*s1new*x0*x1^2 + 207*a1new*s1new*x0*x1^2 + 18*a0^2*x0^2*x1^2 -
        36*a0*a0new*x0^2*x1^2 + 18*a0new^2*x0^2*x1^2 + 30*a0*a1*x0^2*x1^2 -
        30*a0new*a1*x0^2*x1^2 + 18*a1^2*x0^2*x1^2 - 30*a0*a1new*x0^2*x1^2 +
        30*a0new*a1new*x0^2*x1^2 - 36*a1*a1new*x0^2*x1^2 + 18*a1new^2*x0^2*x1^2 -
        52*a0*s1*x1^3 + 52*a0new*s1*x1^3 - 69*a1*s1*x1^3 + 69*a1new*s1*x1^3 +
        52*a0*s1new*x1^3 - 52*a0new*s1new*x1^3 + 69*a1*s1new*x1^3 - 69*a1new*s1new*x1^3 -
        12*a0^2*x0*x1^3 + 24*a0*a0new*x0*x1^3 - 12*a0new^2*x0*x1^3 - 20*a0*a1*x0*x1^3 +
        20*a0new*a1*x0*x1^3 - 12*a1^2*x0*x1^3 + 20*a0*a1new*x0*x1^3 -
        20*a0new*a1new*x0*x1^3 + 24*a1*a1new*x0*x1^3 - 12*a1new^2*x0*x1^3 + 3*a0^2*x1^4 -
        6*a0*a0new*x1^4 + 3*a0new^2*x1^4 + 5*a0*a1*x1^4 - 5*a0new*a1*x1^4 + 3*a1^2*x1^4 -
        5*a0*a1new*x1^4 + 5*a0new*a1new*x1^4 - 6*a1*a1new*x1^4 + 3*a1new^2*x1^4 +
        1812*s1*x0*y0 - 1812*s1new*x0*y0 + 281*a0*x0^2*y0 - 281*a0new*x0^2*y0 +
        181*a1*x0^2*y0 - 181*a1new*x0^2*y0 - 1812*s1*x1*y0 + 1812*s1new*x1*y0 -
        562*a0*x0*x1*y0 + 562*a0new*x0*x1*y0 - 362*a1*x0*x1*y0 + 362*a1new*x0*x1*y0 +
        281*a0*x1^2*y0 - 281*a0new*x1^2*y0 + 181*a1*x1^2*y0 - 181*a1new*x1^2*y0 +
        10860*y0^2 - 1812*s1*x0*y0new + 1812*s1new*x0*y0new - 281*a0*x0^2*y0new +
        281*a0new*x0^2*y0new - 181*a1*x0^2*y0new + 181*a1new*x0^2*y0new +
        1812*s1*x1*y0new - 1812*s1new*x1*y0new + 562*a0*x0*x1*y0new -
        562*a0new*x0*x1*y0new + 362*a1*x0*x1*y0new - 362*a1new*x0*x1*y0new -
        281*a0*x1^2*y0new + 281*a0new*x1^2*y0new - 181*a1*x1^2*y0new +
        181*a1new*x1^2*y0new - 21720*y0*y0new + 10860*y0new^2 + 3732*s1*x0*y1 -
        3732*s1new*x0*y1 + 181*a0*x0^2*y1 - 181*a0new*x0^2*y1 + 281*a1*x0^2*y1 -
        281*a1new*x0^2*y1 - 3732*s1*x1*y1 + 3732*s1new*x1*y1 - 362*a0*x0*x1*y1 +
        362*a0new*x0*x1*y1 - 562*a1*x0*x1*y1 + 562*a1new*x0*x1*y1 + 181*a0*x1^2*y1 -
        181*a0new*x1^2*y1 + 281*a1*x1^2*y1 - 281*a1new*x1^2*y1 + 6000*y0*y1 -
        6000*y0new*y1 + 10860*y1^2 + s0new*(x0 - x1)*(69*a0*x0^2 - 69*a0new*x0^2 +
        52*a1*x0^2 - 52*a1new*x0^2 + 532*s1*(x0 - x1) - 532*s1new*(x0 - x1) -
        138*a0*x0*x1 + 138*a0new*x0*x1 - 104*a1*x0*x1 + 104*a1new*x0*x1 + 69*a0*x1^2 -
        69*a0new*x1^2 + 52*a1*x1^2 - 52*a1new*x1^2 + 3732*y0 - 3732*y0new + 1812*y1 -
        1812*y1new) - s0*(x0 - x1)*(-532*s1new*x0 + 69*a0*x0^2 - 69*a0new*x0^2 +
        52*a1*x0^2 - 52*a1new*x0^2 + 832*s0new*(x0 - x1) + 532*s1*(x0 - x1) +
        532*s1new*x1 - 138*a0*x0*x1 + 138*a0new*x0*x1 - 104*a1*x0*x1 + 104*a1new*x0*x1 +
        69*a0*x1^2 - 69*a0new*x1^2 + 52*a1*x1^2 - 52*a1new*x1^2 + 3732*y0 - 3732*y0new +
        1812*y1 - 1812*y1new) - 3732*s1*x0*y1new + 3732*s1new*x0*y1new -
        181*a0*x0^2*y1new + 181*a0new*x0^2*y1new - 281*a1*x0^2*y1new +
        281*a1new*x0^2*y1new + 3732*s1*x1*y1new - 3732*s1new*x1*y1new +
        362*a0*x0*x1*y1new - 362*a0new*x0*x1*y1new + 562*a1*x0*x1*y1new -
        562*a1new*x0*x1*y1new - 181*a0*x1^2*y1new + 181*a0new*x1^2*y1new -
        281*a1*x1^2*y1new + 281*a1new*x1^2*y1new - 6000*y0*y1new + 6000*y0new*y1new -
        21720*y1*y1new + 10860*y1new^2))/27720
    )
}

distance_spline_deriv_y0new <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                        y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        ((x0 - x1)*(1812*s1*x0 - 1812*s1new*x0 + 281*a0*x0^2 - 281*a0new*x0^2 + 181*a1*x0^2 -
        181*a1new*x0^2 - 3732*s0*(x0 - x1) + 3732*s0new*(x0 - x1) - 1812*s1*x1 +
        1812*s1new*x1 - 562*a0*x0*x1 + 562*a0new*x0*x1 - 362*a1*x0*x1 + 362*a1new*x0*x1 +
        281*a0*x1^2 - 281*a0new*x1^2 + 181*a1*x1^2 - 181*a1new*x1^2 + 21720*y0 -
        21720*y0new + 6000*y1 - 6000*y1new))/27720
    )
}

distance_spline_deriv_y1new <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                        y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        ((x0 - x1)*(3732*s1*x0 - 3732*s1new*x0 + 181*a0*x0^2 - 181*a0new*x0^2 + 281*a1*x0^2 -
        281*a1new*x0^2 - 1812*s0*(x0 - x1) + 1812*s0new*(x0 - x1) - 3732*s1*x1 +
        3732*s1new*x1 - 362*a0*x0*x1 + 362*a0new*x0*x1 - 562*a1*x0*x1 + 562*a1new*x0*x1 +
        181*a0*x1^2 - 181*a0new*x1^2 + 281*a1*x1^2 - 281*a1new*x1^2 + 6000*y0 - 6000*y0new +
        21720*y1 - 21720*y1new))/27720
    )
}

distance_spline_deriv_s0new <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                        y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        -((x0 - x1)^2*(532*s1*x0 - 532*s1new*x0 + 69*a0*x0^2 - 69*a0new*x0^2 + 52*a1*x0^2 -
        52*a1new*x0^2 - 832*s0*(x0 - x1) + 832*s0new*(x0 - x1) - 532*s1*x1 + 532*s1new*x1 -
        138*a0*x0*x1 + 138*a0new*x0*x1 - 104*a1*x0*x1 + 104*a1new*x0*x1 + 69*a0*x1^2 -
        69*a0new*x1^2 + 52*a1*x1^2 - 52*a1new*x1^2 + 3732*y0 - 3732*y0new + 1812*y1 -
        1812*y1new))/27720
    )
}

distance_spline_deriv_s1new <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                        y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        ((x0 - x1)^2*(832*s1*x0 - 832*s1new*x0 + 52*a0*x0^2 - 52*a0new*x0^2 + 69*a1*x0^2 -
        69*a1new*x0^2 - 532*s0*(x0 - x1) + 532*s0new*(x0 - x1) - 832*s1*x1 + 832*s1new*x1 -
        104*a0*x0*x1 + 104*a0new*x0*x1 - 138*a1*x0*x1 + 138*a1new*x0*x1 + 52*a0*x1^2 -
        52*a0new*x1^2 + 69*a1*x1^2 - 69*a1new*x1^2 + 1812*y0 - 1812*y0new + 3732*y1 -
        3732*y1new))/27720
    )
}

distance_spline_deriv_a0new <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                        y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        ((x0 - x1)^3*(52*s1*x0 - 52*s1new*x0 + 6*a0*x0^2 - 6*a0new*x0^2 + 5*a1*x0^2 -
        5*a1new*x0^2 - 69*s0*(x0 - x1) + 69*s0new*(x0 - x1) - 52*s1*x1 + 52*s1new*x1 -
        12*a0*x0*x1 + 12*a0new*x0*x1 - 10*a1*x0*x1 + 10*a1new*x0*x1 + 6*a0*x1^2 -
        6*a0new*x1^2 + 5*a1*x1^2 - 5*a1new*x1^2 + 281*y0 - 281*y0new + 181*y1 - 181*y1new))/27720
    )
}

distance_spline_deriv_a1new <- function(x0, x1, y0, y1, s0, s1, a0, a1,
                                        y0new, y1new, s0new, s1new, a0new, a1new) {
    return(
        ((x0 - x1)^3*(69*s1*x0 - 69*s1new*x0 + 5*a0*x0^2 - 5*a0new*x0^2 + 6*a1*x0^2 -
        6*a1new*x0^2 - 52*s0*(x0 - x1) + 52*s0new*(x0 - x1) - 69*s1*x1 + 69*s1new*x1 -
        10*a0*x0*x1 + 10*a0new*x0*x1 - 12*a1*x0*x1 + 12*a1new*x0*x1 + 5*a0*x1^2 -
        5*a0new*x1^2 + 6*a1*x1^2 - 6*a1new*x1^2 + 181*y0 - 181*y0new + 281*y1 - 281*y1new))/27720
    )
}

#' @title Tension of a quintic spline
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Calculates the tension of a quintic spline, as measured by
#' the square of the integral of the third derivative. Also calculates the
#' derivative of this tension with respect to the second derivative of the
#' spline at each bound.
#'
#' @param x0 Lower bound of the interval.
#' @param x1 Upper bound of the interval.
#' @param y0 Value of the spline at \code{x0} for the first spline.
#' @param y1 Value of the spline at \code{x1} for the first spline.
#' @param s0 Slope of the spline at \code{x0} for the first spline.
#' @param s1 Slope of the spline at \code{x1} for the first spline.
#' @param a0 Second derivative of the spline at \code{x0} for the first spline.
#' @param a1 Second derivative of the spline at \code{x1} for the first spline.
#'
#' @return The value of the distance or its derivatives.
#'
#' @rdname tension_spline
#' @name tension_spline
#' @aliases tension_spline tension_spline_deriv_a0 tension_spline_deriv_a1

tension_spline <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(
        (-3*(3*a0^2*x0^4 - 2*a0*a1*x0^4 + 3*a1^2*x0^4 + 64*s0^2*(x0 - x1)^2 +
        64*s1^2*(x0 - x1)^2 - 12*a0^2*x0^3*x1 + 8*a0*a1*x0^3*x1 - 12*a1^2*x0^3*x1 +
        18*a0^2*x0^2*x1^2 - 12*a0*a1*x0^2*x1^2 + 18*a1^2*x0^2*x1^2 - 12*a0^2*x0*x1^3 +
        8*a0*a1*x0*x1^3 - 12*a1^2*x0*x1^3 + 3*a0^2*x1^4 - 2*a0*a1*x1^4 + 3*a1^2*x1^4 +
        40*a0*x0^2*y0 - 40*a1*x0^2*y0 - 80*a0*x0*x1*y0 + 80*a1*x0*x1*y0 + 40*a0*x1^2*y0 -
        40*a1*x1^2*y0 + 240*y0^2 - 40*a0*x0^2*y1 + 40*a1*x0^2*y1 + 80*a0*x0*x1*y1 -
        80*a1*x0*x1*y1 - 40*a0*x1^2*y1 + 40*a1*x1^2*y1 - 480*y0*y1 + 240*y1^2 -
        8*s1*(x0 - x1)*(2*a0*(x0 - x1)^2 - 3*(a1*(x0 - x1)^2 + 10*(-y0 + y1))) +
        8*s0*(x0 - x1)*(14*s1*(x0 - x1) - 3*a0*(x0 - x1)^2 +
        2*(a1*(x0 - x1)^2 + 15*(-y0 + y1)))))/(x0 - x1)^5
    )
}

tension_spline_deriv_a0 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(
        (-3*(6*a0*x0^4 - 2*a1*x0^4 - 24*s0*(x0 - x1)^3 - 16*s1*(x0 - x1)^3 - 24*a0*x0^3*x1 +
        8*a1*x0^3*x1 + 36*a0*x0^2*x1^2 - 12*a1*x0^2*x1^2 - 24*a0*x0*x1^3 + 8*a1*x0*x1^3 +
        6*a0*x1^4 - 2*a1*x1^4 + 40*x0^2*y0 - 80*x0*x1*y0 + 40*x1^2*y0 - 40*x0^2*y1 +
        80*x0*x1*y1 - 40*x1^2*y1))/(x0 - x1)^5
    )
}

tension_spline_deriv_a1 <- function(x0, x1, y0, y1, s0, s1, a0, a1) {
    return(
        (-3*(-2*a0*x0^4 + 6*a1*x0^4 + 16*s0*(x0 - x1)^3 + 24*s1*(x0 - x1)^3 + 8*a0*x0^3*x1 -
        24*a1*x0^3*x1 - 12*a0*x0^2*x1^2 + 36*a1*x0^2*x1^2 + 8*a0*x0*x1^3 - 24*a1*x0*x1^3 -
        2*a0*x1^4 + 6*a1*x1^4 - 40*x0^2*y0 + 80*x0*x1*y0 - 40*x1^2*y0 + 40*x0^2*y1 -
        80*x0*x1*y1 + 40*x1^2*y1))/(x0 - x1)^5
    )
}
