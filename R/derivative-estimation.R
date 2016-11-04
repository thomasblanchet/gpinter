#' @title Estimate the value of a derivative on an irregular grid using the
#' central difference method
#'
#' @author Thomas Blanchet
#'
#' @description Estimate the value of the derivative of a function based on
#' three irregularly-spaced points. The method is based on a second-order
#' Taylor expansion of the function around \code{x1}.
#'
#' @param x0 The left point of the grid.
#' @param x1 The central point of the grid.
#' @param x2 The right point if the grid.
#' @param y0 The value of the function at \code{x0}.
#' @param y1 The value of the function at \code{x1}.
#' @param y2 The value of the function at \code{x2}.
#'
#' @return The value of the derivative at \code{x1}.

central_derivative <- function(x0, x1, x2, y0, y1, y2) {
    a <- -(x2 - x1)/((x1 - x0)*(x2 - x0))
    b <- ((x2 - x1) - (x1 - x0))/((x1 - x0)*(x2 - x1))
    c <- (x1 - x0)/((x2 - x1)*(x2 - x0))
    return(a*y0 + b*y1 + c*y2)
}

#' @title Estimate the value of the right-derivative on an irregular grid
#' using three points
#'
#' @author Thomas Blanchet
#'
#' @description Estimate the value of the right-derivative of a function
#' based on three irregularly-spaced points. The method is based on a
#' second-order Taylor expansion of the function around \code{x0}.
#'
#' @param x0 The left point of the grid.
#' @param x1 The central point of the grid.
#' @param x2 The right point if the grid.
#' @param y0 The value of the function at \code{x0}.
#' @param y1 The value of the function at \code{x1}.
#' @param y2 The value of the function at \code{x2}.
#'
#' @return The value of the right-derivative at \code{x0}.

right_derivative <- function(x0, x1, x2, y0, y1, y2) {
    a <- -((x1 - x0) + (x2 - x0))/((x0 - x1)*(x0 - x2))
    b <- (x2 - x0)/((x1 - x0)*(x2 - x1))
    c <- -(x1 - x0)/((x2 - x0)*(x2 - x1))
    return(a*y0 + b*y1 + c*y2)
}

#' @title Estimate the value of the left-derivative on an irregular grid
#' using three points
#'
#' @author Thomas Blanchet
#'
#' @description Estimate the value of the right-derivative of a function
#' based on three irregularly-spaced points. The method is based on a
#' second-order Taylor expansion of the function around \code{x2}.
#'
#' @param x0 The left point of the grid.
#' @param x1 The central point of the grid.
#' @param x2 The right point if the grid.
#' @param y0 The value of the function at \code{x0}.
#' @param y1 The value of the function at \code{x1}.
#' @param y2 The value of the function at \code{x2}.
#'
#' @return The value of the left-derivative at \code{x2}.

left_derivative <- function(x0, x1, x2, y0, y1, y2) {
    a <- (x2 - x1)/((x1 - x0)*(x2 - x0))
    b <- -(x2 - x0)/((x1 - x0)*(x2 - x1))
    c <- ((x2 - x0) + (x2 - x1))/((x2 - x0)*(x2 - x1))
    return(a*y0 + b*y1 + c*y2)
}

#' @title Estimate the value of the second derivative on an irregular grid
#' using three points
#'
#' @author Thomas Blanchet
#'
#' @description Estimate the value of the second derivative of a function based
#' on three irregularly-spaced points. The method is based on a second-order
#' Taylor expansion of the function. The value of the estimated derivative holds
#' for the three input points \code{x0}, \code{x1} and \code{x2}.
#'
#' @param x0 The left point of the grid.
#' @param x1 The central point of the grid.
#' @param x2 The right point if the grid.
#' @param y0 The value of the function at \code{x0}.
#' @param y1 The value of the function at \code{x1}.
#' @param y2 The value of the function at \code{x2}.
#'
#' @return The estimated value of the second derivative, which is the same
#' at \code{x0}, \code{x1} and \code{x2}.

second_derivative <- function(x0, x1, x2, y0, y1, y2) {
    a <- 2/((x1 - x0)*(x2 - x0))
    b <- -2/((x1 - x0)*(x2 - x1))
    c <- 2/((x2 - x0)*(x2 - x1))
    return(a*y0 + b*y1 + c*y2)
}
