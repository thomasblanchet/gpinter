test_that("Derivative estimation is exact for polynomials of degree 2", {
    set.seed(19920902)
    for (i in 1:10) {
        # Generate random parameter values
        a <- rnorm(1)
        b <- rnorm(1)
        c <- rnorm(1)

        f <- function(x) {
            return(a*x^2 + b*x + c)
        }
        fd <- function(x) {
            return(2*a*x + b)
        }
        fd2 <- function(x) {
            return(2*a)
        }

        xk <- cumsum(exp(rnorm(3)))
        yk <- f(xk)

        expect_equal(
            central_derivative(xk[1], xk[2], xk[3], yk[1], yk[2], yk[3]),
            fd(xk[2])
        )
        expect_equal(
            right_derivative(xk[1], xk[2], xk[3], yk[1], yk[2], yk[3]),
            fd(xk[1])
        )
        expect_equal(
            left_derivative(xk[1], xk[2], xk[3], yk[1], yk[2], yk[3]),
            fd(xk[3])
        )
        expect_equal(
            second_derivative(xk[1], xk[2], xk[3], yk[1], yk[2], yk[3]),
            fd2(xk[2])
        )
    }
})

