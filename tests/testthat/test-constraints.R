test_that("Jacobian of monotonicity constraint with one additional point is valid", {
    set.seed(19920902)
    for (i in 1:10) {
        # Generate random parameter values
        x <- rnorm(2)
        x0 <- min(x)
        x1 <- max(x)
        y0 <- rnorm(1)
        y1 <- rnorm(1)
        s0 <- rnorm(1)
        s1 <- rnorm(1)
        a0 <- rnorm(1)
        a1 <- rnorm(1)

        y_new <- rnorm(1)
        s_new <- rnorm(1)
        a_new <- rnorm(1)

        x_new <- (x0 + x1)/2

        # Compare jacobian with numerical differentiation
        expect_equal(
            cns_one_point_jac(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, y_new, s_new, a_new
            ),
            numDeriv::jacobian(function(theta) {
                cns_one_point(
                    x0, x1, y0, y1, s0, s1, a0, a1,
                    x_new, theta[1], theta[2], theta[3]
                )
            }, x=c(y_new, s_new, a_new))
        )
    }
})

test_that("Jacobian of monotonicity constraint with two additional points is valid", {
    set.seed(19920902)
    for (i in 1:10) {
        # Generate random parameter values
        x <- rnorm(2)
        x0 <- min(x)
        x1 <- max(x)
        y0 <- rnorm(1)
        y1 <- rnorm(1)
        s0 <- rnorm(1)
        s1 <- rnorm(1)
        a0 <- rnorm(1)
        a1 <- rnorm(1)

        y_new1 <- rnorm(1)
        s_new1 <- rnorm(1)
        a_new1 <- rnorm(1)
        y_new2 <- rnorm(1)
        s_new2 <- rnorm(1)
        a_new2 <- rnorm(1)

        x_new1 <- (2*x0 + x1)/3
        x_new2 <- (x0 + 2*x1)/3

        # Compare jacobian with numerical differentiation
        expect_equal(
            cns_two_points_jac(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, y_new1, s_new1, a_new1,
                x_new2, y_new2, s_new2, a_new2
            ),
            numDeriv::jacobian(function(theta) {
                cns_two_points(
                    x0, x1, y0, y1, s0, s1, a0, a1,
                    x_new1, theta[1], theta[2], theta[3],
                    x_new2, theta[4], theta[5], theta[6]
                )
            }, x=c(y_new1, s_new1, a_new1, y_new2, s_new2, a_new2))
        )
    }
})

