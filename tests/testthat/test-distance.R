test_that("Distance and its gradient with one additional point are valid", {
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

        y_ini <- h(x_new, x0, x1, y0, y1, s0, s1, a0, a1)
        s_ini <- hd1(x_new, x0, x1, y0, y1, s0, s1, a0, a1)
        a_ini <- hd2(x_new, x0, x1, y0, y1, s0, s1, a0, a1)

        # Distance between two identical splines is zero
        expect_equal(0,
            distance_one_point(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, y_ini, s_ini, a_ini,
                y_ini, s_ini, a_ini
            )
        )

        # Compare gradient with numerical differentiation
        expect_equal(
            distance_one_point_grad(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new, y_new, s_new, a_new,
                y_ini, s_ini, a_ini
            ),
            numDeriv::grad(function(theta) {
                distance_one_point(
                    x0, x1, y0, y1, s0, s1, a0, a1,
                    x_new, theta[1], theta[2], theta[3],
                    y_ini, s_ini, a_ini
                )
            }, x=c(y_new, s_new, a_new))
        )
    }
})

test_that("Distance and its gradient with two additional points are valid", {
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

        y_ini1 <- h(x_new1, x0, x1, y0, y1, s0, s1, a0, a1)
        s_ini1 <- hd1(x_new1, x0, x1, y0, y1, s0, s1, a0, a1)
        a_ini1 <- hd2(x_new1, x0, x1, y0, y1, s0, s1, a0, a1)

        y_ini2 <- h(x_new2, x0, x1, y0, y1, s0, s1, a0, a1)
        s_ini2 <- hd1(x_new2, x0, x1, y0, y1, s0, s1, a0, a1)
        a_ini2 <- hd2(x_new2, x0, x1, y0, y1, s0, s1, a0, a1)

        # Distance between two identical splines is zero
        expect_equal(0,
            distance_two_points(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, y_ini1, s_ini1, a_ini1, y_ini1, s_ini1, a_ini1,
                x_new2, y_ini2, s_ini2, a_ini2, y_ini2, s_ini2, a_ini2
            )
        )

        # Compare gradient with numerical differentiation
        expect_equal(
            distance_two_points_grad(
                x0, x1, y0, y1, s0, s1, a0, a1,
                x_new1, y_new1, s_new1, a_new1, y_ini1, s_ini1, a_ini1,
                x_new2, y_new2, s_new2, a_new2, y_ini2, s_ini2, a_ini2
            ),
            numDeriv::grad(function(theta) {
                distance_two_points(
                    x0, x1, y0, y1, s0, s1, a0, a1,
                    x_new1, theta[1], theta[2], theta[3], y_ini1, s_ini1, a_ini1,
                    x_new2, theta[4], theta[5], theta[6], y_ini2, s_ini2, a_ini2
                )
            }, x=c(y_new1, s_new1, a_new1, y_new2, s_new2, a_new2))
        )
    }
})


