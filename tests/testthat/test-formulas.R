test_that("The derivatives of the quantile function monocity constraints are valid", {
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

        expect_equal(
            numDeriv::jacobian(function(x) {
                return(mono_cns(x0, x1, x[1], x[2], x[3], x[4], x[5], x[6]))
            }, x = c(y0, y1, s0, s1, a0, a1)),
            cbind(
                mono_cns_deriv_y0(x0, x1, y0, y1, s0, s1, a0, a1),
                mono_cns_deriv_y1(x0, x1, y0, y1, s0, s1, a0, a1),
                mono_cns_deriv_s0(x0, x1, y0, y1, s0, s1, a0, a1),
                mono_cns_deriv_s1(x0, x1, y0, y1, s0, s1, a0, a1),
                mono_cns_deriv_a0(x0, x1, y0, y1, s0, s1, a0, a1),
                mono_cns_deriv_a1(x0, x1, y0, y1, s0, s1, a0, a1)
            )
        )
    }
})

test_that("The distance between two splines is valid", {
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
        y0new <- rnorm(1)
        y1new <- rnorm(1)
        s0new <- rnorm(1)
        s1new <- rnorm(1)
        a0new <- rnorm(1)
        a1new <- rnorm(1)

        # Distance between identical splines is zero
        expect_equal(0,
            distance_spline(x0, x1,
                y0, y1, s0, s1, a0, a1,
                y0, y1, s0, s1, a0, a1
            )
        )
        expect_equal(0,
            distance_spline(x0, x1,
                y0new, y1new, s0new, s1new, a0new, a1new,
                y0new, y1new, s0new, s1new, a0new, a1new
            )
        )

        # Distance is symetrical
        expect_equal(
            distance_spline(x0, x1,
                y0, y1, s0, s1, a0, a1,
                y0new, y1new, s0new, s1new, a0new, a1new
            ),
            distance_spline(x0, x1,
                y0new, y1new, s0new, s1new, a0new, a1new,
                y0, y1, s0, s1, a0, a1
            )
        )

        # Compare with numerical integration
        expect_equal(
            integrate(function(x) {
                f1 <- h(x, x0, x1, y0new, y1new, s0new, s1new, a0new, a1new)
                f2 <- h(x, x0, x1, y0, y1, s0, s1, a0, a1)
                return((f2 - f1)^2)
            }, lower=x0, upper=x1)$value,
            distance_spline(x0, x1,
                y0, y1, s0, s1, a0, a1,
                y0new, y1new, s0new, s1new, a0new, a1new
            )
        )

        # Compare with numerical differentiation
        expect_equal(
            numDeriv::jacobian(function(x) {
                return(distance_spline(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    x[1], x[2], x[3], x[4], x[5], x[6]
                ))
            }, x = c(y0new, y1new, s0new, s1new, a0new, a1new)),
            cbind(
                distance_spline_deriv_y0new(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    y0new, y1new, s0new, s1new, a0new, a1new
                ),
                distance_spline_deriv_y1new(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    y0new, y1new, s0new, s1new, a0new, a1new
                ),
                distance_spline_deriv_s0new(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    y0new, y1new, s0new, s1new, a0new, a1new
                ),
                distance_spline_deriv_s1new(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    y0new, y1new, s0new, s1new, a0new, a1new
                ),
                distance_spline_deriv_a0new(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    y0new, y1new, s0new, s1new, a0new, a1new
                ),
                distance_spline_deriv_a1new(x0, x1,
                    y0, y1, s0, s1, a0, a1,
                    y0new, y1new, s0new, s1new, a0new, a1new
                )
            )
        )
    }
})

test_that("The tension of the spline is valid", {
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

        # Compare with numerical integration
        expect_equal(
            tension_spline(x0, x1, y0, y1, s0, s1, a0, a1),
            integrate(function(x) {
                return(hd3(x, x0, x1, y0, y1, s0, s1, a0, a1)^2)
            }, lower=x0, upper=x1)$value
        )

        # Compare with numerical differentiation
        expect_equal(
            numDeriv::jacobian(function(x) {
                return(tension_spline(x0, x1, y0, y1, s0, s1, x[1], x[2]))
            }, x=c(a0, a1)),
            cbind(
                tension_spline_deriv_a0(x0, x1, y0, y1, s0, s1, a0, a1),
                tension_spline_deriv_a1(x0, x1, y0, y1, s0, s1, a0, a1)
            )
        )
    }
})
