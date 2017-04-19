test_that("Cubic splines basis functions are valid", {
    expect_equal(g00(0), 1)
    expect_equal(g00(1), 0)
    expect_equal(numDeriv::grad(g00, 0), 0)
    expect_equal(numDeriv::grad(g00, 1), 0)

    expect_equal(g01(0), 0)
    expect_equal(g01(1), 1)
    expect_equal(numDeriv::grad(g01, 0), 0)
    expect_equal(numDeriv::grad(g01, 1), 0)

    expect_equal(g10(0), 0)
    expect_equal(g10(1), 0)
    expect_equal(numDeriv::grad(g10, 0), 1)
    expect_equal(numDeriv::grad(g10, 1), 0)

    expect_equal(g11(0), 0)
    expect_equal(g11(1), 0)
    expect_equal(numDeriv::grad(g11, 0), 0)
    expect_equal(numDeriv::grad(g11, 1), 1)
})

test_that("First derivatives of the cubic splines basis functions are valid", {
    expect_equal(g00d1(0), 0)
    expect_equal(g00d1(1), 0)

    expect_equal(g01d1(0), 0)
    expect_equal(g01d1(1), 0)

    expect_equal(g10d1(0), 1)
    expect_equal(g10d1(1), 0)

    expect_equal(g11d1(0), 0)
    expect_equal(g11d1(1), 1)
})

test_that("Cubic spline function and its derivatives are valid", {
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

        # Test the spline function
        expect_equal(g(x0, x0, x1, y0, y1, s0, s1), y0)
        expect_equal(g(x1, x0, x1, y0, y1, s0, s1), y1)

        expect_equal(numDeriv::grad(function(x) {
            g(x, x0, x1, y0, y1, s0, s1)
        }, x0), s0)
        expect_equal(numDeriv::grad(function(x) {
            g(x, x0, x1, y0, y1, s0, s1)
        }, x1), s1)

        # Test its first derivative
        expect_equal(gd1(x0, x0, x1, y0, y1, s0, s1), s0)
        expect_equal(gd1(x1, x0, x1, y0, y1, s0, s1), s1)
    }
})

test_that("Cubic spline interpolation function and its derivatives are valid", {
    set.seed(19920902)
    for (i in 1:10) {
        # Generate random parameter values
        n <- rpois(1, 10)
        xk <- cumsum(exp(rnorm(n)))
        yk <- rnorm(n)
        sk <- rnorm(n)

        # Test the spline function
        expect_equal(cubic_spline(xk, xk, yk, sk), yk)

        expect_equal(numDeriv::grad(function(x) {
            cubic_spline(x, xk, yk, sk)
        }, xk, side=c(+1, rep(NA, n - 2), -1)), sk, tolerance=0.02)

        # Test its first derivative
        expect_equal(deriv_cubic_spline(xk, xk, yk, sk), sk)
    }
})

test_that("Natural/clampled cubic spline function for simple cases", {
    set.seed(19920902)
    for (i in 1:10) {
        # Constant function
        xk <- cumsum(exp(rnorm(10)))
        yk <- rep(rnorm(1), 10)
        sn <- (yk[10] - yk[9])/(xk[10] - xk[9])
        sk <- rep(0, 10)
        expect_equal(clamped_cubic_spline(xk, yk, sn), sk)

        # Linear function
        xk <- cumsum(exp(rnorm(10)))
        a <- rnorm(1)
        b <- rnorm(1)
        yk <- a*xk + b
        sn <- (yk[10] - yk[9])/(xk[10] - xk[9])
        sk <- rep(a, 10)
        expect_equal(clamped_cubic_spline(xk, yk, sn), sk)

        # Compare with explicit solution for three points only
        xk <- cumsum(exp(rnorm(3)))
        yk <- rnorm(3)
        sk <- rnorm(3)

        x1 <- xk[1]
        x2 <- xk[2]
        x3 <- xk[3]

        y1 <- yk[1]
        y2 <- yk[2]
        y3 <- yk[3]

        sn <- (yk[3] - yk[2])/(xk[3] - xk[2])

        s1 <- (3*x3^2*(y1 - y2) - 2*x1^2*(y2 - y3) + x2^2*(-3*y1 + y2 + 2*y3) -
            2*x1*(3*x3*(y1 - y2) + x2*(-3*y1 + y2 + 2*y3)))/((x1 - x2)*(4*x1 - x2 - 3*x3)*
            (x2 - x3))
        s2 <- (3*x3^2*(y1 - y2) + x2^2*(3*y1 + y2 - 4*y3) + 4*x1^2*(y2 - y3) +
            x2*(-6*x3*y1 - 8*x1*y2 + 6*x3*y2 + 8*x1*y3))/((x1 - x2)*(4*x1 - x2 - 3*x3)*
            (x2 - x3))
        s3 <- (y2 - y3)/(x2 - x3)

        expect_equal(clamped_cubic_spline(xk, yk, sn), c(s1, s2, s3))
    }
})

