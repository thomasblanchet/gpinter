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
