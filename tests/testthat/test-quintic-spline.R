test_that("Quintic splines basis functions are valid", {
    expect_equal(h00(0), 1)
    expect_equal(h00(1), 0)
    expect_equal(numDeriv::grad(h00, 0), 0)
    expect_equal(numDeriv::grad(h00, 1), 0)
    expect_equal(numDeriv::hessian(h00, 0)[1, 1], 0)
    expect_equal(numDeriv::hessian(h00, 1)[1, 1], 0)

    expect_equal(h01(0), 0)
    expect_equal(h01(1), 1)
    expect_equal(numDeriv::grad(h01, 0), 0)
    expect_equal(numDeriv::grad(h01, 1), 0)
    expect_equal(numDeriv::hessian(h01, 0)[1, 1], 0)
    expect_equal(numDeriv::hessian(h01, 1)[1, 1], 0)

    expect_equal(h10(0), 0)
    expect_equal(h10(1), 0)
    expect_equal(numDeriv::grad(h10, 0), 1)
    expect_equal(numDeriv::grad(h10, 1), 0)
    expect_equal(numDeriv::hessian(h10, 0)[1, 1], 0)
    expect_equal(numDeriv::hessian(h10, 1)[1, 1], 0)

    expect_equal(h11(0), 0)
    expect_equal(h11(1), 0)
    expect_equal(numDeriv::grad(h11, 0), 0)
    expect_equal(numDeriv::grad(h11, 1), 1)
    expect_equal(numDeriv::hessian(h11, 0)[1, 1], 0)
    expect_equal(numDeriv::hessian(h11, 1)[1, 1], 0)

    expect_equal(h20(0), 0)
    expect_equal(h20(1), 0)
    expect_equal(numDeriv::grad(h20, 0), 0)
    expect_equal(numDeriv::grad(h20, 1), 0)
    expect_equal(numDeriv::hessian(h20, 0)[1, 1], 1)
    expect_equal(numDeriv::hessian(h20, 1)[1, 1], 0)

    expect_equal(h21(0), 0)
    expect_equal(h21(1), 0)
    expect_equal(numDeriv::grad(h21, 0), 0)
    expect_equal(numDeriv::grad(h21, 1), 0)
    expect_equal(numDeriv::hessian(h21, 0)[1, 1], 0)
    expect_equal(numDeriv::hessian(h21, 1)[1, 1], 1)
})

test_that("First derivatives of the quintic splines basis functions are valid", {
    expect_equal(h00d1(0), 0)
    expect_equal(h00d1(1), 0)
    expect_equal(numDeriv::grad(h00d1, 0), 0)
    expect_equal(numDeriv::grad(h00d1, 1), 0)

    expect_equal(h01d1(0), 0)
    expect_equal(h01d1(1), 0)
    expect_equal(numDeriv::grad(h01d1, 0), 0)
    expect_equal(numDeriv::grad(h01d1, 1), 0)

    expect_equal(h10d1(0), 1)
    expect_equal(h10d1(1), 0)
    expect_equal(numDeriv::grad(h10d1, 0), 0)
    expect_equal(numDeriv::grad(h10d1, 1), 0)

    expect_equal(h11d1(0), 0)
    expect_equal(h11d1(1), 1)
    expect_equal(numDeriv::grad(h11d1, 0), 0)
    expect_equal(numDeriv::grad(h11d1, 1), 0)

    expect_equal(h20d1(0), 0)
    expect_equal(h20d1(1), 0)
    expect_equal(numDeriv::grad(h20d1, 0), 1)
    expect_equal(numDeriv::grad(h20d1, 1), 0)

    expect_equal(h21d1(0), 0)
    expect_equal(h21d1(1), 0)
    expect_equal(numDeriv::grad(h21d1, 0), 0)
    expect_equal(numDeriv::grad(h21d1, 1), 1)
})

test_that("Second derivatives of the quintic splines basis functions are valid", {
    expect_equal(h00d2(0), 0)
    expect_equal(h00d2(1), 0)

    expect_equal(h01d2(0), 0)
    expect_equal(h01d2(1), 0)

    expect_equal(h10d2(0), 0)
    expect_equal(h10d2(1), 0)

    expect_equal(h11d2(0), 0)
    expect_equal(h11d2(1), 0)

    expect_equal(h20d2(0), 1)
    expect_equal(h20d2(1), 0)

    expect_equal(h21d2(0), 0)
    expect_equal(h21d2(1), 1)
})

test_that("Quintic spline function and its derivatives are valid", {
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

        # Test the spline function
        expect_equal(h(x0, x0, x1, y0, y1, s0, s1, a0, a1), y0)
        expect_equal(h(x1, x0, x1, y0, y1, s0, s1, a0, a1), y1)

        expect_equal(numDeriv::grad(function(x) {
            h(x, x0, x1, y0, y1, s0, s1, a0, a1)
        }, x0), s0)
        expect_equal(numDeriv::grad(function(x) {
            h(x, x0, x1, y0, y1, s0, s1, a0, a1)
        }, x1), s1)

        expect_equal(numDeriv::hessian(function(x) {
            h(x, x0, x1, y0, y1, s0, s1, a0, a1)
        }, x0)[1, 1], a0)
        expect_equal(numDeriv::hessian(function(x) {
            h(x, x0, x1, y0, y1, s0, s1, a0, a1)
        }, x1)[1, 1], a1)

        # Test its first derivative
        expect_equal(hd1(x0, x0, x1, y0, y1, s0, s1, a0, a1), s0)
        expect_equal(hd1(x1, x0, x1, y0, y1, s0, s1, a0, a1), s1)

        expect_equal(numDeriv::grad(function(x) {
            hd1(x, x0, x1, y0, y1, s0, s1, a0, a1)
        }, x0), a0)
        expect_equal(numDeriv::grad(function(x) {
            hd1(x, x0, x1, y0, y1, s0, s1, a0, a1)
        }, x1), a1)

        # Test its second derivative
        expect_equal(hd2(x0, x0, x1, y0, y1, s0, s1, a0, a1), a0)
        expect_equal(hd2(x1, x0, x1, y0, y1, s0, s1, a0, a1), a1)
    }
})

test_that("Quintic spline interpolation function and its derivatives are valid", {
    set.seed(19920902)
    for (i in 1:10) {
        # Generate random parameter values
        n <- rpois(1, 10)
        xk <- cumsum(exp(rnorm(n)))
        yk <- rnorm(n)
        sk <- rnorm(n)
        ak <- rnorm(n)

        # Test the spline function
        expect_equal(quintic_spline(xk, xk, yk, sk, ak), yk)

        expect_equal(numDeriv::grad(function(x) {
            quintic_spline(x, xk, yk, sk, ak)
        }, xk, side=c(+1, rep(NA, n - 2), -1)), sk, tolerance=0.02)

        # Test its first derivative
        expect_equal(deriv_quintic_spline(xk, xk, yk, sk, ak), sk)

        # Test its second derivative
        expect_equal(deriv2_quintic_spline(xk, xk, yk, sk, ak), ak)
    }
})
