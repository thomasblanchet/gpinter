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

test_that("Natural quintic spline for some simple cases", {
    set.seed(19920902)
    for (i in 1:10) {
        # Constant function
        xk <- cumsum(exp(rnorm(10)))
        yk <- rep(1, 10)
        sk <- rep(0, 10)
        ak <- rep(0, 10)
        expect_equal(natural_quintic_spline(xk, yk, sk), ak)

        # Linear function
        xk <- cumsum(exp(rnorm(10)))
        a <- rnorm(1)
        b <- rnorm(1)
        yk <- a*xk + b
        sk <- rep(a, 10)
        ak <- rep(0, 10)
        expect_equal(natural_quintic_spline(xk, yk, sk), ak)

        # Quadratic function
        xk <- cumsum(exp(rnorm(10)))
        a <- rnorm(1)
        b <- rnorm(1)
        c <- rnorm(1)
        yk <- a*xk^2 + b*xk + c
        sk <- 2*a*xk + b
        ak <- rep(2*a, 10)
        expect_equal(natural_quintic_spline(xk, yk, sk), ak)

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

        s1 <- sk[1]
        s2 <- sk[2]
        s3 <- sk[3]

        a1 <- (24*s1*x1^2*x2^2 - 27*s1*x1*x2^3 + 3*s1*x2^4 + 3*s3*(x1 - x2)^3*(x2 - x3) +
            s2*(x1 - x2)*(7*x1 + 2*x2 - 9*x3)*(x1 - x3)*(x2 - x3) - 48*s1*x1^2*x2*x3 +
            33*s1*x1*x2^2*x3 + 15*s1*x2^3*x3 + 24*s1*x1^2*x3^2 + 15*s1*x1*x2*x3^2 -
            39*s1*x2^2*x3^2 - 21*s1*x1*x3^3 + 21*s1*x2*x3^3 - 40*x1*x2^2*y1 + 10*x2^3*y1 +
            80*x1*x2*x3*y1 + 10*x2^2*x3*y1 - 40*x1*x3^2*y1 - 50*x2*x3^2*y1 + 30*x3^3*y1 -
            10*x1^3*y2 + 30*x1^2*x2*y2 + 10*x1*x2^2*y2 - 80*x1*x2*x3*y2 - 10*x2^2*x3*y2 +
            40*x1*x3^2*y2 + 50*x2*x3^2*y2 - 30*x3^3*y2 + 10*x1^3*y3 - 30*x1^2*x2*y3 +
            30*x1*x2^2*y3 - 10*x2^3*y3)/(6*(x1 - x2)^2*(x1 - x3)*(x2 - x3)^2)
        a2 <- (-3*s1*x1*x2^3 + 3*s1*x2^4 + 3*s3*(x1 - x2)^3*(x2 - x3) + 9*s1*x1*x2^2*x3 -
            9*s1*x2^3*x3 - 9*s1*x1*x2*x3^2 + 9*s1*x2^2*x3^2 + 3*s1*x1*x3^3 - 3*s1*x2*x3^3 +
            7*s2*(x1 - x2)*(x1 - x3)*(x2 - x3)*(x1 - 2*x2 + x3) + 10*x2^3*y1 - 30*x2^2*x3*y1 +
            30*x2*x3^2*y1 - 10*x3^3*y1 - 10*x1^3*y2 + 30*x1^2*x2*y2 - 30*x1*x2^2*y2 +
            30*x2^2*x3*y2 - 30*x2*x3^2*y2 + 10*x3^3*y2 + 10*x1^3*y3 - 30*x1^2*x2*y3 +
            30*x1*x2^2*y3 - 10*x2^3*y3)/(2*(x1 - x2)^2*(x1 - x3)*(x2 - x3)^2)
        a3 <- -(3*s1*x1*x2^3 - 3*s1*x2^4 + 3*s3*(x1 - x2)^2*(7*x1 + x2 - 8*x3)*(x2 - x3) +
            s2*(x1 - x2)*(9*x1 - 2*x2 - 7*x3)*(x1 - x3)*(x2 - x3) - 9*s1*x1*x2^2*x3 +
            9*s1*x2^3*x3 + 9*s1*x1*x2*x3^2 - 9*s1*x2^2*x3^2 - 3*s1*x1*x3^3 + 3*s1*x2*x3^3 -
            10*x2^3*y1 + 30*x2^2*x3*y1 - 30*x2*x3^2*y1 + 10*x3^3*y1 - 30*x1^3*y2 +
            50*x1^2*x2*y2 - 10*x1*x2^2*y2 + 40*x1^2*x3*y2 - 80*x1*x2*x3*y2 + 10*x2^2*x3*y2 +
            30*x2*x3^2*y2 - 10*x3^3*y2 + 30*x1^3*y3 - 50*x1^2*x2*y3 + 10*x1*x2^2*y3 +
            10*x2^3*y3 - 40*x1^2*x3*y3 + 80*x1*x2*x3*y3 - 40*x2^2*x3*y3)/
            (6*(x1 - x2)^2*(x1 - x3)*(x2 - x3)^2)

        expect_equal(natural_quintic_spline(xk, yk, sk), c(a1, a2, a3))
    }
})

test_that("Natural quintic spline is equivalent to minimization of strain energy", {
    set.seed(19920902)
    for (i in 1:10) {
        xk <- cumsum(exp(rnorm(10)))
        yk <- rnorm(10)
        sk <- rnorm(10)

        # Minimize strain energy
        fit <- optim(
            par = rep(0, 10),
            fn = function(theta) {
                tension(xk, yk, sk, theta)
            },
            gr = function(theta) {
                tension_grad(xk, yk, sk, theta)
            },
            method = "BFGS",
            control = list(trace=0)
        )

        expect_equal(fit$par, natural_quintic_spline(xk, yk, sk),
            tolerance=1e-4, scale=max(fit$par))
    }
})
