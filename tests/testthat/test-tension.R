test_that("Tension of several splines and its gradient are valid", {
    set.seed(19920902)
    for (i in 1:10) {
        # Generate random parameter values
        n <- rpois(1, 10)
        xk <- cumsum(exp(rnorm(n)))
        yk <- rnorm(n)
        sk <- rnorm(n)
        ak <- rnorm(n)

        # Compare tension with numerical integration
        t <- tension(xk, yk, sk, ak)
        expect_equal(t,
            integrate(function(x) {
                return(deriv3_quintic_spline(x, xk, yk, sk, ak)^2)
            }, lower=min(xk), upper=max(xk))$value,
            tolerance=0.001, scale=t
        )

        # Compare gradient with numerical differentiation
        expect_equal(
            numDeriv::grad(function(x) {
                return(tension(xk, yk, sk, x))
            }, x=ak),
            tension_grad(xk, yk, sk, ak),
            tolerance=0.001
        )
    }
})
