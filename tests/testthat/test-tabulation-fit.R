test_that("tabulation_fit is exact for a Pareto distribution", {
    # Parameters of the Pareto distribution
    alpha <- 1.5
    mu <- 1

    # Generate the tabulation
    p <- seq(0, 0.9, 0.1)
    n <- length(p)
    q <- mu*(1 - p)^(-1/alpha)
    average <- alpha*mu/(alpha - 1)
    topavg <- alpha*q/(alpha - 1)

    dist <- tabulation_fit(p, q, average, topavg=topavg)

    # The estimated function must be an exact Pareto distribution
    expect_equal(dist$pk, p)
    expect_equal(dist$xk, -log(1 - p))
    expect_equal(dist$yk, (1 - 1/alpha)*dist$xk - log(alpha*mu/(alpha - 1)))
    expect_equal(dist$qk, q)
    expect_equal(dist$sk, rep(1 - 1/alpha, n))
    expect_equal(dist$ak, rep(0, n))
    expect_equal(dist$mk, topavg*(1 - p))
    expect_equal(dist$bk, rep(alpha/(alpha - 1), n))
    expect_equal(dist$average, average)
    expect_equal(dist$xi_top, 1/alpha)
    expect_equal(dist$mu_top, max(q))
    expect_equal(dist$mu_top, dist$sigma_top/dist$xi_top)

    # Test associated methods
    p_test <- seq(0, 0.99, 0.01)
    q_test <- mu*(1 - p_test)^(-1/alpha)
    n_test <- length(p_test)
    x_test <- -log(1 - p_test)
    expect_equal(
        phi(dist, x_test),
        (1 - 1/alpha)*x_test - log(alpha*mu/(alpha - 1))
    )
    expect_equal(
        deriv_phi(dist, x_test),
        rep((1 - 1/alpha), n_test)
    )
    expect_equal(
        fitted_quantile(dist, p_test),
        q_test
    )
    expect_equal(
        support(dist),
        list(lower=mu, upper=+Inf)
    )
    expect_equal(
        fitted_cdf(dist, q_test),
        1 - (mu/q_test)^alpha,
        tolerance = 1e-4
    )
    expect_equal(
        # Remove first value because it is on the support's boundary
        fitted_density(dist, q_test[-1]),
        alpha*mu^alpha/q_test[-1]^(alpha + 1),
        tolerance = 1e-4
    )
    expect_equal(
        top_share(dist, p_test),
        (1 - p_test)^(1 - 1/alpha)
    )
    expect_equal(
        invpareto(dist, p_test),
        rep(alpha/(alpha - 1), n_test)
    )
})

