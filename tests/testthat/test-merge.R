test_that("Nothing changes if we merge the same distributions", {
    set.seed(19920902)

    for (i in 1:3) {
        # Parameters of a Pareto distribution
        alpha <- runif(1, min=1, max=3)
        mu <- 5*rexp(1)

        # Generate the tabulation
        p <- seq(0, 0.9, 0.1)
        n <- length(p)
        q <- mu*(1 - p)^(-1/alpha)
        average <- alpha*mu/(alpha - 1)
        topavg <- alpha*q/(alpha - 1)

        dist1 <- tabulation_fit(p, q, average, topavg=topavg)
        dist2 <- tabulation_fit(p, q, average, topavg=topavg)
        dist3 <- tabulation_fit(p, q, average, topavg=topavg)

        # Respective population sizes
        populations <- runif(3)

        dist_total <- merge_dist(list(dist1, dist2, dist3), populations)

        expect_equal(support(dist_total)$lower, mu)
        expect_equal(support(dist_total)$upper, +Inf)

        p_test <- seq(0, 0.9, 0.1)
        q_test <- mu*(1 - p_test)^(-1/alpha)
        n_test <- length(p_test)
        x_test <- -log(1 - p_test)

        expect_equal(
            phi(dist_total, x_test),
            (1 - 1/alpha)*x_test - log(alpha*mu/(alpha - 1)),
            tolerance = 1e-4
        )
        expect_equal(
            deriv_phi(dist_total, x_test),
            rep((1 - 1/alpha), n_test),
            tolerance = 1e-4
        )
        expect_equal(
            fitted_cdf(dist_total, q_test),
            1 - (mu/q_test)^alpha,
            tolerance = 1e-4
        )
        expect_equal(
            # Remove first value because it is on the support's boundary
            fitted_density(dist_total, q_test[-1]),
            alpha*mu^alpha/q_test[-1]^(alpha + 1),
            tolerance = 1e-7
        )
        expect_equal(
            fitted_quantile(dist_total, p_test),
            q_test,
            tolerance = 1e-4
        )
        expect_equal(
            threshold_share(dist_total, q_test),
            (1 - p_test)^(1 - 1/alpha),
            tolerance = 1e-4
        )
        expect_equal(
            top_share(dist_total, p_test),
            (1 - p_test)^(1 - 1/alpha),
            tolerance = 1e-4
        )
        expect_equal(
            invpareto(dist_total, p_test),
            rep(alpha/(alpha - 1), n_test),
            tolerance = 1e-3
        )
        expect_equal(gini(dist_total), 1/(2*alpha - 1), tolerance=1e-4)
    }
})
