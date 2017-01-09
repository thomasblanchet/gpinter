test_that("Adding up is consistent with Monte-Carlo", {
    set.seed(19920902)
    n <- 1e6

    # Parameters of the tabulation
    p <- seq(0, 0.9, 0.1)
    k <- length(p)

    # Parameters of the first Pareto distribution
    alpha1 <- runif(1, min=1, max=3)
    mu1 <- 5*rexp(1)

    # Parameters of the second Pareto distribution
    alpha2 <- runif(1, min=1, max=3)
    mu2 <- 5*rexp(1)

    # Parameter of the Gumbel copula
    theta <- runif(1, min=2, max=4)

    # Simulate
    u <- gumbel::rgumbel(n, theta)
    x1 <- mu1/(1 - u[, 1])^(1/alpha1)
    x2 <- mu2/(1 - u[, 2])^(1/alpha2)
    y <- x1 + x2

    # Generate tabulations
    q1 <- quantile(x1, p)
    topavg1 <- sapply(q1, function(q) mean(x1[x1 >= q]))
    average1 <- mean(x1)

    q2 <- quantile(x2, p)
    topavg2 <- sapply(q2, function(q) mean(x2[x2 >= q]))
    average2 <- mean(x2)

    dist1 <- tabulation_fit(p, q1, average1, topavg=topavg1)
    dist2 <- tabulation_fit(p, q2, average2, topavg=topavg2)
    dist_addup <- addup_dist(dist1, dist2, theta)

    # Generate test tabulation
    p_test <- seq(0, 0.90, 0.01)
    q_test <- quantile(y, p_test)
    average_test <- mean(y)
    topavg_test <- sapply(q_test, function(q) mean(y[y >= q]))
    topshare_test <- (1 - p_test)*topavg_test/average_test

    expect_equal(
        fitted_quantile(dist_addup, p_test),
        q_test,
        tolerance = 1e-3,
        check.attributes = FALSE
    )
    expect_equal(
        fitted_cdf(dist_addup, q_test),
        p_test,
        tolerance = 1e-3,
        check.attributes = FALSE
    )
    expect_equal(
        threshold_share(dist_addup, q_test),
        topshare_test,
        tolerance = 1e-3,
        check.attributes = FALSE
    )
    expect_equal(
        top_share(dist_addup, p_test),
        topshare_test,
        tolerance = 1e-3,
        check.attributes = FALSE
    )
    expect_equal(
        gini(dist_addup),
        reldist::gini(y),
        tolerance = 1e-3,
        check.attributes = FALSE
    )
})
