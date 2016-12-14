test_that("Individualization is consistent with Monte-Carlo", {
    set.seed(19920902)
    n <- 1e6

    # Parameters of the tabulation
    p <- seq(0, 0.9, 0.1)
    k <- length(p)

    # Parameters of a Pareto distribution
    alpha <- runif(1, min=1, max=3)
    mu <- 5*rexp(1)

    # Simulate
    u <- runif(n)
    x <- mu/(1 - u)^(1/alpha)
    prob_couple <- sqrt(floor(u*10)/10)
    couple <- (runif(n) <= prob_couple)
    x_indiv <- c(x[couple]/2, x[couple]/2, x[!couple])

    # Generate tabulation
    q <- quantile(x, p)
    topavg <- sapply(q, function(q) mean(x[x >= q]))
    average <- mean(x)
    m <- mean(couple)
    lambda1 <- sapply(q, function(q) mean(couple[x >= q]))
    lambda2 <- sapply(1:k, function(i) {
        if (i == k) {
            return(mean(couple[x >= q[i]]))
        } else {
            return(mean(couple[x >= q[i] & x < q[i + 1]]))
        }
    })

    dist <- tabulation_fit(p, q, average, topavg=topavg)
    dist_indiv1 <- individualize_dist(dist, p, coupleshare=m, coupletop=lambda1)
    dist_indiv2 <- individualize_dist(dist, p, coupleshare=m, couplebracket=lambda2)

    expect_equal(dist_indiv1, dist_indiv2, check.attributes=FALSE, tolerance=1e-6)
    expect_equal(
        dist_indiv1$ck,
        lambda2,
        check.attributes = FALSE,
        tolerance = 1e-4
    )
    expect_equal(
        dist_indiv2$ck,
        lambda2,
        check.attributes = FALSE,
        tolerance = 1e-4
    )

    # Generate test tabulation
    p_test <- seq(0, 0.99, 0.01)
    q_test <- quantile(x_indiv, p_test)
    topavg_test <- sapply(q_test, function(q) mean(x_indiv[x_indiv >= q]))
    average_test <- mean(x_indiv)

    # Test the couple is correctly interpolated
    expect_equal(
        couple_share(dist_indiv1, p_test),
        sapply(p_test, function(p) mean(couple[u >= p])),
        check.attributes = FALSE,
        tolerance = 1e-3
    )
    expect_equal(
        couple_share(dist_indiv2, p_test),
        sapply(p_test, function(p) mean(couple[u >= p])),
        check.attributes = FALSE,
        tolerance = 1e-3
    )

    # Test the distribution is correctly individualized
    expect_equal(
        fitted_cdf(dist_indiv1, q_test),
        p_test,
        check.attributes = FALSE,
        tolerance = 1e-2
    )
    expect_equal(
        fitted_cdf(dist_indiv2, q_test),
        p_test,
        check.attributes = FALSE,
        tolerance = 1e-2
    )
})
