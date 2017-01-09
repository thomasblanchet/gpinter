test_that("Individualization is consistent with Monte-Carlo", {
    set.seed(19920902)
    n <- 1e6

    # Parameters of the tabulation
    p <- sort(runif(10))

    # Parameters of a Pareto distribution
    alpha <- runif(1, min=1, max=3)
    mu <- 5*rexp(1)

    # Simulate
    u <- runif(n)
    x <- mu/(1 - u)^(1/alpha)
    prob_couple <- 0.1 + 0.8*sqrt(u)
    couple <- (runif(n) <= prob_couple)
    x_indiv <- c(x[couple]/2, x[couple]/2, x[!couple])

    # Generate tabulation
    q <- quantile(x, p)
    p <- c(0, p)
    q <- c(mu, q)
    k <- length(p)
    topavg <- sapply(q, function(q) mean(x[x >= q]))
    average <- mean(x)
    ratio <- apply(cbind(q, c(q[-1], Inf)), 1, function(thr) {
        return(mean(x[!couple & x > thr[1] & x <= thr[2]])/mean(x[couple & x > thr[1] & x <= thr[2]]))
    })
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
    dist_indiv1 <- individualize_dist(dist, p, coupleshare=m, coupletop=lambda1, ratio=ratio)
    dist_indiv2 <- individualize_dist(dist, p, coupleshare=m, couplebracket=lambda2, ratio=ratio)

    expect_equal(dist_indiv1, dist_indiv2, check.attributes=FALSE, tolerance=1e-4)

    # Generate test tabulation
    p_test <- seq(0, 0.99, 0.01)
    q_test <- quantile(x_indiv, p_test)
    topavg_test <- sapply(q_test, function(q) mean(x_indiv[x_indiv >= q]))
    average_test <- mean(x_indiv)
    topshare_test <- (1 - p_test)*topavg_test/average_test
    density_test <- density(x_indiv, from=mu/3, to=2*mu, n=100)

    # Test that the tabulations for couples and singles are correct
    expect_equal(
        dist_indiv1$singles$p, sapply(q, function(thr) mean(x[!couple] <= thr)),
        tolerance = 1e-6
    )
    expect_equal(
        dist_indiv1$singles$bracketavg, apply(cbind(q, c(q[-1], Inf)), 1, function(thr) {
            return(mean(x[!couple & x > thr[1] & x <= thr[2]]))
        }),
        tolerance = 1e-5,
        check.attributes = FALSE
    )
    expect_equal(
        dist_indiv1$couples$p, sapply(q, function(thr) mean(x[couple] <= thr)),
        tolerance = 1e-6
    )
    expect_equal(
        dist_indiv1$couples$bracketavg, apply(cbind(q, c(q[-1], Inf)), 1, function(thr) {
            return(mean(x[couple & x > thr[1] & x <= thr[2]]))
        }),
        tolerance = 1e-5,
        check.attributes = FALSE
    )

    # Test the distribution is correctly individualized
    expect_equal(
        fitted_cdf(dist_indiv1, q_test),
        p_test,
        check.attributes = FALSE,
        tolerance = 1e-3
    )
    expect_equal(
        fitted_density(dist_indiv1, density_test$x),
        density_test$y,
        check.attributes = FALSE,
        tolerance = 0.1
    )
    expect_equal(
        fitted_quantile(dist_indiv1, p_test),
        q_test,
        check.attributes = FALSE,
        tolerance = 1e-3
    )
    expect_equal(
        threshold_share(dist_indiv1, q_test),
        topshare_test,
        check.attributes = FALSE,
        tolerance = 1e-3
    )
    expect_equal(
        top_share(dist_indiv1, p_test),
        topshare_test,
        check.attributes = FALSE,
        tolerance = 1e-3
    )
})
