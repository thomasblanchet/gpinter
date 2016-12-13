test_that("Individualization is consistent with Monte-Carlo", {
    set.seed(19920902)
    n <- 1e6

    for (i in 1:1) {
        set.seed(19920902)

        # Parameters of the tabulation
        p <- seq(0, 0.9, 0.1)
        k <- length(p)

        # Parameters of a Pareto distribution
        alpha <- runif(1, min=1, max=3)
        mu <- 5*rexp(1)

        # Simulate
        u <- runif(n)
        x <- mu/(1 - u)^(1/alpha)
        prob_couple <- ifelse(u >= 0.9, 0.9, u)
        couple <- (runif(n) <= prob_couple)
        x_indiv <- c(x[couple]/2, x[couple]/2, x[!couple])

        # Generate tabulation
        q <- quantile(x, p)
        topavg <- sapply(q, function(q) mean(x[x >= q]))
        average <- mean(x)
        lambda <- sapply(q, function(q) mean(couple[x >= q]))

        dist <- tabulation_fit(p, q, average, topavg=topavg)
        dist_indiv <- individualize_dist(dist, p, coupleshare=lambda[1], coupletop=lambda)

        # Generate test tabulation
        p_test <- seq(0, 0.90, 0.01)
        q_test <- quantile(x_indiv, p_test)
        topavg_test <- sapply(q_test, function(q) mean(x_indiv[x_indiv >= q]))
        average_test <- mean(x_indiv)

        expect_equal(
            fitted_cdf(dist_indiv, q_test),
            p_test,
            tolerance = 1e-2
        )
    }
})
