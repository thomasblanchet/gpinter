test_that("Nothing changes if we individualize with a constant share", {
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

        # Constant share of couples
        lambda <- rep(runif(1), length(p))

        dist <- tabulation_fit(p, q, average, topavg=topavg)

        dist_indiv <- individualize_dist(dist, p, coupleshare=lambda[1], coupletop=lambda)

    }
})
