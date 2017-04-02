library(plyr)
library(readxl)

# ---------------------------------------------------------------------------- #
# French data (1970-2012)
# ---------------------------------------------------------------------------- #

# Local polynomial fitting
lpoly <- function(x0, x, y, dydx, alpha) {
    n <- length(x)
    t <- x - x0
    h <- sort(abs(t))[floor(alpha*n)]
    w <- dnorm(t, sd=h)/h

    Y <- c(y, dydx)

    X0 <- c(rep(1, n), rep(0, n))
    X1 <- c(t, rep(1, n))
    X2 <- c(t^2/2, t)
    X3 <- c(t^3/6, t^2/2)

    fit <- lm(Y ~ 0 + X0 + X1 + X2 + X3, weights=c(w, w))
    return(coef(fit)["X3"])
}

file <- system.file("replication", "data-fr", "GGP2017DINAAppendixC.xlsx", package="gpinter")
col_names <- c("p", unlist(lapply(1970:2014, function(y) paste0(c("thr", "topavg", "b"), y))))
tab_full <- data.frame(read_excel(file, sheet="TC11", skip=13, col_names=col_names))
tab_full <- tab_full[1:127, ]

tab_full <- tab_full[tab_full$p >= 10 & tab_full$p <= 99, ]

# Estimate phi''' on the test table
d3_phi <- data.frame()

for (year in 2003:2012) {
    average <- tab_full[1, paste0("topavg", year)]
    p <- tab_full$p/100

    s <- tab_full[, paste0("topavg", year)]*(1 - p)/average
    dphi <- tab_full[, paste0("thr", year)]/tab_full[, paste0("topavg", year)]

    x <- -log(1 - p)

    x_out <- seq(-log(1 - min(p)), -log(1 - max(p)), length.out=1000)

    phi <- -log(average*s)

    d3_phi <- rbind(d3_phi, data.frame(
        year = rep(year, length(x_out)),
        x = x_out,
        d3_phi = sapply(x_out, function(x0) lpoly(x0, x, phi, dphi, 0.05))
    ))
}

d3_phi <- d3_phi[1 - exp(-d3_phi$x) >= 0.1 & 1 - exp(-d3_phi$x) <= 0.99, ]

# Summarize the estimates
d3_phi <- ddply(d3_phi, "x", function(df) {
    p1 <- quantile(df$d3_phi, 0.1)
    p9 <- quantile(df$d3_phi, 0.9)
    mean <- mean(df$d3_phi)
    median <- median(df$d3_phi)
    max <- max(abs(df$d3_phi))
    return(data.frame(p1=p1, p9=p9, mean=mean, median=median, max=max))
})

plot(d3_phi$x, d3_phi$max, type='l')
d3_phi_max <- splinefun(d3_phi$x, d3_phi$max, method="monoH.FC")

# Estimate the error on phi and phi'''
xk <- -log(1 - c(0.1, 0.5, 0.9, 0.99))
u <- -log(1 - c(0.30, 0.75, 0.95))

fr_estim_err_max <- abs(interpolation_value_error(u, xk, d3_phi_max))
fr_estim_derr_max <- abs(interpolation_deriv_error(u, xk, d3_phi_max))

# Define the brackets for the short tabulation
tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 10, 50, 90, 99, 100),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

err_phi <- data.frame()
err_dphi <- data.frame()

for (year in c(1970, 1975, 1979, 1984, 1988, 1990:2002)) {
    average <- tab_full[1, paste0("topavg", year)]
    p <- tab_full$p/100

    s <- tab_full[, paste0("topavg", year)]*(1 - p)/average
    dphi_actual <- tab_full[, paste0("thr", year)]/tab_full[, paste0("topavg", year)]
    phi_actual <- -log(average*s)

    # Create a short tabulation with only four threhsolds
    tab <- ddply(tab_full, "bracket", function(df) {
        return(data.frame(
            p = min(df$p)/100,
            threshold = min(df[, paste0("thr", year)]),
            topavg = min(df[, paste0("topavg", year)])
        ))
    })
    tab$topshare <- (1 - tab$p)*tab$topavg/average
    tab$m <- tab$topshare*average

    # Generalized Pareto interpolation
    dist <- tabulation_fit(tab$p, tab$threshold, average, topshare=tab$topshare)

    # Get the true thresholds and shares
    err_phi <- rbind(err_phi, data.frame(
        p30 = phi(dist, -log(1 - 0.30)) - phi_actual[p == 0.30],
        p75 = phi(dist, -log(1 - 0.75)) - phi_actual[p == 0.75],
        p95 = phi(dist, -log(1 - 0.95)) - phi_actual[p == 0.95]
    ))
    err_dphi <- rbind(err_dphi, data.frame(
        p30 = deriv_phi(dist, -log(1 - 0.30)) - dphi_actual[p == 0.30],
        p75 = deriv_phi(dist, -log(1 - 0.75)) - dphi_actual[p == 0.75],
        p95 = deriv_phi(dist, -log(1 - 0.95)) - dphi_actual[p == 0.95]
    ))
}

fr_actual_err_max <- apply(abs(err_phi), 2, max)
fr_actual_derr_max <- apply(abs(err_dphi), 2, max)


print(format(fr_estim_err_max, digits = 2, scientific = FALSE))
print(format(fr_actual_err_max, digits = 2, scientific = FALSE))

print(format(fr_estim_derr_max, digits = 2, scientific = FALSE))
print(format(fr_actual_derr_max, digits = 2, scientific = FALSE))

# ---------------------------------------------------------------------------- #
# US data (1962--2014)
# ---------------------------------------------------------------------------- #

file <- system.file("replication", "data-us", "US_income.csv", package="gpinter")
tab_full <- read.csv(file, sep=";")
tab_full <- tab_full[tab_full$p2 != "pall", ]
tab_full[, "p"] <- as.numeric(substring(tab_full$p2, 2))/100

# Estimate phi''' on the test table
d3_phi <- ddply(tab_full, "year", function(df) {
    year <- df[1, "year"]
    if (year >= 2005) {
        delta <- diff(c(df$p, 1))
        average <- sum(df$aptinc992j*delta)
        p <- df$p
        s <- rev(cumsum(rev(df$sptinc992j)))
        phi <- -log(average*s)
        dphi <- df$tptinc992j*(1 - p)/(average*s)

        phi <- phi[p >= 0.1 & p <= 0.99]
        dphi <- dphi[p >= 0.1 & p <= 0.99]
        s <- s[p >= 0.1 & p <= 0.99]
        p <- p[p >= 0.1 & p <= 0.99]

        x <- -log(1 - p)
        x_out <- seq(-log(1 - min(p)), -log(1 - max(p)), length.out=1000)

        return(data.frame(
            year = rep(year, length(x_out)),
            x = x_out,
            d3_phi = sapply(x_out, function(x0) lpoly(x0, x, phi, dphi, 0.05))
        ))
    } else {
        return(NULL)
    }
})

d3_phi <- d3_phi[1 - exp(-d3_phi$x) >= 0.1 & 1 - exp(-d3_phi$x) <= 0.99, ]

# Summarize the estimates
d3_phi <- ddply(d3_phi, "x", function(df) {
    p1 <- quantile(df$d3_phi, 0.1)
    p9 <- quantile(df$d3_phi, 0.9)
    mean <- mean(df$d3_phi)
    median <- median(df$d3_phi)
    max <- max(abs(df$d3_phi))
    return(data.frame(p1=p1, p9=p9, mean=mean, median=median, max=max))
})

plot(d3_phi$x, d3_phi$max, type='l', ylim=c(0, 5))
d3_phi_max <- splinefun(d3_phi$x, d3_phi$max, method="monoH.FC")

# Estimate the error on phi and phi'''
xk <- -log(1 - c(0.1, 0.5, 0.9, 0.99))
u <- -log(1 - c(0.30, 0.75, 0.95))

us_estim_err_max <- abs(interpolation_value_error(u, xk, d3_phi_max))
us_estim_derr_max <- abs(interpolation_deriv_error(u, xk, d3_phi_max))

# Define the brackets for the short tabulation
tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 0.10, 0.50, 0.90, 0.99, 1),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

err_phi <- data.frame()
err_dphi <- data.frame()

d_ply(tab_full, "year", function(df) {
    year <- df$year[1]
    if (year < 2005) {
        delta <- diff(c(df$p, 1))
        average <- sum(df$aptinc992j*delta)
        p <- df$p
        s <- rev(cumsum(rev(df$sptinc992j)))
        phi_actual <- -log(average*s)
        dphi_actual <- df$tptinc992j*(1 - p)/(average*s)

        year <- df$year[1]

        tab <- ddply(df, "bracket", function(df) {
            return(data.frame(
                p = min(df$p),
                threshold = min(df$tptinc992j),
                sh = sum(df$sptinc992j)
            ))
        })

        # Truncated average
        topsh <- 1 - cumsum(tab$sh)
        # Remove first row corresponding to the bracket [p=0, p=0.1]
        tab <- tab[-1, ]
        tab$topsh <- topsh[1:4]
        tab$m <- topsh[1:4]*average
        tab$topavg <- tab$m/(1 - tab$p)

        # Generalized Pareto interpolation
        dist <- tabulation_fit(tab$p, tab$threshold, average, bracketshare=tab$sh)

        # Get the true thresholds and shares
        err_phi <<- rbind(err_phi, data.frame(
            p30 = phi(dist, -log(1 - 0.30)) - phi_actual[p == 0.30],
            p75 = phi(dist, -log(1 - 0.75)) - phi_actual[p == 0.75],
            p95 = phi(dist, -log(1 - 0.95)) - phi_actual[p == 0.95]
        ))
        err_dphi <<- rbind(err_dphi, data.frame(
            p30 = deriv_phi(dist, -log(1 - 0.30)) - dphi_actual[p == 0.30],
            p75 = deriv_phi(dist, -log(1 - 0.75)) - dphi_actual[p == 0.75],
            p95 = deriv_phi(dist, -log(1 - 0.95)) - dphi_actual[p == 0.95]
        ))
    } else {
        return(NULL)
    }
})

us_actual_err_max <- apply(abs(err_phi), 2, max)
us_actual_derr_max <- apply(abs(err_dphi), 2, max)

print(format(us_estim_err_max, digits = 2, scientific = FALSE))
print(format(us_actual_err_max, digits = 2, scientific = FALSE))

print(format(us_estim_derr_max, digits = 2, scientific = FALSE))
print(format(us_actual_derr_max, digits = 2, scientific = FALSE))
