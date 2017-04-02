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

d3_phi_fr <- d3_phi[1 - exp(-d3_phi$x) >= 0.1 & 1 - exp(-d3_phi$x) <= 0.99, ]

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

d3_phi_us <- d3_phi[1 - exp(-d3_phi$x) >= 0.1 & 1 - exp(-d3_phi$x) <= 0.99, ]

# Summarize the estimates
d3_phi <- ddply(rbind(d3_phi_fr, d3_phi_us), "x", function(df) {
    p1 <- quantile(df$d3_phi, 0.1)
    p9 <- quantile(df$d3_phi, 0.9)
    mean <- mean(df$d3_phi)
    median <- median(df$d3_phi)
    max <- max(abs(df$d3_phi))
    return(data.frame(p1=p1, p9=p9, mean=mean, median=median, max=max))
})

d3_phi$p <- 1 - exp(-d3_phi$x)

fit <- lm(median ~ p + I(p^2) + I(p^3) + I(p^4), data=d3_phi)

spline <- smooth.spline(d3_phi$x, d3_phi$median, spar=0.5)
plot(spline, type='l')
abline(h=0, col='red')

d3_phi_fun <- function(x) predict(spline, x)$y

max_error <- function(xk) {
    n <- length(xk)
    # Make the approximation that the maximum error is at the midpoints
    midpoints <- (xk[1:(n - 1)] + xk[2:n])/2

    if (xk[n - 1] >= xn) {
        return(+Inf)
    }

    return(max(abs(interpolation_value_error(midpoints, xk, d3_phi_fun))))
}

x1 <- -log(1 - 0.1)
xn <- -log(1 - 0.999)

results <- list()

for (n in c(4, 6, 8, 10)) {
    xk <- seq(x1, xn, length.out=n)
    dk <- diff(xk)[1:(length(xk) - 2)]

    fit <- optim(
        par = log(dk),
        fn = function(theta) {
            dk <- exp(theta)
            xk <- c(cumsum(c(x1, dk)), xn)
            return(max_error(xk))
        },
        method = "Nelder-Mead",
        control = list(trace=1, maxit=1000)
    )

    dk_fit <- exp(fit$par)
    xk_fit <- c(cumsum(c(x1, dk_fit)), xn)

    results[[n]] <- list(
        pk_fit = 1 - exp(-xk_fit),
        maxerr = fit$value
    )

    print(1 - exp(-xk_fit))
    print(1 - exp(-xk))
}

print(format(100*results[[4]]$pk_fit, digits = 4, scientific = FALSE))
print(format(100*results[[4]]$maxerr, digits = 3, scientific = FALSE))

print(format(100*results[[6]]$pk_fit, digits = 4, scientific = FALSE))
print(format(100*results[[6]]$maxerr, digits = 3, scientific = FALSE))

print(format(100*results[[8]]$pk_fit, digits = 4, scientific = FALSE))
print(format(100*results[[8]]$maxerr, digits = 3, scientific = FALSE))

print(format(100*results[[10]]$pk_fit, digits = 4, scientific = FALSE))
print(format(100*results[[10]]$maxerr, digits = 3, scientific = FALSE))




# Estimate phi''' on the test table
d3_phi <- ddply(tab_full, "year", function(df) {
    year <- df[1, "year"]
    delta <- diff(c(df$p, 1))
    average <- sum(df$aptinc992j*delta)
    p <- df$p
    s <- rev(cumsum(rev(df$sptinc992j)))
    phi <- -log(average*s)
    dphi <- df$tptinc992j*(1 - p)/(average*s)

    x <- -log(1 - p)
    x_out <- seq(-log(1 - min(p)), -log(1 - max(p)), length.out=500)

    return(data.frame(
        year = rep(year, length(x_out)),
        x = x_out,
        d3_phi = sapply(x_out, function(x0) lpoly(x0, x, phi, dphi, 0.05))
    ))
})

d3_phi_us <- d3_phi[1 - exp(-d3_phi$x) >= 0.1 & 1 - exp(-d3_phi$x) <= 0.99, ]







lm(log(-median) ~ x, data=d3_phi)

plot(d3_phi$x, -d3_phi$median, type='l')
curve(exp(1.2985 - 1.6998*x)^2, col='red', add=T)
d3_phi_fun <- splinefun(d3_phi$x, d3_phi$median, method="monoH.FC")
d3_phi_fun <- function(x) exp(1.2985 - 1.6998*x)^2

max_error <- function(xk) {
    n <- length(xk)
    # Make the approximation that the maximum error is at the midpoints
    midpoints <- (xk[1:(n - 1)] + xk[2:n])/2

    return(max(abs(interpolation_value_error(midpoints, xk, d3_phi_fun))))
}

x1 <- -log(1 - 0.1)
xn <- -log(1 - 0.9999)

n <- 5
xk <- seq(x1, xn, length.out=n)
dk <- diff(xk)[1:(length(xk) - 2)]

max_error(xk)

fit <- optim(
    par = log(dk),
    fn = function(theta) {
        dk <- exp(theta)
        xk <- c(cumsum(c(x1, dk)), xn)
        return(max_error(xk))
    },
    method = "Nelder-Mead",
    control = list(trace=1)
)

dk_fit <- exp(fit$par)
xk_fit <- c(cumsum(c(x1, dk_fit)), xn)
print(1 - exp(-xk_fit))
print(1 - exp(-xk))

