# ---------------------------------------------------------------------------- #
# Code for the replication of the figures showing the random error
# in Blanchet, Fournier & Piketty (2017).
# ---------------------------------------------------------------------------- #

library(gpinter)
library(pbapply)
library(FMStable)
library(readxl)
library(ggplot2)
library(plyr)

fancy_scientific <- function(x) {
    # turn in to character string in scientific notation
    l <- format(x, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    # turn the 'e+' into plotmath format
    l <- gsub("e", "%*%10^", l)
    # return this as an expression
    return(ifelse(x == 0, parse(text="0"), parse(text=l)))
}

# Import the distribution of labor income in the US, 1962
file <- system.file("replication", "data-us", "US_income.csv", package="gpinter")
tab_full <- read.csv(file, sep=";")
n <- tab_full[tab_full$year == 1962 & tab_full$p2 == "pall", "npopul992j"]
tab_full <- tab_full[tab_full$p2 != "pall", ]
tab_full <- tab_full[tab_full$year == 1962, ]
tab_full[, "p"] <- as.numeric(substring(tab_full$p2, 2))/100

# Overall average
delta <- diff(c(tab_full$p, 1))
average <- sum(tab_full$aptlin992j*delta)
tab_full$topshare <- rev(cumsum(rev(tab_full$sptlin992j)))

tab_full$topavg <- average*tab_full$topshare/(1 - tab_full$p)
tab_full$b <- tab_full$topavg/tab_full$tptlin992j

tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 0.1, 0.5, 0.9, 0.99, 1),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)
tab <- ddply(tab_full, "bracket", function(df) {
    return(data.frame(
        p = min(df$p),
        threshold = min(df$tptlin992j),
        sh = sum(df$sptlin992j)
    ))
})
# Truncated average
topsh <- 1 - cumsum(tab$sh)
# Remove first row corresponding to the bracket [p=0, p=0.1]
tab <- tab[-1, ]
tab$topsh <- topsh[1:4]
tab$m <- topsh[1:4]*average
tab$topavg <- tab$m/(1 - tab$p)

dist <- tabulation_fit(tab$p, tab$threshold, average, bracketshare=tab$sh)

plot_density(dist, c(min(tab$threshold), max(tab$threshold)))

# The distribution has finite variance: calculate the asymptotic error

pk <- tab$p
x_out <- seq(-log(1 - 0.1), -log(1 - 0.99), length.out=1000)

asymptotic_error_finite <- function(x, pk, samplesize, dist) {
    p <- 1 - exp(-x)
    k <- cut(p, pk, labels=FALSE, include.lowest=TRUE)
    if (is.na(k)) {
        return(c(NA, NA))
    }
    pk <- sort(c(pk, p))
    n <- length(pk)

    xk <- -log(1 - pk)
    mk <- (1 - pk)*top_average(dist, pk)
    qk <- as.integer(fitted_quantile(dist, pk))

    muk <- vector(mode="numeric", length=n)
    for (i in 1:(n - 1)) {
        muk[i] <- integrate(function(x) {
            return(fitted_quantile(dist, x))
        }, lower=pk[i], upper=pk[i+1])$value
    }
    muk[n] <- integrate(function(x) {
        return(fitted_quantile(dist, x))
    }, lower=pk[n], upper=1)$value

    sigmak <- vector(mode="numeric", length=n)
    for (i in 1:(n - 1)) {
        sigmak[i] <- integrate(function(x) {
            return(fitted_quantile(dist, x)^2)
        }, lower=pk[i], upper=pk[i+1])$value - muk[i]^2
    }
    sigmak[n] <- integrate(function(x) {
        return(fitted_quantile(dist, x)^2)
    }, lower=pk[n], upper=1)$value - muk[n]^2

    M01 <- matrix(0, nrow=n, ncol=n)
    for (i in 1:n) {
        for (j in 1:n) {
            if (i == j) {
                M01[i, j] <- sigmak[i]
            } else {
                M01[i, j] <- -muk[i]*muk[j]
            }
        }
    }

    M02 <- matrix(0, nrow=n, ncol=n)
    for (i in 1:n) {
        for (j in 1:n) {
            M02[i, j] <- pk[min(i, j)]*(1 - pk[max(i, j)])
        }
    }

    M03 <- matrix(nrow=n, ncol=n)
    for (i in 1:n) {
        for (j in 1:n) {
            if (i >= j) {
                M03[i, j] <- -pk[j]*muk[i]
            } else {
                M03[i, j] <- (1 - pk[j])*muk[i]
            }
        }
    }

    M04 <- diag(1/fitted_density(dist, qk))

    M05 <- matrix(0, nrow=n, ncol=n)
    for (i in 1:n) {
        for (j in 1:n) {
            if (i == j) {
                M05[i, j] <- qk[i]
            } else if (j == i + 1) {
                M05[i, j] <- -qk[j]
            }
        }
    }

    M06 <- rbind(
        cbind(diag(n), M05),
        cbind(matrix(0, nrow=n, ncol=n), -M04)
    )

    M07 <- rbind(
        cbind(M01, M03),
        cbind(t(M03), M02)
    )

    ham <- M06 %*% M07 %*% t(M06)

    M01 <- matrix(0, nrow=2*n, ncol=2*n)
    for (i in 1:(2*n)) {
        for (j in 1:(2*n)) {
            if (i <= n && j <= n) {
                if (i <= j) {
                    M01[i, j] <- 1
                }
            } else {
                if (i == j) {
                    M01[i, j] <- 1
                }
            }
        }
    }

    M02 <- matrix(0, nrow=2*n, ncol=2*n)
    for (i in 1:(2*n)) {
        for (j in 1:(2*n)) {
            if (i <= n && j <= n) {
                if (i == j) {
                    M02[i, j] <- -1/mk[i]
                }
            } else if (i > n && j <= n) {
                if (i - n == j) {
                    M02[i, j] <- -exp(-xk[j])*qk[j]/mk[j]^2
                }
            } else if (i > n && j > n) {
                if (i == j) {
                    M02[i, j] <- exp(-xk[j - n])/mk[j - n]
                }
            }
        }
    }

    M03 <- matrix(0, nrow=2*n - 3, ncol=2*n - 2)
    for (i in 1:(2*n - 3)) {
        for (j in 1:(2*n - 2)) {
            if (i <= n - 2) {
                if (i == j) {
                    M03[i, j] <- -1
                } else if (i + 1 == j) {
                    M03[i, j] <- 1
                }
            } else {
                if (i + 1 == j) {
                    M03[i, j] <- 1
                }
            }
        }
    }

    M03 <- cbind(
        M03[, 1:k],
        0,
        M03[, (k + 1):(n - 1 + k)],
        0,
        M03[, (n + k):(2*n - 2)]
    )

    M04 <- matrix(0, nrow=n-1, ncol=n-1)
    xk_ <- xk[-(k + 1)]
    for (i in 1:(n - 1)) {
        for (j in 1:(n - 1)) {
            if (i == 1 && j == 1) {
                M04[i, j] <- 9/(xk_[2] - xk_[1])
            } else if (i == n - 1 && j == n - 1) {
                M04[i, j] <- 1
            } else if (i == n - 1) {
                M04[i, j] <- 0
            } else if (i == j) {
                M04[i, j] <- 9/(xk_[i] - xk_[i - 1]) + 9/(xk_[i + 1] - xk_[i])
            } else if (i + 1 == j) {
                M04[i, j] <- -3/(xk_[i + 1] - xk_[i])
            } else if (i == j + 1) {
                M04[i, j] <- -3/(xk_[j + 1] - xk_[j])
            }
        }
    }

    M05 <- matrix(0, nrow=n-1, ncol=n-2)
    for (i in 1:(n - 1)) {
        for (j in 1:(n - 2)) {
            if (i == j) {
                M05[i, j] <- 60/(xk_[j + 1] - xk_[j])^3
            } else if (i == j + 1 && i <= n - 2) {
                M05[i, j] <- -60/(xk_[i] - xk_[i - 1])^3
            }
        }
    }

    M06 <- matrix(0, nrow=n-1, ncol=n-1)
    for (i in 1:(n-1)) {
        for (j in 1:(n-1)) {
            if (i == 1 && j == 1) {
                M06[i, j] <- -36/(xk_[2] - xk_[1])^2
            } else if (i == 1 && j == 2) {
                M06[i, j] <- -24/(xk_[2] - xk_[1])^2
            } else if (i == j && i <= n - 2) {
                M06[i, j] <- 36/(xk_[i] - xk_[i - 1])^2 - 36/(xk_[i + 1] - xk_[i])^2
            } else if (i == j + 1 && i <= n - 2) {
                M06[i, j] <- 24/(xk_[i] - xk_[i - 1])^2
            } else if (i + 1 == j && i <= n - 2) {
                M06[i, j] <- -24/(xk_[j] - xk_[j - 1])^2
            } else if (i == n - 1 && j == n - 2) {
                M06[i, j] <- -1/(xk_[n - 1] - xk_[n - 2])
            } else if (i == n - 1 && j == n - 1) {
                M06[i, j] <- 1/(xk_[n - 1] - xk_[n - 2])
            }
        }
    }

    M08 <- solve(M04) %*% cbind(M05, M06) %*% M03

    M09 <- matrix(0, nrow=8, ncol=2*n)
    M09[1, k + 0] <- 1
    M09[2, k + 1] <- 1
    M09[3, k + 2] <- 1
    M09[4, n + k + 0] <- 1
    M09[5, n + k + 1] <- 1
    M09[6, n + k + 2] <- 1
    M09[7, ] <- M08[k, ]
    M09[8, ] <- M08[k + 1, ]

    x0 <- xk_[k]
    x1 <- xk_[k+1]
    t <- (x - x0)/(x1 - x0)
    u <- (x1 - x0)
    M10 <- rbind(
        c(h00(t), -1, h01(t), u*h10(t), 0, u*h11(t), u^2*h20(t), u^2*h21(t)),
        c(h00d1(t)/u, 0, h01d1(t)/u, h10d1(t), -1, h11d1(t), u*h20d1(t), u*h21d1(t))
    )

    bread <- M10 %*% M09 %*% M02 %*% M01

    sigma <- sqrt(diag(bread %*% ham %*% t(bread))/samplesize)

    return(sqrt(2/pi)*sigma)
}

err_asymp2 <- pbsapply(x_out, function(x) asymptotic_error_finite(x, pk, n, dist))

error_asymp2 <- data.frame(
    x = x_out,
    phi_asymp = as.vector(err_asymp2[1, ]),
    dphi_asymp = as.vector(err_asymp2[2, ])
)

xk <- -log(1 - pk)

pdf("~/Desktop/err-phi-finite-variance.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(data=error_asymp2) + geom_line(aes(x=x, y=phi_asymp)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="paste(italic(p) == 10, '%')", x=xk[1] + 0.16, y=3.1e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 50, '%')", x=xk[2] + 0.16, y=3.1e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=xk[3] + 0.16, y=3.1e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=xk[4] - 0.16, y=3.1e-5, angle=90, parse=TRUE) +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    scale_y_continuous(name="mean absolute error", labels=fancy_scientific) +
    ggtitle(expression(paste(phi1, '(', italic(x), ") for US labor income (1962)")),
        subtitle=expression("for a tabulation with"~italic(p)~"= 10%, 50%, 90% and 99%")) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-phi-finite-variance.pdf"))

pdf("~/Desktop/err-dphi-finite-variance.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(data=error_asymp2) + geom_line(aes(x=x, y=dphi_asymp)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="paste(italic(p) == 10, '%')", x=xk[1] + 0.16, y=8e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 50, '%')", x=xk[2] + 0.16, y=8e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=xk[3] + 0.16, y=8e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=xk[4] - 0.16, y=8e-5, angle=90, parse=TRUE) +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    scale_y_continuous(name="mean absolute error", labels=fancy_scientific, limits=c(0, 9e-5)) +
    ggtitle(expression(paste(phi1, "'(", italic(x), ") for US labor income (1962)")),
        subtitle=expression("for a tabulation with"~italic(p)~"= 10%, 50%, 90% and 99%")) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-dphi-finite-variance.pdf"))

# Compare with misspecification error
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

tab_full <- tab_full[tab_full$p <= 0.99 & tab_full$p >= 0.1, ]

tab_full$x <- -log(1 - tab_full$p)
tab_full$dphi_actual <- tab_full$tptlin992j/tab_full$topavg
tab_full$phi_actual <- -log(average*tab_full$topshare)

d3_phi_x <- seq(-log(1 - 0.1), -log(1 - 0.99), length.out=1000)
d3_phi_y <- sapply(d3_phi_x, function(x0) {
    lpoly(x0, tab_full$x, tab_full$phi_actual, tab_full$dphi_actual, 0.05)
})

plot(d3_phi_x, d3_phi_y, type='l')
d3_phi_fun <- splinefun(d3_phi_x, d3_phi_y, method="monoH.FC")

tab_full$err_estim <- abs(interpolation_value_error(tab_full$x, xk, d3_phi_fun))
tab_full$derr_estim <- abs(interpolation_deriv_error(tab_full$x, xk, d3_phi_fun))

tab_full$err_actual <- abs(phi(dist, -log(1 - tab_full$p)) - tab_full$phi_actual)
tab_full$derr_actual <- abs(deriv_phi(dist, -log(1 - tab_full$p)) - tab_full$dphi_actual)

# Slightly smooth the error on dphi because of rounding in original data
tab_full$derr_actual <- smooth(tab_full$derr_actual)

df1 <- melt(tab_full[, c("x", "err_estim", "err_actual")], id.vars="x")
df1[df1$variable == "err_estim", "error"] <- "estimated"
df1[df1$variable == "err_actual", "error"] <- "observed"

pdf("~/Desktop/err-estim-us-1962.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(df1) +
    geom_line(aes(x=x, y=value, linetype=error)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    ylab("absolute error") +
    ggtitle(expression(paste("Error on ", phi1, "(", italic(x), ")")),
        subtitle="US labor income, 1962") +
    theme_bw() + theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.background = element_rect(linetype="solid", color="black", size=0.25),
        legend.box.margin = margin(10, 10, 10, 10),
        legend.direction = "horizontal", plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-estim-us-1962.pdf"))

df2 <- melt(tab_full[, c("x", "derr_estim", "derr_actual")], id.vars="x")
df2[df2$variable == "derr_estim", "error"] <- "estimated"
df2[df2$variable == "derr_actual", "error"] <- "observed"
pdf("~/Desktop/derr-estim-us-1962.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(df2) +
    geom_line(aes(x=x, y=value, linetype=error)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    ylab("absolute error") + ylim(c(0, 0.0108)) +
    ggtitle(expression(paste("Error on ", phi1, "'(", italic(x), ")")),
        subtitle="US labor income, 1962") +
    theme_bw() + theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.background = element_rect(linetype="solid", color="black", size=0.25),
        legend.box.margin = margin(10, 10, 10, 10),
        legend.direction = "horizontal", plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/derr-estim-us-1962.pdf"))

# Import the distribution of capital income in the US, 1962
file <- system.file("replication", "data-us", "US_income.csv", package="gpinter")
tab_full <- read.csv(file, sep=";")
n <- tab_full[tab_full$year == 1962 & tab_full$p2 == "pall", "npopul992j"]
tab_full <- tab_full[tab_full$p2 != "pall", ]
tab_full <- tab_full[tab_full$year == 1962, ]
tab_full[, "p"] <- as.numeric(substring(tab_full$p2, 2))/100

# Overall average
delta <- diff(c(tab_full$p, 1))
average <- sum(tab_full$aptkin992j*delta)
tab_full$topshare <- rev(cumsum(rev(tab_full$sptkin992j)))

tab_full$topavg <- average*tab_full$topshare/(1 - tab_full$p)
tab_full$b <- tab_full$topavg/tab_full$tptkin992j

tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 0.1, 0.5, 0.9, 0.99, 1),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)
tab <- ddply(tab_full, "bracket", function(df) {
    return(data.frame(
        p = min(df$p),
        threshold = min(df$tptkin992j),
        sh = sum(df$sptkin992j)
    ))
})
# Truncated average
topsh <- 1 - cumsum(tab$sh)
# Remove first row corresponding to the bracket [p=0, p=0.1]
tab <- tab[-1, ]
tab$topsh <- topsh[1:4]
tab$m <- topsh[1:4]*average
tab$topavg <- tab$m/(1 - tab$p)

dist <- tabulation_fit(tab$p, tab$threshold, average, bracketshare=tab$sh)

plot_density(dist, c(min(tab$threshold), max(tab$threshold)))

# The distribution has infinite variance: calculate the asymptotic error

# Spline basis functions
h00 <- function(x) {
    return(1 - 10*x^3 + 15*x^4 - 6*x^5)
}
h01 <- function(x) {
    return(10*x^3 - 15*x^4 + 6*x^5)
}
h10 <- function(x) {
    return(x - 6*x^3 + 8*x^4 - 3*x^5)
}
h11 <- function(x) {
    return(-4*x^3 + 7*x^4 - 3*x^5)
}
h20 <- function(x) {
    return(x^2/2 - (3*x^3)/2 + (3*x^4)/2 - x^5/2)
}
h21 <- function(x) {
    return(x^3/2 - x^4 + x^5/2)
}

# First derivatives
h00d1 <- function(x) {
    return(-30*x^2 + 60*x^3 - 30*x^4)
}
h01d1 <- function(x) {
    return(30*x^2 - 60*x^3 + 30*x^4)
}
h10d1 <- function(x) {
    return(1 - 18*x^2 + 32*x^3 - 15*x^4)
}
h11d1 <- function(x) {
    return(-12*x^2 + 28*x^3 - 15*x^4)
}
h20d1 <- function(x) {
    return(x - (9*x^2)/2 + 6*x^3 - (5*x^4)/2)
}
h21d1 <- function(x) {
    return((3*x^2)/2 - 4*x^3 + (5*x^4)/2)
}

# The stable distribution

dstable <- function(x, alpha) {
    param <- setParam(alpha=alpha, location=0, logscale=0, pm="M")
    return(dEstable(x, param))
}
pstable <- function(x, alpha) {
    param <- setParam(alpha=alpha, location=0, logscale=0, pm="M")
    return(pEstable(x, param))
}
qstable <- function(x, alpha) {
    param <- setParam(alpha=alpha, location=0, logscale=0, pm="M")
    return(qEstable(x, param))
}

mean_abs_error_stable <- function(alpha) {
    param <- setParam(alpha=alpha, location=0, logscale=0, pm="M")

    I1 <- integrate(function(x) {
        return(-x*dEstable(x, param))
    }, lower=-Inf, upper=0)$value

    I2 <- integrate(function(x) {
        return(x*dEstable(x, param))
    }, lower=0, upper=+Inf)$value

    return(I1 + I2)
}

asymptotic_error_infinite <- function(x, pk, samplesize, dist) {
    p <- 1 - exp(-x)
    k <- cut(p, pk, labels=FALSE, include.lowest=TRUE)
    if (is.na(k)) {
        return(c(NA, NA))
    }
    pk <- sort(c(pk, p))
    n <- length(pk)

    xk <- -log(1 - pk)
    qk <- fitted_quantile(dist, pk)
    mk <- threshold_share(dist, qk)*dist$average

    cons <- (1 - pk[n])*(dist$xi_top/dist$sigma_top)^(-1/dist$xi_top)
    alpha <- 1/dist$xi_top

    g <- (pi*cons/(2*gamma(alpha)*sin(alpha*pi/2)))^(1/alpha)

    S <- matrix(0, nrow=2*n, ncol=1)
    S[n, 1] <- mean_abs_error_stable(alpha)

    M01 <- matrix(0, nrow=2*n, ncol=2*n)
    for (i in 1:(2*n)) {
        for (j in 1:(2*n)) {
            if (i <= n && j <= n) {
                if (i <= j) {
                    M01[i, j] <- 1
                }
            } else {
                if (i == j) {
                    M01[i, j] <- 1
                }
            }
        }
    }

    M02 <- matrix(0, nrow=2*n, ncol=2*n)
    for (i in 1:(2*n)) {
        for (j in 1:(2*n)) {
            if (i <= n && j <= n) {
                if (i == j) {
                    M02[i, j] <- -1/mk[i]
                }
            } else if (i > n && j <= n) {
                if (i - n == j) {
                    M02[i, j] <- -exp(-xk[j])*qk[j]/mk[j]^2
                }
            } else if (i > n && j > n) {
                if (i == j) {
                    M02[i, j] <- exp(-xk[j - n])/mk[j - n]
                }
            }
        }
    }

    M03 <- matrix(0, nrow=2*n - 3, ncol=2*n - 2)
    for (i in 1:(2*n - 3)) {
        for (j in 1:(2*n - 2)) {
            if (i <= n - 2) {
                if (i == j) {
                    M03[i, j] <- -1
                } else if (i + 1 == j) {
                    M03[i, j] <- 1
                }
            } else {
                if (i + 1 == j) {
                    M03[i, j] <- 1
                }
            }
        }
    }

    M03 <- cbind(
        M03[, 1:k],
        0,
        M03[, (k + 1):(n - 1 + k)],
        0,
        M03[, (n + k):(2*n - 2)]
    )

    M04 <- matrix(0, nrow=n-1, ncol=n-1)
    xk_ <- xk[-(k + 1)]
    for (i in 1:(n - 1)) {
        for (j in 1:(n - 1)) {
            if (i == 1 && j == 1) {
                M04[i, j] <- 9/(xk_[2] - xk_[1])
            } else if (i == n - 1 && j == n - 1) {
                M04[i, j] <- 1
            } else if (i == n - 1) {
                M04[i, j] <- 0
            } else if (i == j) {
                M04[i, j] <- 9/(xk_[i] - xk_[i - 1]) + 9/(xk_[i + 1] - xk_[i])
            } else if (i + 1 == j) {
                M04[i, j] <- -3/(xk_[i + 1] - xk_[i])
            } else if (i == j + 1) {
                M04[i, j] <- -3/(xk_[j + 1] - xk_[j])
            }
        }
    }

    M05 <- matrix(0, nrow=n-1, ncol=n-2)
    for (i in 1:(n - 1)) {
        for (j in 1:(n - 2)) {
            if (i == j) {
                M05[i, j] <- 60/(xk_[j + 1] - xk_[j])^3
            } else if (i == j + 1 && i <= n - 2) {
                M05[i, j] <- -60/(xk_[i] - xk_[i - 1])^3
            }
        }
    }

    M06 <- matrix(0, nrow=n-1, ncol=n-1)
    for (i in 1:(n-1)) {
        for (j in 1:(n-1)) {
            if (i == 1 && j == 1) {
                M06[i, j] <- -36/(xk_[2] - xk_[1])^2
            } else if (i == 1 && j == 2) {
                M06[i, j] <- -24/(xk_[2] - xk_[1])^2
            } else if (i == j && i <= n - 2) {
                M06[i, j] <- 36/(xk_[i] - xk_[i - 1])^2 - 36/(xk_[i + 1] - xk_[i])^2
            } else if (i == j + 1 && i <= n - 2) {
                M06[i, j] <- 24/(xk_[i] - xk_[i - 1])^2
            } else if (i + 1 == j && i <= n - 2) {
                M06[i, j] <- -24/(xk_[j] - xk_[j - 1])^2
            } else if (i == n - 1 && j == n - 2) {
                M06[i, j] <- -1/(xk_[n - 1] - xk_[n - 2])
            } else if (i == n - 1 && j == n - 1) {
                M06[i, j] <- 1/(xk_[n - 1] - xk_[n - 2])
            }
        }
    }

    M08 <- solve(M04) %*% cbind(M05, M06) %*% M03

    M09 <- matrix(0, nrow=8, ncol=2*n)
    M09[1, k + 0] <- 1
    M09[2, k + 1] <- 1
    M09[3, k + 2] <- 1
    M09[4, n + k + 0] <- 1
    M09[5, n + k + 1] <- 1
    M09[6, n + k + 2] <- 1
    M09[7, ] <- M08[k, ]
    M09[8, ] <- M08[k + 1, ]

    x0 <- xk_[k]
    x1 <- xk_[k+1]
    t <- (x - x0)/(x1 - x0)
    u <- (x1 - x0)
    M10 <- rbind(
        c(h00(t), -1, h01(t), u*h10(t), 0, u*h11(t), u^2*h20(t), u^2*h21(t)),
        c(h00d1(t)/u, 0, h01d1(t)/u, h10d1(t), -1, h11d1(t), u*h20d1(t), u*h21d1(t))
    )

    return(abs(samplesize^(1/alpha - 1) * g * M10 %*% M09 %*% M02 %*% M01 %*% S))
}

err_asymp2 <- pbsapply(x_out, function(x) asymptotic_error_infinite(x, pk, n, dist))

error_asymp2 <- data.frame(
    x = x_out,
    phi_asymp = as.vector(err_asymp2[1, ]),
    dphi_asymp = as.vector(err_asymp2[2, ])
)


pdf("~/Desktop/err-phi-infinite-variance.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(data=error_asymp2) + geom_line(aes(x=x, y=phi_asymp)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="paste(italic(p) == 10, '%')", x=xk[1] + 0.16, y=1.8e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 50, '%')", x=xk[2] + 0.16, y=1.8e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=xk[3] + 0.16, y=1.8e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=xk[4] - 0.16, y=1.8e-5, angle=90, parse=TRUE) +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    scale_y_continuous(name="mean absolute error", labels=fancy_scientific) +
    ggtitle(expression(paste(phi1, '(', italic(x), ") for US capital income (1962)")),
        subtitle=expression("for a tabulation with"~italic(p)~"= 10%, 50%, 90% and 99%")) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-phi-infinite-variance.pdf"))

pdf("~/Desktop/err-dphi-infinite-variance.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(data=error_asymp2) + geom_line(aes(x=x, y=dphi_asymp)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="paste(italic(p) == 10, '%')", x=xk[1] + 0.16, y=4.5e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 50, '%')", x=xk[2] + 0.16, y=4.5e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=xk[3] + 0.16, y=4.5e-5, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=xk[4] - 0.16, y=4.5e-5, angle=90, parse=TRUE) +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    scale_y_continuous(name="mean absolute error", labels=fancy_scientific, limits=c(0, 5e-5)) +
    ggtitle(expression(paste(phi1, "'(", italic(x), ") for US capital income (1962)")),
        subtitle=expression("for a tabulation with"~italic(p)~"= 10%, 50%, 90% and 99%")) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-dphi-infinite-variance.pdf"))

xk <- -log(1 - pk)
ggplot(data=error_asymp2) + geom_line(aes(x=x, y=phi_asymp)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=2.3e-5, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=2.3e-5, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=2.3e-5, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=2.3e-5, angle=90) +
    xlab("x = -log(1 - p)") + ylab("mean absolute error") + #ylim(c(0, 2.6e-5)) +
    ggtitle(expression(paste("Asymptotic error on ", phi1(x), " for US capital income (1962)")), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
ggsave("~/Desktop/err-phi-infinite-variance.pdf", height=5, width=5)

ggplot(data=error_asymp2) + geom_line(aes(x=x, y=dphi_asymp)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=4.5e-5, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=4.5e-5, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=4.5e-5, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=4.5e-5, angle=90) +
    xlab("x = -log(1 - p)") + ylab("mean absolute error") + #ylim(c(0, 5e-5)) +
    ggtitle(expression(paste("Asymptotic error on ", phi1, "'"(x), " for US capital income (1962)")), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
ggsave("~/Desktop/err-dphi-infinite-variance.pdf", height=5, width=5)

