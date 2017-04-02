library(readxl)
library(plyr)
library(gpinter)
library(ggplot2)
library(reshape2)
library(extrafont)
loadfonts()

file <- system.file("replication", "data-us", "US_income.csv", package="gpinter")
tab_full <- read.csv(file, sep=";")
tab_full <- tab_full[tab_full$p2 != "pall", ]
tab_full[, "p"] <- as.numeric(substring(tab_full$p2, 2))/100
# Compute Pareto coefficients
tab_full <- ddply(tab_full, "year", function(df) {
    delta <- diff(c(df$p, 1))
    avg_total <- sum(df$aptinc992j*delta)
    avg_labor <- sum(df$aptlin992j*delta)
    avg_capit <- sum(df$aptkin992j*delta)

    df[, "sptinc992j"] <- rev(cumsum(rev(df$sptinc992j)))
    df[, "sptlin992j"] <- rev(cumsum(rev(df$sptlin992j)))
    df[, "sptkin992j"] <- rev(cumsum(rev(df$sptkin992j)))

    df[, "bptinc992j"] <- avg_total*df$sptinc992j/(1 - df$p)/df$tptinc992j
    df[, "bptlin992j"] <- avg_labor*df$sptlin992j/(1 - df$p)/df$tptlin992j
    df[, "bptkin992j"] <- avg_capit*df$sptkin992j/(1 - df$p)/df$tptkin992j

    df[, "phi_ptinc992j"] <- -log(avg_total*df$sptinc992j)
    df[, "phi_ptlin992j"] <- -log(avg_labor*df$sptlin992j)
    df[, "phi_ptkin992j"] <- -log(avg_capit*df$sptkin992j)

    df[, "dphi_ptinc992j"] <- 1/df$bptinc992j
    df[, "dphi_ptlin992j"] <- 1/df$bptlin992j
    df[, "dphi_ptkin992j"] <- 1/df$bptkin992j

    return(df)
})

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

# Estimate phi'''
d3_phi <- ddply(tab_full, "year", function(df) {
    x <- -log(1 - df$p)

    x_out <- seq(-log(1 - min(df$p)), -log(1 - max(df$p)), length.out=1000)
    d3_phi <- sapply(x_out, function(x0) lpoly(x0, x, df$phi_ptinc992j, df$dphi_ptinc992j, 0.05))

    return(data.frame(x=x_out, d3_phi=d3_phi))
})

# Summarize the estimates
d3_phi <- ddply(d3_phi, "x", function(df) {
    p1 <- quantile(df$d3_phi, 0.1)
    p9 <- quantile(df$d3_phi, 0.9)
    mean <- mean(df$d3_phi)
    median <- median(df$d3_phi)
    return(data.frame(p1=p1, p9=p9, mean=mean, median=median))
})

pdf("~/Desktop/phid3_us.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(d3_phi) +
    geom_line(aes(x=x, y=p1), color="grey") +
    geom_line(aes(x=x, y=p9), color="grey") +
    geom_line(aes(x=x, y=median)) +
    geom_vline(xintercept=c(-log(1 - 0.5), -log(1 - 0.9), -log(1 - 0.99)), linetype="dashed") +
    annotate("text", label="paste(italic(p) == 50, '%')", x=-log(1 - 0.50) + 0.16, y=-5.2, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=-log(1 - 0.90) + 0.16, y=-5.2, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=-log(1 - 0.99) - 0.16, y=-5.2, angle=90, parse=TRUE) +
    ggtitle("United States, 1962-2014") +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    ylab(expression(paste(phi1, "'''", "(", italic(x), ")"))) +
    ylim(c(-6, 0.5)) + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/phid3_us.pdf"))

# ---------------------------------------------------------------------------- #
# French data
# ---------------------------------------------------------------------------- #

file <- system.file("replication", "data-fr", "GGP2017DINAAppendixC.xlsx", package="gpinter")
col_names <- c("p", unlist(lapply(1970:2014, function(y) paste0(c("thr", "topavg", "b"), y))))
tab_full <- data.frame(read_excel(file, sheet="TC11", skip=13, col_names=col_names))
tab_full <- tab_full[1:127, ]

tab_full <- tab_full[tab_full$p <= 99 & tab_full$p >= 10, ]

d3_phi <- data.frame()

for (year in c(1970, 1975, 1979, 1984, 1988, 1990:2012)) {
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
    return(data.frame(p1=p1, p9=p9, mean=mean, median=median))
})

pdf("~/Desktop/phid3_fr.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(d3_phi) +
    geom_line(aes(x=x, y=p1), color="grey") +
    geom_line(aes(x=x, y=p9), color="grey") +
    geom_line(aes(x=x, y=median)) +
    geom_vline(xintercept=c(-log(1 - 0.5), -log(1 - 0.9), -log(1 - 0.99)), linetype="dashed") +
    annotate("text", label="paste(italic(p) == 50, '%')", x=-log(1 - 0.50) + 0.16, y=-5.2, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=-log(1 - 0.90) + 0.16, y=-5.2, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=-log(1 - 0.99) - 0.16, y=-5.2, angle=90, parse=TRUE) +
    ggtitle("France, 1970-2012") +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) +
    ylab(expression(paste(phi1, "'''", "(", italic(x), ")"))) +
    ylim(c(-6, 0.5)) + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/phid3_fr.pdf"))
