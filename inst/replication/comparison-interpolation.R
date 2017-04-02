# ---------------------------------------------------------------------------- #
# Code for the replication of the tables comparing the different interpolation
# methods in Blanchet, Fournier & Piketty (2017).
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Method 1: Constant Pareto coefficient above a threshold
# ---------------------------------------------------------------------------- #

method1 <- function(p, pk, qk, mk, average) {
    k <- cut(p, breaks=c(pk, 1), include.lowest=TRUE, labels=FALSE)

    # Pareto coefficients
    b <- mk[k]/((1 - pk[k])*qk[k])
    a <- b/(b - 1)
    # Thresholds
    q <- qk[k]*((1 - p)/(1 - pk[k]))^(-1/a)
    # Averages above
    e <- b*q
    # Top shares
    s <- e*(1 - p)/average

    return(list(threshold=q, topshare=s))
}

# ---------------------------------------------------------------------------- #
# Method 2: Piecewise Pareto with threshold information only
# ---------------------------------------------------------------------------- #

method2 <- function(p, pk, qk, average) {
    # Number of thresholds
    n <- length(pk)

    # Pareto coefficient in each bracket
    ak <- -log((1 - pk[2:n])/(1 - pk[1:(n - 1)]))/log(qk[2:n]/qk[1:(n -1)])
    # Truncated average
    uk <- ak/(ak - 1) * qk[1:(n - 1)]^ak * (qk[1:(n - 1)]^(1 - ak) - qk[2:n]^(1 - ak)) * (1 - pk[1:(n-1)])
    # Beyond the last bracket
    uk <- c(uk, ak[n - 1]/(ak[n - 1] - 1)*(1 - pk[n])*qk[n])
    ak <- c(ak, ak[n - 1])
    # Lorenz curve
    mk <- rev(cumsum(rev(uk)))

    k <- cut(p, breaks=c(pk, 1), include.lowest=TRUE, labels=FALSE)

    q <- qk[k]*((1 - p)/(1 - pk[k]))^(-1/ak[k])
    u <- ak[k]/(ak[k] - 1) * qk[k]^ak[k] * (qk[k]^(1 - ak[k]) - q^(1 - ak[k])) * (1 - pk[k])

    return(list(threshold=q, topshare=(mk[k] - u)/average))
}

# ---------------------------------------------------------------------------- #
# Method 3: Mean-split histogram
# ---------------------------------------------------------------------------- #

method3 <- function(p, pk, qk, mk, average) {
    n <- length(pk)

    p0 <- pk[1:(n - 1)]
    p1 <- pk[2:n]
    q0 <- qk[1:(n - 1)]
    q1 <- qk[2:n]
    m0 <- mk[1:(n - 1)]
    m1 <- mk[2:n]

    uk <- -diff(mk)/diff(pk)

    f0 <- (p1 - p0)/(q1 - q0)*(q1 - uk)/(uk - q0)
    f1 <- (p1 - p0)/(q1 - q0)*(uk - q0)/(q1 - uk)

    qstar <- uk
    pstar <- p0 + (p1 - p0)*(q1 - uk)/(q1 - q0)
    mstar <- (p1 - pstar)*(qstar + 0.5*(p1 - pstar)/f1) + m1

    k <- cut(p, breaks=c(pk, 1), include.lowest=TRUE, labels=FALSE)

    return(list(
        threshold = ifelse(p <= pstar[k],
            q0[k] + (p - p0[k])/f0[k],
            qstar[k] + (p - pstar[k])/f1[k]
        ),
        topshare = ifelse(p <= pstar[k],
            (pstar[k] - p)*(q0[k] + 0.5*(p - p0[k] + pstar[k] - p0[k])/f0[k]) + mstar[k],
            (p1[k] - p)*(qstar[k] + 0.5*(p - pstar[k] + p1[k] - pstar[k])/f1[k]) + m1[k]
        )/average
    ))
}

# ---------------------------------------------------------------------------- #
# Method 4: Piecewise Pareto with both thresholds and mean information
# ---------------------------------------------------------------------------- #

f <- function(a, q0, q1) {
    if (a == 0) {
        return((q1 - q0)/(log(q1) - log(q0)))
    } else if (a == 1) {
        return(q0*q1*(log(q1) - log(q0))/(q1 - q0))
    } else {
        return(a/(a - 1)*(q1^(1 - a) - q0^(1 - a))/(q1^(-a) - q0^(-a)))
    }
}

method4 <- function(p, pk, qk, mk, average) {
    # Number of thresholds
    n <- length(pk)

    # Mean inside each threshold
    uk <- -diff(mk)/diff(pk)

    # Solve an equation for the Pareto coefficient
    ak <- sapply(1:(n - 1), function(i) {
        return(uniroot(function(x) f(x, qk[i], qk[i + 1]) - uk[i],
            lower = -10,
            upper = 10,
            extendInt = "downX",
            tol = sqrt(.Machine$double.eps)
        )$root)
    })
    kk <- -ak*(pk[2:n] - pk[1:(n - 1)])/(qk[2:n]^(-ak) - qk[1:(n - 1)]^(-ak))

    # Constant Pareto coefficient in the last bracket
    bn <- mk[n]/(qk[n]*(1 - pk[n]))
    an <- bn/(bn - 1)
    ak <- c(ak, an)
    kk <- c(kk, an*(1 - pk[n])/qk[n]^(-an))

    k <- cut(p, breaks=c(pk, 1), include.lowest=TRUE, labels=FALSE)

    q <- (qk[k]^(-ak[k]) - ak[k]/kk[k]*(p - pk[k]))^(-1/ak[k])
    m <- mk[k] - kk[k]/(ak[k] - 1)*(qk[k]^(1 - ak[k]) - q^(1 - ak[k]))

    return(list(threshold=q, topshare=m/average))
}

# ---------------------------------------------------------------------------- #
# Comparison of the methods on US data
# ---------------------------------------------------------------------------- #

library(readxl)
library(plyr)
library(gpinter)
library(ggplot2)
library(reshape2)

file <- system.file("replication", "data-us", "US_income.csv", package="gpinter")
tab_full <- read.csv(file, sep=";")
tab_full <- tab_full[tab_full$p2 != "pall", ]
tab_full[, "p"] <- as.numeric(substring(tab_full$p2, 2))/100
# Compute top share
tab_full <- ddply(tab_full, c("alpha2", "year"), function(df) {
    df[, "topshare"] <- rev(cumsum(rev(df$sptinc992j)))
    return(df)
})
# Compute Pareto coefficients
tab_full <- ddply(tab_full, c("alpha2", "year"), function(df) {
    delta <- diff(c(df$p, 1))
    avg_total <- sum(df$aptinc992j*delta)
    avg_labor <- sum(df$aptlin992j*delta)
    avg_capit <- sum(df$aptkin992j*delta)

    topshare_total <- rev(cumsum(rev(df$sptinc992j)))
    topshare_labor <- rev(cumsum(rev(df$sptlin992j)))
    topshare_capit <- rev(cumsum(rev(df$sptkin992j)))

    df[, "bptinc992j"] <- avg_total*topshare_total/(1 - df$p)/df$tptinc992j
    df[, "bptlin992j"] <- avg_labor*topshare_labor/(1 - df$p)/df$tptlin992j
    df[, "bptkin992j"] <- avg_capit*topshare_capit/(1 - df$p)/df$tptkin992j

    return(df)
})

# Define the brackets for the short tabulation
tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 0.10, 0.50, 0.90, 0.99, 1),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

interp_thresholds <- data.frame()
interp_topshares <- data.frame()

d_ply(tab_full, "year", function(df) {
    delta <- diff(c(df$p, 1))
    average <- sum(df$aptinc992j*delta)

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

    # Interpolate with the different methods
    p_out <- c(0.30, 0.75, 0.95)
    # Constant Pareto coefficient
    m1 <- method1(p_out, tab$p, tab$threshold, tab$m, average)
    # Piecewise Pareto with threshold information only
    m2 <- method2(p_out, tab$p, tab$threshold, average)
    # Mean-split histogram
    m3 <- method3(p_out, tab$p, tab$threshold, tab$m, average)
    # Piecewise Pareto with both thresholds and mean information
    m4 <- method4(p_out, tab$p, tab$threshold, tab$m, average)

    # Generalized Pareto interpolation
    dist <- tabulation_fit(tab$p, tab$threshold, average, bracketshare=tab$sh)
    m0 <- list(
        threshold = fitted_quantile(dist, p_out),
        topshare = top_share(dist, p_out)
    )

    # Get the true thresholds and shares
    actual <- list(
        threshold = c(
            df[df$p == 0.30, "tptinc992j"],
            df[df$p == 0.75, "tptinc992j"],
            df[df$p == 0.95, "tptinc992j"]
        ),
        topshare = c(
            df[df$p == 0.30, "topshare"],
            df[df$p == 0.75, "topshare"],
            df[df$p == 0.95, "topshare"]
        )
    )

    interp_thresholds <<- rbind(interp_thresholds,
        data.frame(year=year, method="m0",
            p30=m0$threshold[1]/average,
            p75=m0$threshold[2]/average,
            p95=m0$threshold[3]/average
        ),
        data.frame(year=year, method="m1",
            p30=m1$threshold[1]/average,
            p75=m1$threshold[2]/average,
            p95=m1$threshold[3]/average
        ),
        data.frame(year=year, method="m2",
            p30=m2$threshold[1]/average,
            p75=m2$threshold[2]/average,
            p95=m2$threshold[3]/average
        ),
        data.frame(year=year, method="m3",
            p30=m3$threshold[1]/average,
            p75=m3$threshold[2]/average,
            p95=m3$threshold[3]/average
        ),
        data.frame(year=year, method="m4",
            p30=m4$threshold[1]/average,
            p75=m4$threshold[2]/average,
            p95=m4$threshold[3]/average
        ),
        data.frame(year=year, method="actual",
            p30=actual$threshold[1]/average,
            p75=actual$threshold[2]/average,
            p95=actual$threshold[3]/average
        )
    )

    interp_topshares <<- rbind(interp_topshares,
        data.frame(year=year, method="m0",
            p30=m0$topshare[1],
            p75=m0$topshare[2],
            p95=m0$topshare[3]
        ),
        data.frame(year=year, method="m1",
            p30=m1$topshare[1],
            p75=m1$topshare[2],
            p95=m1$topshare[3]
        ),
        data.frame(year=year, method="m2",
            p30=m2$topshare[1],
            p75=m2$topshare[2],
            p95=m2$topshare[3]
        ),
        data.frame(year=year, method="m3",
            p30=m3$topshare[1],
            p75=m3$topshare[2],
            p95=m3$topshare[3]
        ),
        data.frame(year=year, method="m4",
            p30=m4$topshare[1],
            p75=m4$topshare[2],
            p95=m4$topshare[3]
        ),
        data.frame(year=year, method="actual",
            p30=actual$topshare[1],
            p75=actual$topshare[2],
            p95=actual$topshare[3]
        )
    )
})

# Tables
compare_thresholds <- melt(interp_thresholds, id.vars=c("method", "year"))
compare_thresholds <- dcast(compare_thresholds, year + variable ~ method)

compare_topshares <- melt(interp_topshares, id.vars=c("method", "year"))
compare_topshares <- dcast(compare_topshares, year + variable ~ method)

compare_thresholds$m0 <- abs((compare_thresholds$m0 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m1 <- abs((compare_thresholds$m1 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m2 <- abs((compare_thresholds$m2 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m3 <- abs((compare_thresholds$m3 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m4 <- abs((compare_thresholds$m4 - compare_thresholds$actual)/compare_thresholds$actual)

compare_thresholds_us <- ddply(compare_thresholds, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:4))]))
})

compare_topshares$m0 <- abs((compare_topshares$m0 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m1 <- abs((compare_topshares$m1 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m2 <- abs((compare_topshares$m2 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m3 <- abs((compare_topshares$m3 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m4 <- abs((compare_topshares$m4 - compare_topshares$actual)/compare_topshares$actual)

compare_topshares_us <- ddply(compare_topshares, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:4))]))
})

# ---------------------------------------------------------------------------- #
# Comparison of the methods on French data
# ---------------------------------------------------------------------------- #

file <- system.file("replication", "data-fr", "GGP2017DINAAppendixC.xlsx", package="gpinter")
col_names <- c("p", unlist(lapply(1970:2014, function(y) paste0(c("thr", "topavg", "b"), y))))
tab_full <- data.frame(read_excel(file, sheet="TC11", skip=13, col_names=col_names))
tab_full <- tab_full[1:127, ]

# Define the brackets for the short tabulation
tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 10, 50, 90, 99, 100),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

interp_thresholds <- data.frame()
interp_topshares <- data.frame()

for (year in c(1970, 1975, 1979, 1984, 1988, 1990:2012)) {
    # Overall average
    average <- tab_full[1, paste0("topavg", year)]

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
    # Remove first row corresponding to the bracket [p=0, p=0.2]
    tab <- tab[-1, ]

    # Interpolate with the different methods
    p_out <- c(0.30, 0.75, 0.95)
    # Constant Pareto coefficient
    m1 <- method1(p_out, tab$p, tab$threshold, tab$m, average)
    # Piecewise Pareto with threshold information only
    m2 <- method2(p_out, tab$p, tab$threshold, average)
    # Mean-split histogram
    m3 <- method3(p_out, tab$p, tab$threshold, tab$m, average)
    # Piecewise Pareto with both thresholds and mean information
    m4 <- method4(p_out, tab$p, tab$threshold, tab$m, average)

    # Generalized Pareto interpolation
    dist <- tabulation_fit(tab$p, tab$threshold, average, topshare=tab$topshare)
    m0 <- list(
        threshold = fitted_quantile(dist, p_out),
        topshare = top_share(dist, p_out)
    )


    # Get the true thresholds and shares
    actual <- list(
        threshold = c(
            tab_full[tab_full$p == 30, paste0("thr", year)],
            tab_full[tab_full$p == 75, paste0("thr", year)],
            tab_full[tab_full$p == 95, paste0("thr", year)]
        ),
        topshare = c(
            tab_full[tab_full$p == 30, paste0("topavg", year)]*(1 - 0.30)/average,
            tab_full[tab_full$p == 75, paste0("topavg", year)]*(1 - 0.75)/average,
            tab_full[tab_full$p == 95, paste0("topavg", year)]*(1 - 0.95)/average
        )
    )

    interp_thresholds <- rbind(interp_thresholds,
        data.frame(year=year, method="m0",
            p30=m0$threshold[1]/average,
            p75=m0$threshold[2]/average,
            p95=m0$threshold[3]/average
        ),
        data.frame(year=year, method="m1",
            p30=m1$threshold[1]/average,
            p75=m1$threshold[2]/average,
            p95=m1$threshold[3]/average
        ),
        data.frame(year=year, method="m2",
            p30=m2$threshold[1]/average,
            p75=m2$threshold[2]/average,
            p95=m2$threshold[3]/average
        ),
        data.frame(year=year, method="m3",
            p30=m3$threshold[1]/average,
            p75=m3$threshold[2]/average,
            p95=m3$threshold[3]/average
        ),
        data.frame(year=year, method="m4",
            p30=m4$threshold[1]/average,
            p75=m4$threshold[2]/average,
            p95=m4$threshold[3]/average
        ),
        data.frame(year=year, method="actual",
            p30=actual$threshold[1]/average,
            p75=actual$threshold[2]/average,
            p95=actual$threshold[3]/average
        )
    )

    interp_topshares <- rbind(interp_topshares,
        data.frame(year=year, method="m0",
            p30=m0$topshare[1],
            p75=m0$topshare[2],
            p95=m0$topshare[3]
        ),
        data.frame(year=year, method="m1",
            p30=m1$topshare[1],
            p75=m1$topshare[2],
            p95=m1$topshare[3]
        ),
        data.frame(year=year, method="m2",
            p30=m2$topshare[1],
            p75=m2$topshare[2],
            p95=m2$topshare[3]
        ),
        data.frame(year=year, method="m3",
            p30=m3$topshare[1],
            p75=m3$topshare[2],
            p95=m3$topshare[3]
        ),
        data.frame(year=year, method="m4",
            p30=m4$topshare[1],
            p75=m4$topshare[2],
            p95=m4$topshare[3]
        ),
        data.frame(year=year, method="actual",
            p30=actual$topshare[1],
            p75=actual$topshare[2],
            p95=actual$topshare[3]
        )
    )
}

# Tables
compare_thresholds <- melt(interp_thresholds, id.vars=c("method", "year"))
compare_thresholds <- dcast(compare_thresholds, year + variable ~ method)

compare_topshares <- melt(interp_topshares, id.vars=c("method", "year"))
compare_topshares <- dcast(compare_topshares, year + variable ~ method)

compare_thresholds$m0 <- abs((compare_thresholds$m0 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m1 <- abs((compare_thresholds$m1 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m2 <- abs((compare_thresholds$m2 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m3 <- abs((compare_thresholds$m3 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m4 <- abs((compare_thresholds$m4 - compare_thresholds$actual)/compare_thresholds$actual)

compare_thresholds_fr <- ddply(compare_thresholds, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:4))]))
})

compare_topshares$m0 <- abs((compare_topshares$m0 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m1 <- abs((compare_topshares$m1 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m2 <- abs((compare_topshares$m2 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m3 <- abs((compare_topshares$m3 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m4 <- abs((compare_topshares$m4 - compare_topshares$actual)/compare_topshares$actual)

compare_topshares_fr <- ddply(compare_topshares, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:4))]))
})

# Return the tables in LaTeX
print_table <- function() {
    cat("\\begin{tabular}{ccP@{}P@{}P@{}P@{}} \\toprule\n")
    cat("& & \\multicolumn{4}{>{\\centering\\arraybackslash}p{8cm}}{mean percentage gap between estimated and observed values} \\\\ \\cmidrule(l){3-6}\n")
    cat("& & ")
    cat(paste(sprintf("M%d", 0:3), collapse=" & "))
    cat("\\\\ \\midrule \n")

    cat("\\multirow{12}{3cm}{\\centering United States \\ (1962--2014)} & ")

    cat("\\multirow{2}{*}{Top 70\\% share}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_topshares_us[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[1, 2 + i]/compare_topshares_us[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{Top 25\\% share}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_topshares_us[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[2, 2 + i]/compare_topshares_us[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{Top 5\\% share}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_topshares_us[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[3, 2 + i]/compare_topshares_us[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{P30/average}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_thresholds_us[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[1, 2 + i]/compare_thresholds_us[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{P75/average}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_thresholds_us[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[2, 2 + i]/compare_thresholds_us[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{P95/average}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_thresholds_us[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[3, 2 + i]/compare_thresholds_us[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\midrule \n")

    cat("\\multirow{12}{3cm}{\\centering France \\ (1970--2012)} & ")

    cat("\\multirow{2}{*}{Top 70\\% share}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_topshares_fr[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[1, 2 + i]/compare_topshares_fr[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{Top 25\\% share}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_topshares_fr[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[2, 2 + i]/compare_topshares_fr[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{Top 5\\% share}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_topshares_fr[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[3, 2 + i]/compare_topshares_fr[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{P30/average}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_thresholds_fr[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[1, 2 + i]/compare_thresholds_fr[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{P75/average}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_thresholds_fr[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[2, 2 + i]/compare_thresholds_fr[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-6} \n")

    cat("& \\multirow{2}{*}{P95/average}")
    for (i in 0:3) {
        cat(" & ")
        cat(format(compare_thresholds_fr[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:3) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[3, 2 + i]/compare_thresholds_fr[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat("\\\\ \\bottomrule\n")
    cat("\\end{tabular}\n")
}
print_table()

# For the larger table in appendix
# Return the tables in LaTeX
print_table_appendix <- function() {
    cat("\\begin{tabular}{ccccccc} \\toprule\n")
    cat("& & ")
    cat(paste(sprintf("M%d", 0:4), collapse=" & "))
    cat("\\\\ \\midrule \n")

    cat("\\multirow{12}{3cm}{\\centering United States \\ (1962--2014)} & ")

    cat("\\multirow{2}{*}{Top 70\\% share}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_topshares_us[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[1, 2 + i]/compare_topshares_us[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{Top 25\\% share}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_topshares_us[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[2, 2 + i]/compare_topshares_us[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{Top 5\\% share}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_topshares_us[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[3, 2 + i]/compare_topshares_us[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{P30/average}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_thresholds_us[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[1, 2 + i]/compare_thresholds_us[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{P75/average}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_thresholds_us[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[2, 2 + i]/compare_thresholds_us[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{P95/average}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_thresholds_us[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[3, 2 + i]/compare_thresholds_us[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\midrule \n")

    cat("\\multirow{12}{3cm}{\\centering France \\ (1970--2012)} & ")

    cat("\\multirow{2}{*}{Top 70\\% share}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_topshares_fr[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[1, 2 + i]/compare_topshares_fr[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{Top 25\\% share}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_topshares_fr[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[2, 2 + i]/compare_topshares_fr[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{Top 5\\% share}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_topshares_fr[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[3, 2 + i]/compare_topshares_fr[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{P30/average}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_thresholds_fr[1, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[1, 2 + i]/compare_thresholds_fr[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{P75/average}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_thresholds_fr[2, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[2, 2 + i]/compare_thresholds_fr[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-7} \n")

    cat("& \\multirow{2}{*}{P95/average}")
    for (i in 0:4) {
        cat(" & ")
        cat(format(compare_thresholds_fr[3, 2 + i], scientific=FALSE, digits=2))
        cat("\\%")
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:4) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[3, 2 + i]/compare_thresholds_fr[3, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat("\\\\ \\bottomrule\n")
    cat("\\end{tabular}\n")
}
print_table_appendix()

