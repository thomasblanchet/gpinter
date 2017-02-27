# ---------------------------------------------------------------------------- #
# Code for the replication of the tables comparing the different extrapolation
# methods in Blanchet, Fournier & Piketty (2017).
# ---------------------------------------------------------------------------- #

library(readxl)
library(plyr)
library(gpinter)
library(ggplot2)
library(reshape2)
library(scales)

# ---------------------------------------------------------------------------- #
# Comparison of the methods on US data
# ---------------------------------------------------------------------------- #

extra_thresholds <- data.frame()
extra_topshares <- data.frame()

for (year in c(1962, 1964, 1966:2010)) {
    # Import full tabulation from the excel file
    file <- system.file("replication", "data-us", paste0("gpercptinc", year, "equal.xlsx"), package="gpinter")
    tab_full <- data.frame(read_excel(file))
    tab_full$topshare <- rev(cumsum(rev(tab_full$sh)))

    # Overall average
    average <- sum(diff(c(tab_full$gperc/100, 1))*tab_full$avg)

    # Create a short tabulation with only five thresholds
    tab_full$bracket <- cut(tab_full$gperc,
        breaks = c(0, 10, 50, 90, 99, 100),
        include.lowest = TRUE,
        labels = FALSE,
        right = FALSE
    )
    tab <- ddply(tab_full, "bracket", function(df) {
        return(data.frame(
            p = min(df$gperc)/100,
            threshold = min(df$thres),
            sh = sum(df$sh)
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
    p_out <- c(0.999, 0.9999)
    # Constant Pareto coefficient
    m1 <- method1(p_out, tab$p, tab$threshold, tab$m, average)
    # Piecewise Pareto with threshold information only
    m2 <- method2(p_out, tab$p, tab$threshold, average)

    # Generalized Pareto interpolation
    dist <- tabulation_fit(tab$p, tab$threshold, average, bracketshare=tab$sh)
    m0 <- list(
        threshold = fitted_quantile(dist, p_out),
        topshare = top_share(dist, p_out)
    )

    # Get the true thresholds and shares
    actual <- list(
        threshold = c(
            tab_full[as.integer(1000*tab_full$gperc) == 100000*0.999, "thres"],
            tab_full[as.integer(1000*tab_full$gperc) == 100000*0.9999, "thres"]
        ),
        topshare = c(
            tab_full[as.integer(1000*tab_full$gperc) == 100000*0.999, "topshare"],
            tab_full[as.integer(1000*tab_full$gperc) == 100000*0.9999, "topshare"]
        )
    )

    extra_thresholds <- rbind(extra_thresholds,
        data.frame(year=year, method="m0",
            p999=m0$threshold[1]/average,
            p9999=m0$threshold[2]/average
        ),
        data.frame(year=year, method="m1",
            p999=m1$threshold[1]/average,
            p9999=m1$threshold[2]/average
        ),
        data.frame(year=year, method="m2",
            p999=m2$threshold[1]/average,
            p9999=m2$threshold[2]/average
        ),
        data.frame(year=year, method="actual",
            p999=actual$threshold[1]/average,
            p9999=actual$threshold[2]/average
        )
    )

    extra_topshares <- rbind(extra_topshares,
        data.frame(year=year, method="m0",
            p999=m0$topshare[1],
            p9999=m0$topshare[2]
        ),
        data.frame(year=year, method="m1",
            p999=m1$topshare[1],
            p9999=m1$topshare[2]
        ),
        data.frame(year=year, method="m2",
            p999=m2$topshare[1],
            p9999=m2$topshare[2]
        ),
        data.frame(year=year, method="actual",
            p999=actual$topshare[1],
            p9999=actual$topshare[2]
        )
    )
}

ggplot(extra_topshares) + geom_line(aes(x=year, y=p999, color=method))
ggplot(extra_thresholds) + geom_line(aes(x=year, y=p999, color=method))

# Tables
compare_thresholds <- melt(extra_thresholds, id.vars=c("method", "year"))
compare_thresholds <- dcast(compare_thresholds, year + variable ~ method)

compare_topshares <- melt(extra_topshares, id.vars=c("method", "year"))
compare_topshares <- dcast(compare_topshares, year + variable ~ method)

compare_thresholds$m0 <- abs(compare_thresholds$m0 - compare_thresholds$actual)
compare_thresholds$m1 <- abs(compare_thresholds$m1 - compare_thresholds$actual)
compare_thresholds$m2 <- abs(compare_thresholds$m2 - compare_thresholds$actual)

compare_thresholds <- ddply(compare_thresholds, "variable", function(df) {
    return(colMeans(df[, c(paste0("m", 0:2))]))
})

compare_topshares$m0 <- abs(compare_topshares$m0 - compare_topshares$actual)
compare_topshares$m1 <- abs(compare_topshares$m1 - compare_topshares$actual)
compare_topshares$m2 <- abs(compare_topshares$m2 - compare_topshares$actual)

compare_topshares <- ddply(compare_topshares, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:2))]))
})

# Export in LaTeX
print_table_us <- function() {
    cat("\\begin{tabular}{lccccc} \\toprule\n")
    cat("& ")
    cat(paste(sprintf("M%d", 0:2), collapse=" & "))
    cat("\\\\ \\midrule \\\\ \n")

    cat("\\multirow{2}{*}{P99.9/average}")
    for (i in 0:2) {
        cat(" & ")
        cat(format(compare_thresholds[1, 2 + i], scientific=FALSE, digits=3))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds[1, 2 + i]/compare_thresholds[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \n")

    cat("\\multirow{2}{*}{P99.99/average}")
    for (i in 0:2) {
        cat(" & ")
        cat(format(compare_thresholds[2, 2 + i], scientific=FALSE, digits=3))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds[2, 2 + i]/compare_thresholds[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \\midrule \\\\ \n")

    cat("\\multirow{2}{*}{Top 0.1\\% share}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_topshares[1, 2 + i], scientific=FALSE, digits=3), "\\%"))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares[1, 2 + i]/compare_topshares[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \n")
    cat("\\multirow{2}{*}{Top 0.01\\% share}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_topshares[2, 2 + i], scientific=FALSE, digits=3), "\\%"))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares[2, 2 + i]/compare_topshares[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \n")

    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
}
print_table_us()

# ---------------------------------------------------------------------------- #
# Comparison of the methods on French data
# ---------------------------------------------------------------------------- #

file <- system.file("replication", "data-fr", "GGP2017DINAAppendixC.xlsx", package="gpinter")
col_names <- c("p", unlist(lapply(1970:2014, function(y) paste0(c("thr", "topavg", "b"), y))))
tab_full <- data.frame(read_excel(file, sheet="TC2", skip=13, col_names=col_names))
tab_full <- tab_full[1:127, ]

# Define the brackets for the short tabulation
tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 30, 50, 90, 99, 100),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

extra_thresholds <- data.frame()
extra_topshares <- data.frame()

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
    # Remove first row corresponding to the bracket [p=0, p=0.5]
    tab <- tab[-1, ]

    # Interpolate with the different methods
    p_out <- c(0.999, 0.9999)
    # Constant Pareto coefficient
    m1 <- method1(p_out, tab$p, tab$threshold, tab$m, average)
    # Piecewise Pareto with threshold information only
    m2 <- method2(p_out, tab$p, tab$threshold, average)

    # Generalized Pareto interpolation
    dist <- tabulation_fit(tab$p, tab$threshold, average, topshare=tab$topshare)
    m0 <- list(
        threshold = fitted_quantile(dist, p_out),
        topshare = top_share(dist, p_out)
    )

    # Get the true thresholds and shares
    actual <- list(
        threshold = c(
            tab_full[as.integer(1000*tab_full$p) == 100000*0.999, paste0("thr", year)],
            tab_full[as.integer(1000*tab_full$p) == 100000*0.9999, paste0("thr", year)]
        ),
        topshare = c(
            tab_full[as.integer(1000*tab_full$p) == 100000*0.999, paste0("topavg", year)]*(1 - 0.999)/average,
            tab_full[as.integer(1000*tab_full$p) == 100000*0.9999, paste0("topavg", year)]*(1 - 0.9999)/average
        )
    )

    extra_thresholds <- rbind(extra_thresholds,
        data.frame(year=year, method="m0",
            p999=m0$threshold[1]/average,
            p9999=m0$threshold[2]/average
        ),
        data.frame(year=year, method="m1",
            p999=m1$threshold[1]/average,
            p9999=m1$threshold[2]/average
        ),
        data.frame(year=year, method="m2",
            p999=m2$threshold[1]/average,
            p9999=m2$threshold[2]/average
        ),
        data.frame(year=year, method="actual",
            p999=actual$threshold[1]/average,
            p9999=actual$threshold[2]/average
        )
    )

    extra_topshares <- rbind(extra_topshares,
        data.frame(year=year, method="m0",
            p999=m0$topshare[1],
            p9999=m0$topshare[2]
        ),
        data.frame(year=year, method="m1",
            p999=m1$topshare[1],
            p9999=m1$topshare[2]
        ),
        data.frame(year=year, method="m2",
            p999=m2$topshare[1],
            p9999=m2$topshare[2]
        ),
        data.frame(year=year, method="actual",
            p999=actual$topshare[1],
            p9999=actual$topshare[2]
        )
    )
}

ggplot(extra_topshares) + geom_line(aes(x=year, y=p9999, color=method))
ggplot(extra_thresholds) + geom_line(aes(x=year, y=p999, color=method))

# Tables
compare_thresholds <- melt(extra_thresholds, id.vars=c("method", "year"))
compare_thresholds <- dcast(compare_thresholds, year + variable ~ method)

compare_topshares <- melt(extra_topshares, id.vars=c("method", "year"))
compare_topshares <- dcast(compare_topshares, year + variable ~ method)

compare_thresholds$m0 <- abs(compare_thresholds$m0 - compare_thresholds$actual)
compare_thresholds$m1 <- abs(compare_thresholds$m1 - compare_thresholds$actual)
compare_thresholds$m2 <- abs(compare_thresholds$m2 - compare_thresholds$actual)

compare_thresholds <- ddply(compare_thresholds, "variable", function(df) {
    return(colMeans(df[, c(paste0("m", 0:2))]))
})

compare_topshares$m0 <- abs(compare_topshares$m0 - compare_topshares$actual)
compare_topshares$m1 <- abs(compare_topshares$m1 - compare_topshares$actual)
compare_topshares$m2 <- abs(compare_topshares$m2 - compare_topshares$actual)

compare_topshares <- ddply(compare_topshares, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:2))]))
})

# Export in LaTeX
print_table_fr <- function() {
    cat("\\begin{tabular}{lccccc} \\toprule\n")
    cat("& ")
    cat(paste(sprintf("M%d", 0:2), collapse=" & "))
    cat("\\\\ \\midrule \\\\ \n")

    cat("\\multirow{2}{*}{P99.9/average}")
    for (i in 0:2) {
        cat(" & ")
        cat(format(compare_thresholds[1, 2 + i], scientific=FALSE, digits=3))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds[1, 2 + i]/compare_thresholds[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \n")

    cat("\\multirow{2}{*}{P99.99/average}")
    for (i in 0:2) {
        cat(" & ")
        cat(format(compare_thresholds[2, 2 + i], scientific=FALSE, digits=3))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds[2, 2 + i]/compare_thresholds[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \\midrule \\\\ \n")

    cat("\\multirow{2}{*}{Top 0.1\\% share}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_topshares[1, 2 + i], scientific=FALSE, digits=3), "\\%"))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares[1, 2 + i]/compare_topshares[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \n")
    cat("\\multirow{2}{*}{Top 0.01\\% share}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_topshares[2, 2 + i], scientific=FALSE, digits=3), "\\%"))
    }
    cat(" \\\\\n")
    cat("& \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares[2, 2 + i]/compare_topshares[2, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\\\ \n")

    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
}
print_table_fr()

# ---------------------------------------------------------------------------- #
# Comparison of the methods on US data for the year 2010
# ---------------------------------------------------------------------------- #

year <- 1970

# Import full tabulation from the excel file
file <- system.file("replication", "data-us", paste0("gpercptinc", year, "equal.xlsx"), package="gpinter")
tab_full <- data.frame(read_excel(file))
tab_full$topshare <- rev(cumsum(rev(tab_full$sh)))

# Overall average
average <- sum(diff(c(tab_full$gperc/100, 1))*tab_full$avg)

# Inverted Pareto coefficients
tab_full$b <- average*tab_full$topshare/(tab_full$thres*(1 - tab_full$gperc/100))

# Create a short tabulation with only five thresholds
tab_full$bracket <- cut(tab_full$gperc,
    breaks = c(0, 10, 50, 90, 99, 100),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)
tab <- ddply(tab_full, "bracket", function(df) {
    return(data.frame(
        p = min(df$gperc)/100,
        threshold = min(df$thres),
        sh = sum(df$sh)
    ))
})
# Truncated average
topsh <- 1 - cumsum(tab$sh)
# Remove first row corresponding to the bracket [p=0, p=0.1]
tab <- tab[-1, ]
tab$topsh <- topsh[1:4]
tab$m <- topsh[1:4]*average
tab$topavg <- tab$m/(1 - tab$p)

# Top of the tabulation only
tab_top <- tab_full[tab_full$gperc > 98 & tab_full$gperc <= 99.9, ]
p_out <- tab_top$gperc/100

# Actual values
b_actual <- data.frame(
    p = round(1000*p_out),
    b = tab_top$b
)

# Constant Pareto coefficient
b_cons <- data.frame(
    p = round(1000*seq(0.99, 0.999, 0.001)),
    b_cons = rep(tab_full$b[tab_full$gperc == 99], length(seq(0.99, 0.999, 0.001)))
)

# Generalized Pareto interpolation
dist <- tabulation_fit(tab$p, tab$threshold, average, bracketshare=tab$sh)
b_gpi1_inter <- data.frame(
    p = round(1000*seq(0.98, 0.99, 0.001)),
    b_gpi1_inter = invpareto(dist, seq(0.98, 0.99, 0.001))
)
b_gpi1_extra <- data.frame(
    p = round(1000*seq(0.99, 0.999, 0.001)),
    b_gpi1_extra = invpareto(dist, seq(0.99, 0.999, 0.001))
)

compare_us <- merge(b_actual, b_gpi1_inter, all=TRUE)
compare_us <- merge(compare_us, b_gpi1_extra, all=TRUE)
compare_us <- merge(compare_us, b_cons, all=TRUE)

ggplot(compare_us) +
    geom_line(aes(x=p/1000, y=b_gpi1_inter), na.rm=TRUE) +
    geom_line(aes(x=p/1000, y=b_gpi1_extra), linetype="dashed", na.rm=TRUE) +
    geom_line(aes(x=p/1000, y=b_cons), linetype="dashed", na.rm=TRUE) +
    geom_point(aes(x=p/1000, y=b), na.rm=TRUE) +
    geom_label(label="strict Pareto", x=0.975, y=2.68) +
    geom_label(label="generalized Pareto", x=0.968, y=3.1) +
    xlab("p") + ylab("b(p)") + scale_x_continuous(label=percent) +
    ggtitle(paste0("United States (", year, ")")) + theme(plot.title = element_text(hjust=0.5))
ggsave("~/Desktop/extrapol-us.pdf", height=5, width=5)

# ---------------------------------------------------------------------------- #
# Same thing for France (2010, still)
# ---------------------------------------------------------------------------- #

file <- system.file("replication", "data-fr", "GGP2017DINAAppendixC.xlsx", package="gpinter")
col_names <- c("p", unlist(lapply(1970:2014, function(y) paste0(c("thr", "topavg", "b"), y))))
tab_full <- data.frame(read_excel(file, sheet="TC2", skip=13, col_names=col_names))
tab_full <- tab_full[1:127, c("p", "b2010", "thr2010", "topavg2010")]
colnames(tab_full) <- c("p", "b", "thr", "topavg")

# Define the brackets for the short tabulation
tab_full$bracket <- cut(tab_full$p,
    breaks = c(0, 30, 50, 90, 95, 100),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

# Create a short tabulation with only four threhsolds
tab <- ddply(tab_full, "bracket", function(df) {
    return(data.frame(
        p = min(df$p)/100,
        threshold = min(df[, "thr"]),
        topavg = min(df[, "topavg"])
    ))
})
tab$topshare <- (1 - tab$p)*tab$topavg/average
tab$m <- tab$topshare*average

# Remove first row corresponding to the bracket [p=0, p=0.5]
tab <- tab[-1, ]

# Top of the tabulation only
tab_top <- tab_full[tab_full$p >= 90 & tab_full$p <= 99.9, ]
p_out <- tab_top$p/100

# Actual values
b_actual <- data.frame(
    p = as.integer(1000*p_out),
    b = tab_top$b
)

# Constant Pareto coefficient
b_cons <- data.frame(
    p = as.integer(1000*seq(0.95, 0.999, 0.001)),
    b_cons = rep(tab_full$b[tab_full$p == 95], length(seq(0.95, 0.999, 0.001)))
)

# Generalized Pareto interpolation
dist <- tabulation_fit(tab$p, tab$threshold, average, topavg=tab$topavg)
b_gpi1_inter <- data.frame(
    p = as.integer(1000*seq(0.9, 0.95, 0.001)),
    b_gpi1_inter = invpareto(dist, seq(0.9, 0.95, 0.001))
)
b_gpi1_extra <- data.frame(
    p = as.integer(1000*seq(0.95, 0.9999, 0.001)),
    b_gpi1_extra = invpareto(dist, seq(0.95, 0.999, 0.001))
)

compare_fr <- merge(b_actual, b_gpi1_inter, all=TRUE)
compare_fr <- merge(compare_fr, b_gpi1_extra, all=TRUE)
compare_fr <- merge(compare_fr, b_cons, all=TRUE)

ggplot(compare_fr) +
    geom_line(aes(x=p/1000, y=b_gpi1_inter), na.rm=TRUE) +
    geom_line(aes(x=p/1000, y=b_gpi1_extra), linetype="dashed", na.rm=TRUE) +
    geom_line(aes(x=p/1000, y=b_cons), linetype="dashed", na.rm=TRUE) +
    geom_point(aes(x=p/1000, y=b), na.rm=TRUE) +
    geom_label(label="strict Pareto", x=0.975, y=1.745) +
    geom_label(label="generalized Pareto", x=0.968, y=1.81) +
    xlab("p") + ylab("b(p)") + scale_x_continuous(label=percent) +
    ggtitle("France (2010)") + theme(plot.title = element_text(hjust=0.5))
ggsave("~/Desktop/extrapol-fr.pdf", height=5, width=5)


