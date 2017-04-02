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
library(extrafont)
loadfonts()

# ---------------------------------------------------------------------------- #
# Comparison of the methods on US data
# ---------------------------------------------------------------------------- #

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
    breaks = c(0, 0.30, 0.50, 0.80, 0.90, 1),
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
)

extra_thresholds <- data.frame()
extra_topshares <- data.frame()

d_ply(tab_full, c("alpha2", "year"), function(df) {
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
    p_out <- 0.99
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

    # Generate the plot
    # p_plot <- seq(0.9, 0.9999, 0.000001)
    # df_points <- data.frame(p=df$p, b=df$bptinc992j)
    # df_points$type <- "excluded"
    # df_points[df_points$p == 0.9 | df_points$p == 0.99, "type"] <- "included"
    # df_points <- df_points[df_points$p >= 0.9 & df_points$p < 0.9991, ]
    # df_line <- data.frame(p=p_plot, b=invpareto(dist, p_plot))
    # df_line[df_line$p <= 0.99, "type"] <- "interpolation"
    # df_line[df_line$p > 0.99, "type"] <- "extrapolation"
    # pdf(paste0("~/Desktop/extrapol/extrapol-us-", year, ".pdf"), family="CM Roman", width=4.5, height=3.5)
    # print(ggplot() +
    #     geom_point(data=df_points, aes(x=p, y=b, shape=type)) +
    #     geom_line(data=df_line, aes(x=p, y=b, linetype=type)) +
    #     scale_linetype_manual("estimation", values=c("interpolation"="solid", "extrapolation"="dashed")) +
    #     scale_shape_manual("data", values=c("included"=19, "excluded"=1)) +
    #     xlab(expression(paste("rank ", italic(p)))) +
    #     ylab(expression(paste("inverted Pareto coefficient ", italic(b), "(", italic(p), ")"))) +
    #     scale_x_continuous(breaks=seq(0.9, 1, 0.02)) +
    #     guides(linetype = guide_legend(title.position = "top", order=1),
    #         shape = guide_legend(title.position = "top", order=2)) +
    #     theme_bw() + theme(legend.justification = c(0, 1), legend.position = c(0, 1),
    #         legend.background = element_rect(linetype="solid", color="black", size=0.25),
    #         legend.box.margin = margin(10, 10, 10, 10),
    #         legend.direction = "horizontal", plot.title=element_text(hjust=0.5),
    #         plot.subtitle=element_text(hjust=0.5)) + ggtitle(paste0("United States, ", year)))
    # dev.off()
    # embed_fonts(path.expand(paste0("~/Desktop/extrapol/extrapol-us-", year, ".pdf")))

    # Get the true thresholds and shares
    actual <- list(
        threshold = c(
            df[round(100000*df$p) == 100000*p_out, "tptinc992j"]
        ),
        topshare = c(
            df[round(100000*df$p) == 100000*p_out, "topshare"]
        )
    )

    extra_thresholds <<- rbind(extra_thresholds,
        data.frame(year=year, method="m0",
            p999=m0$threshold[1]/average
        ),
        data.frame(year=year, method="m1",
            p999=m1$threshold[1]/average
        ),
        data.frame(year=year, method="m2",
            p999=m2$threshold[1]/average
        ),
        data.frame(year=year, method="actual",
            p999=actual$threshold[1]/average
        )
    )

    extra_topshares <<- rbind(extra_topshares,
        data.frame(year=year, method="m0",
            p999=m0$topshare[1]
        ),
        data.frame(year=year, method="m1",
            p999=m1$topshare[1]
        ),
        data.frame(year=year, method="m2",
            p999=m2$topshare[1]
        ),
        data.frame(year=year, method="actual",
            p999=actual$topshare[1]
        )
    )
})

# Tables
compare_thresholds <- melt(extra_thresholds, id.vars=c("method", "year"))
compare_thresholds <- dcast(compare_thresholds, year + variable ~ method)

compare_topshares <- melt(extra_topshares, id.vars=c("method", "year"))
compare_topshares <- dcast(compare_topshares, year + variable ~ method)

compare_thresholds$m0 <- abs((compare_thresholds$m0 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m1 <- abs((compare_thresholds$m1 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m2 <- abs((compare_thresholds$m2 - compare_thresholds$actual)/compare_thresholds$actual)

compare_thresholds_us <- ddply(compare_thresholds, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:2))]))
})

compare_topshares$m0 <- abs((compare_topshares$m0 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m1 <- abs((compare_topshares$m1 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m2 <- abs((compare_topshares$m2 - compare_topshares$actual)/compare_topshares$actual)

compare_topshares_us <- ddply(compare_topshares, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:2))]))
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
    breaks = c(0, 30, 50, 80, 90, 100),
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
    p_out <- 0.99
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

    # Generate the plot
    # p_plot <- seq(0.9, 0.9999, 0.000001)
    # df_points <- data.frame(p=tab_full$p/100, b=tab_full[, paste0("b", year)])
    # df_points$type <- "excluded"
    # df_points[df_points$p == 0.9 | df_points$p == 0.99, "type"] <- "included"
    # df_points <- df_points[df_points$p >= 0.9 & df_points$p < 0.9991, ]
    # df_line <- data.frame(p=p_plot, b=invpareto(dist, p_plot))
    # df_line[df_line$p <= 0.99, "type"] <- "interpolation"
    # df_line[df_line$p > 0.99, "type"] <- "extrapolation"
    # pdf(paste0("~/Desktop/extrapol/extrapol-fr-", year, ".pdf"), family="CM Roman", width=4.5, height=3.5)
    # print(ggplot() +
    #     geom_point(data=df_points, aes(x=p, y=b, shape=type)) +
    #     geom_line(data=df_line, aes(x=p, y=b, linetype=type)) +
    #     scale_linetype_manual("estimation", values=c("interpolation"="solid", "extrapolation"="dashed")) +
    #     scale_shape_manual("data", values=c("included"=19, "excluded"=1)) +
    #     xlab(expression(paste("rank ", italic(p)))) +
    #     ylab(expression(paste("inverted Pareto coefficient ", italic(b), "(", italic(p), ")"))) +
    #     scale_x_continuous(breaks=seq(0.9, 1, 0.02)) +
    #     guides(linetype = guide_legend(title.position = "top", order=1),
    #         shape = guide_legend(title.position = "top", order=2)) +
    #     theme_bw() + theme(legend.justification = c(0, 1), legend.position = c(0, 1),
    #         legend.background = element_rect(linetype="solid", color="black", size=0.25),
    #         legend.box.margin = margin(10, 10, 10, 10),
    #         legend.direction = "horizontal", plot.title=element_text(hjust=0.5),
    #         plot.subtitle=element_text(hjust=0.5)) + ggtitle(paste0("France, ", year)))
    # dev.off()
    # embed_fonts(path.expand(paste0("~/Desktop/extrapol/extrapol-fr-", year, ".pdf")))

    # Get the true thresholds and shares
    actual <- list(
        threshold = c(
            tab_full[as.integer(1000*tab_full$p) == 100000*p_out, paste0("thr", year)]
        ),
        topshare = c(
            tab_full[as.integer(1000*tab_full$p) == 100000*p_out, paste0("topavg", year)]*(1 - p_out)/average
        )
    )

    extra_thresholds <- rbind(extra_thresholds,
        data.frame(year=year, method="m0",
            p999=m0$threshold[1]/average
        ),
        data.frame(year=year, method="m1",
            p999=m1$threshold[1]/average
        ),
        data.frame(year=year, method="m2",
            p999=m2$threshold[1]/average
        ),
        data.frame(year=year, method="actual",
            p999=actual$threshold[1]/average
        )
    )

    extra_topshares <- rbind(extra_topshares,
        data.frame(year=year, method="m0",
            p999=m0$topshare[1]
        ),
        data.frame(year=year, method="m1",
            p999=m1$topshare[1]
        ),
        data.frame(year=year, method="m2",
            p999=m2$topshare[1]
        ),
        data.frame(year=year, method="actual",
            p999=actual$topshare[1]
        )
    )
}

# Tables
compare_thresholds <- melt(extra_thresholds, id.vars=c("method", "year"))
compare_thresholds <- dcast(compare_thresholds, year + variable ~ method)

compare_topshares <- melt(extra_topshares, id.vars=c("method", "year"))
compare_topshares <- dcast(compare_topshares, year + variable ~ method)

compare_thresholds$m0 <- abs((compare_thresholds$m0 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m1 <- abs((compare_thresholds$m1 - compare_thresholds$actual)/compare_thresholds$actual)
compare_thresholds$m2 <- abs((compare_thresholds$m2 - compare_thresholds$actual)/compare_thresholds$actual)

compare_thresholds_fr <- ddply(compare_thresholds, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:2))]))
})

compare_topshares$m0 <- abs((compare_topshares$m0 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m1 <- abs((compare_topshares$m1 - compare_topshares$actual)/compare_topshares$actual)
compare_topshares$m2 <- abs((compare_topshares$m2 - compare_topshares$actual)/compare_topshares$actual)

compare_topshares_fr <- ddply(compare_topshares, "variable", function(df) {
    return(100*colMeans(df[, c(paste0("m", 0:2))]))
})

# Export in LaTeX
print_table <- function() {
    cat("\\begin{tabular}{p{3cm}cP@{}P@{}P@{}} \\toprule\n")
    cat("& & ")
    cat(paste(sprintf("M%d", 0:2), collapse=" & "))

    cat(" \\\\ \\midrule \n")

    cat("\\multirow{4}{3cm}{\\centering United States \\ (1962--2014)} & ")

    cat("\\multirow{2}{*}{Top 0.1\\% share}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_topshares_us[1, 2 + i], scientific=FALSE, digits=2), "\\%"))
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_us[1, 2 + i]/compare_topshares_us[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }
    cat(" \\\\ \\cmidrule(l){2-5} \n")

    cat("& \\multirow{2}{*}{P99.9/average}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_thresholds_us[1, 2 + i], scientific=FALSE, digits=2), "\\%"))
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_us[1, 2 + i]/compare_thresholds_us[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\midrule \n")

    cat("\\multirow{4}{3cm}{\\centering France \\ (1970--2012)} & ")

    cat("\\multirow{2}{*}{Top 0.1\\% share}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_topshares_fr[1, 2 + i], scientific=FALSE, digits=2), "\\%"))
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_topshares_fr[1, 2 + i]/compare_topshares_fr[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat(" \\\\ \\cmidrule(l){2-5} \n")

    cat("& \\multirow{2}{*}{P99.9/average}")
    for (i in 0:2) {
        cat(" & ")
        cat(paste0(format(compare_thresholds_fr[1, 2 + i], scientific=FALSE, digits=2), "\\%"))
    }
    cat(" \\\\\n")
    cat("& & \\footnotesize (ref.)")
    for (i in 1:2) {
        cat(" & \\footnotesize ($\\times ")
        cat(format(compare_thresholds_fr[1, 2 + i]/compare_thresholds_fr[1, 2], scientific=FALSE, digits=2))
        cat("$)")
    }

    cat("\\\\ \\bottomrule\n")
    cat("\\end{tabular}\n")
}
print_table()
