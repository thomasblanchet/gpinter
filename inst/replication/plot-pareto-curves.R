# ---------------------------------------------------------------------------- #
# Code for the replication of the figures showing empirical Pareto curves
# in Blanchet, Fournier & Piketty (2017).
# ---------------------------------------------------------------------------- #

library(readxl)
library(ggplot2)
library(extrafont)
library(reshape2)
library(plyr)

# US ------------------------------------------------------------------------- #

file <- system.file("replication", "data-us", "US_income.csv", package="gpinter")
tab_us <- read.csv(file, sep=";")
tab_us <- tab_us[tab_us$p != "pall", ]
tab_us$p <- as.numeric(substring(tab_us$p2, 2))/100
tab_us <- ddply(tab_us, c("alpha2", "year"), function(df) {
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

# France, 2010 --------------------------------------------------------------- #

file <- system.file("replication", "data-fr", "GGP2017DINAAppendixC.xlsx", package="gpinter")
col_names <- c("p", unlist(lapply(1970:2014, function(y) paste0(c("thr", "topavg", "b"), y))))
tab_fr <- data.frame(read_excel(file, sheet="TC11", skip=13, col_names=col_names))
tab_fr <- tab_fr[1:127, ]

tab_both <- data.frame(
    p = tab_us$p,
    "United States" = tab_us[tab_us$year == 2012, "bptinc992j"],
    "France" = tab_fr$b2012,
    check.names = FALSE
)
tab_both <- tab_both[tab_both$p >= 0.3 & tab_both$p < 0.999, ]

tab_both <- melt(tab_both, id.vars="p")

pdf("~/Desktop/gpc-us-fr-2012.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(tab_both) + geom_line(aes(x=p, y=value, linetype=variable)) + ggtitle("Year 2012") +
    scale_x_continuous(limits = c(0.3, 1), breaks=seq(0.3, 1, 0.1)) +
    scale_linetype_manual(values=c("United States"="solid", "France"="longdash")) +
    xlab(expression(paste("rank ", italic(p)))) +
    ylab(expression(paste("inverted Pareto coefficient ", italic(b), "(", italic(p), ")"))) +
    theme_bw() + theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.background = element_rect(linetype="solid", color="black", size=0.25),
        legend.title = element_blank(), legend.box.margin = margin(10, 10, 10, 10),
        legend.direction = "horizontal", plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) + ylim(c(1.5, 4.5))
dev.off()
embed_fonts(path.expand("~/Desktop/gpc-us-fr-2012.pdf"))

tab_both <- data.frame(
    p = tab_us$p,
    "United States" = tab_us[tab_us$year == 1980, "bptinc992j"],
    "France" = tab_fr$b1980,
    check.names = FALSE
)
tab_both <- tab_both[tab_both$p >= 0.3 & tab_both$p < 0.999, ]

tab_both <- melt(tab_both, id.vars="p")

pdf("~/Desktop/gpc-us-fr-1980.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(tab_both) + geom_line(aes(x=p, y=value, linetype=variable)) + ggtitle("Year 1980") +
    scale_x_continuous(limits = c(0.3, 1), breaks=seq(0.3, 1, 0.1)) +
    scale_linetype_manual(values=c("United States"="solid", "France"="longdash")) +
    xlab(expression(paste("rank ", italic(p)))) +
    ylab(expression(paste("inverted Pareto coefficient ", italic(b), "(", italic(p), ")"))) +
    theme_bw() + theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.background = element_rect(linetype="solid", color="black", size=0.3),
        legend.title = element_blank(), legend.box.margin = margin(10, 10, 10, 10),
        legend.direction = "horizontal", plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) + ylim(c(1.5, 4.5))
dev.off()
embed_fonts(path.expand("~/Desktop/gpc-us-fr-1980.pdf"))
