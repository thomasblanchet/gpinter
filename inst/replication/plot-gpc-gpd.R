library(ggplot2)
library(extrafont)

loadfonts()

p <- seq(0, 1, 0.001)

mu <- 1
xi <- 1/2
sigma <- seq(0.1, 0.9, 0.2)

df <- expand.grid(list(p=p, mu=mu, xi=xi, sigma=sigma))

df$b <- 1 + df$xi*df$sigma/((1 - df$xi)*(df$sigma + (1 - df$p)^df$xi*(df$mu*df$xi - df$sigma)))

pdf("~/Desktop/gpc-gpd-sigma.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(df) + geom_line(aes(x=p, y=b, colour=as.factor(sigma))) +
    xlab(expression(paste("rank ", italic(p)))) +
    ylab(expression(paste("inverted Pareto coefficient ", italic(b), "(", italic(p), ")"))) +
    scale_x_continuous(breaks=seq(0, 1, 0.2)) +
    scale_colour_grey(start=0.8, end=0.2) +
    ggtitle(expression(paste(mu==1, ", ", xi==1/2))) + theme_bw() +
    theme(legend.position = "none", plot.title=element_text(hjust=0.5)) +
    annotate("text", label="sigma==0.1", x=0.1, y=1.28, parse=TRUE) +
    annotate("text", label="sigma==0.3", x=0.1, y=1.68, parse=TRUE) +
    annotate("text", label="sigma==0.5", x=0.1, y=2.07, parse=TRUE) +
    annotate("text", label="sigma==0.7", x=0.1, y=2.45, parse=TRUE) +
    annotate("text", label="sigma==0.9", x=0.3, y=2.70, parse=TRUE)
dev.off()
embed_fonts(path.expand("~/Desktop/gpc-gpd-sigma.pdf"))

p <- seq(0, 1, 0.001)

mu <- 1
xi <- c(0.2, 0.5, 0.6, 0.7)
sigma <- 1/2

df <- expand.grid(list(p=p, mu=mu, xi=xi, sigma=sigma))

df$b <- 1 + df$xi*df$sigma/((1 - df$xi)*(df$sigma + (1 - df$p)^df$xi*(df$mu*df$xi - df$sigma)))

pdf("~/Desktop/gpc-gpd-xi.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(df) + geom_line(aes(x=p, y=b, colour=as.factor(xi))) +
    xlab(expression(paste("rank ", italic(p)))) +
    ylab(expression(paste("inverted Pareto coefficient ", italic(b), "(", italic(p), ")"))) +
    scale_x_continuous(breaks=seq(0, 1, 0.2)) +
    scale_colour_grey(expression(xi), start=0.8, end=0.2) +
    ggtitle(expression(paste(mu==1, ", ", sigma==1/2))) + theme_bw() +
    theme(legend.position = "none", plot.title=element_text(hjust=0.5)) +
    annotate("text", label="xi==0.2", x=0.1, y=1.5, parse=TRUE) +
    annotate("text", label="xi==0.5", x=0.1, y=1.9, parse=TRUE) +
    annotate("text", label="xi==0.6", x=0.1, y=2.35, parse=TRUE) +
    annotate("text", label="xi==0.7", x=0.1, y=2.8, parse=TRUE)
dev.off()
embed_fonts(path.expand("~/Desktop/gpc-gpd-xi.pdf"))
