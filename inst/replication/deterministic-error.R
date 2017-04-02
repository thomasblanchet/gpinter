# ---------------------------------------------------------------------------- #
# Code for the replication of the figures showing the deterministic error
# in Blanchet, Fournier & Piketty (2017).
# ---------------------------------------------------------------------------- #

library(ggplot2)
library(reshape2)

pk <- c(0.1, 0.5, 0.9, 0.99)
xk <- -log(1 - pk)

x <- seq(min(xk), max(xk), length.out=1e3)

df <- data.frame(
    x = x,
    ey = interpolation_value_error_bound(x, xk, 1),
    edy = interpolation_deriv_error_bound(x, xk, 1)
)

pdf("~/Desktop/err-phi.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(df) + geom_line(aes(x=x, y=ey)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="paste(italic(p) == 10, '%')", x=xk[1] + 0.16, y=0.125, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 50, '%')", x=xk[2] + 0.16, y=0.125, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=xk[3] + 0.16, y=0.125, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=xk[4] - 0.16, y=0.125, angle=90, parse=TRUE) +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) + ylab(expression(paste("multiple of ||", phi1, "'''||"[infinity]))) +
    ggtitle(expression(paste("Error bound on ", phi1, "(", italic(x), ")")),
        subtitle=expression("for a tabulation with"~italic(p)~"= 10%, 50%, 90% and 99%")) + theme_bw() +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-phi.pdf"))

pdf("~/Desktop/err-dphi.pdf", family="CM Roman", width=4.5, height=3.5)
ggplot(df) + geom_line(aes(x=x, y=edy)) +
    geom_vline(xintercept=xk, linetype="dashed") + ylim(c(0, 0.25)) +
    annotate("text", label="paste(italic(p) == 10, '%')", x=xk[1] + 0.16, y=0.22, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 50, '%')", x=xk[2] + 0.16, y=0.22, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 90, '%')", x=xk[3] + 0.16, y=0.22, angle=90, parse=TRUE) +
    annotate("text", label="paste(italic(p) == 99, '%')", x=xk[4] - 0.16, y=0.22, angle=90, parse=TRUE) +
    xlab(expression(paste(italic(x) == -log, "(", 1 - italic(p), ")"))) + ylab(expression(paste("multiple of ||", phi1, "'''||"[infinity]))) +
    ggtitle(expression(paste("Error bound on ", phi1, "'(", italic(x), ")")),
        subtitle=expression("for a tabulation with"~italic(p)~"= 10%, 50%, 90% and 99%")) + theme_bw() +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
dev.off()
embed_fonts(path.expand("~/Desktop/err-dphi.pdf"))

ggplot(df) + geom_line(aes(x=x, y=edy)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=0.175, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=0.175, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=0.175, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=0.175, angle=90) +
    xlab("x = -log(1 - p)") + ylab(expression(paste("multiple of ||", phi1, "'''||"[infinity]))) +
    ggtitle(expression(paste("Error bound on ", phi1, "'"(x))), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
ggsave("~/Desktop/err-dphi.pdf", height=5, width=5)

steps <- -log(1 - c(0.1, 0.2, 0.5, 0.9, 0.99))
values_min <- c(0.771, 0.549, 0.464, 0.050)
values_med <- c(2.106, 1.018, 0.529, 0.063)
values_max <- c(3.247, 1.452, 0.627, 0.091)

interpolation_value_error(-log(1 - 0.9), xk, deriv3_steps=steps, deriv3_values=values_med)

df_ey <- data.frame(
    x = x,
    "minimum" = abs(interpolation_value_error(x, xk, deriv3_steps=steps, deriv3_values=values_min)),
    "maximum" = abs(interpolation_value_error(x, xk, deriv3_steps=steps, deriv3_values=values_max)),
    "median"  = abs(interpolation_value_error(x, xk, deriv3_steps=steps, deriv3_values=values_med)),
    check.names = FALSE
)

u <- seq(-log(1 - 0.1), -log(1 - 0.99), length.out = 1000)
x <- -log(1 - 0.9)

u <- -log(1 - df$p)

err0 <- interpolation_value_error(u, xk, function(t) 1)
err1 <- interpolation_value_error(u, xk, function(t) t)
err2 <- interpolation_value_error(u, xk, function(t) t^2)
err3 <- interpolation_value_error(u, xk, function(t) t^3)

err_real <- phi_actual - phi(dist, u)

fit <- lm(err_real ~ 0 + err0 + err1 + err2 + err3)
summary(fit)

y <- coef(fit)["err0"] + u*coef(fit)["err1"] + u^2*coef(fit)["err2"] + u^3*coef(fit)["err3"]

derr <- interpolation_deriv_error(u, xk, dphi3)


err <- interpolation_value_error(u, xk, phid3)

lines(u, abs(err), col='red')

plot(u, abs(err), type='l')
plot(u, abs(derr), type='l')

plot(u, kernel(u, x), type='l')
plot(u, deriv_kernel(u, x), type='l')

#plot(u, kernel_value_error(xk, x, u), type='l')

param <- kernel_interpolation_param(xk, x)

plot(u, quintic_spline(u, param$xk, param$yk, param$sk, param$ak), type='l')
lines(u, kernel(x, u), type='l', col="red")

plot(u, quintic_spline(u, param$xk, param$yk, param$sk, param$ak) - kernel(u, x), type='l')
plot(u, kernel_value_error(xk, x, u), type='l')

plot(u, deriv_quintic_spline(u, param$xk, param$yk, param$sk, param$ak), type='l')
lines(u, deriv_kernel(x, u), type='l', col="red")

plot(u, deriv_quintic_spline(u, param$xk, param$yk, param$sk, param$ak) - deriv_kernel(u, x), type='l')
plot(u, kernel_deriv_error(xk, x, u), type='l')

df_edy <- data.frame(
    x = x,
    "minimum" = abs(interpolation_deriv_error(x, xk, deriv3_steps=steps, deriv3_values=values_min)),
    "maximum" = abs(interpolation_deriv_error(x, xk, deriv3_steps=steps, deriv3_values=values_max)),
    "median"  = abs(interpolation_deriv_error(x, xk, deriv3_steps=steps, deriv3_values=values_med)),
    check.names = FALSE
)

df_ey <- melt(df_ey, id.vars="x")
df_edy <- melt(df_edy, id.vars="x")

ggplot(df_ey) +
    geom_line(aes(x=x, y=value, color=variable, linetype=variable)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=0.013, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=0.013, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=0.013, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=0.013, angle=90) +
    xlab("x = -log(1 - p)") + ylab("absolute value of the error") +
    ggtitle(expression(paste("Error on ", phi1(x))), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        legend.position="bottom") + scale_color_discrete(name="variant") +
    scale_linetype_discrete(name="variant")
ggsave("~/Desktop/err-phi-variants.pdf", height=5, width=5)

ggplot(df_edy) +
    geom_line(aes(x=x, y=value, color=variable, linetype=variable)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=0.0019, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=0.0019, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=0.0019, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=0.0019, angle=90) +
    xlab("x = -log(1 - p)") + ylab("absolute value of the error") +
    ggtitle(expression(paste("Error on ", phi1, "'"(x))), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        legend.position="bottom") + scale_color_discrete(name="variant") +
    scale_linetype_discrete(name="variant")
ggsave("~/Desktop/err-dphi-variants.pdf", height=5, width=5)
