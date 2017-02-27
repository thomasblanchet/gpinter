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

ggplot(df) + geom_line(aes(x=x, y=ey)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=0.115, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=0.115, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=0.115, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=0.115, angle=90) +
    xlab("x = -log(1 - p)") + ylab(expression(paste("multiple of ||", phi1, "'''||"[infinity]))) +
    ggtitle(expression(paste("Error bound on ", phi1(x))), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
ggsave("~/Desktop/err-phi.pdf", height=5, width=5)

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

df_ey <- data.frame(
    x = x,
    "minimum" = abs(interpolation_value_error(x, xk, deriv3_steps=steps, deriv3_values=values_min)),
    "maximum" = abs(interpolation_value_error(x, xk, deriv3_steps=steps, deriv3_values=values_max)),
    "median"  = abs(interpolation_value_error(x, xk, deriv3_steps=steps, deriv3_values=values_med)),
    check.names = FALSE
)

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
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=0.01, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=0.01, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=0.01, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=0.01, angle=90) +
    xlab("x = -log(1 - p)") + ylab("absolute value of the error") +
    ggtitle(expression(paste("Error on ", phi1(x))), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        legend.position="bottom", legend.title=element_text("variant"))

ggplot(df) +
    geom_line(aes(x=x, y=edy_min), linetype="dashed") +
    geom_line(aes(x=x, y=edy_max), linetype="dashed") +
    geom_line(aes(x=x, y=edy_med)) +
    geom_vline(xintercept=xk, linetype="dashed") +
    annotate("text", label="p = 10%", x=xk[1] + 0.1, y=0.019, angle=90) +
    annotate("text", label="p = 50%", x=xk[2] + 0.1, y=0.019, angle=90) +
    annotate("text", label="p = 90%", x=xk[3] + 0.1, y=0.019, angle=90) +
    annotate("text", label="p = 99%", x=xk[4] - 0.13, y=0.019, angle=90) +
    xlab("x = -log(1 - p)") + ylab(expression(paste("multiple of ||", phi1, "'''||"[infinity]))) +
    ggtitle(expression(paste("Error on ", phi1(x))), subtitle="for a tabulation with p = 10%, 50%, 90% and 99%") +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))



