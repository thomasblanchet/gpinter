plot_text <- function(text) {
    plot(c(0, 1), c(0, 1), ann=FALSE, bty='n', type='n', xaxt='n', yaxt='n')
    text(
        x = 0.5,
        y = 0.5,
        text,
        cex = 1.6,
        col = "darkgrey"
    )
}
