#' @title Run the Shiny application in the web browser
#'
#' @author Thomas Blanchet, Juliette Fournier, Thomas Piketty
#'
#' @description Run the Shiny app of wid.world/gpinter locally.
#'
#' @export
#' @importFrom shiny runApp

run_app <- function() {
    app_dir <- system.file("shiny_app", package="gpinter")
    runApp(app_dir)
}
