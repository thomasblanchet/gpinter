library(shiny)
library(shinyBS)
library(shinyjs)
library(evaluate)
library(xlsx)
library(readxl)
library(gpinter)

# Increased max upload size to 50MB
options(shiny.maxRequestSize=50*1024^2)

source(file.path("ui", "radiobutton-html.R"), local=TRUE)

source(file.path("server", "parse-input.R"), local=TRUE)
source(file.path("server", "plot-text.R"), local=TRUE)

server <- function(input, output, session) {
    # g-percentiles: fractiles to show to the user
    gperc <- c(
        seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
        seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
    )

    # Reactive values for the input data and the results
    data <- reactiveValues(
        data               = NULL,
        nb_data            = NULL,
        errors             = NULL,
        years              = NULL,
        countries          = NULL,
        components         = NULL,
        results            = NULL,
        tables             = NULL,
        results_years      = NULL,
        results_countries  = NULL,
        results_components = NULL
    )

    source(file.path("server", "clear-all.R"), local=TRUE)$value

    # Clear all and go back to home when clicking title
    observeEvent(input$main_logo, {
        updateNavbarPage(session, "main_navbar", selected="Input data")
        clear_all()
    })

    source(file.path("server", "import-data.R"), local=TRUE)$value
    source(file.path("server", "display-input-data.R"), local=TRUE)$value
    source(file.path("server", "run-program.R"), local=TRUE)$value
    source(file.path("server", "render-output-tables.R"), local=TRUE)$value
    source(file.path("server", "render-plot.R"), local=TRUE)$value
    source(file.path("server", "download-sample.R"), local=TRUE)$value
}

ui <- tagList(
    useShinyjs(),
    navbarPage(
        title = actionLink("main_logo", tagList(
            tags$p("WID.WORLD"),
            tags$p("generalized Pareto interpolation")
        )),
        source(file.path("ui", "tab-input-data.R"), local=TRUE)$value,
        source(file.path("ui", "tab-output-tables.R"), local=TRUE)$value,
        source(file.path("ui", "tab-plots.R"), local=TRUE)$value,
        source(file.path("ui", "tab-sample.R"), local=TRUE)$value,
        source(file.path("ui", "tab-diagnostic.R"), local=TRUE)$value,
        source(file.path("ui", "tab-settings.R"), local=TRUE)$value,
        navbarMenu("Help",
            source(file.path("ui", "tab-usage.R"), local=TRUE)$value,
            source(file.path("ui", "tab-about.R"), local=TRUE)$value,
            icon = icon("question-circle")
        ),
        id = "main_navbar",
        selected = "Input data",
        inverse = TRUE,
        theme = "style.css",
        windowTitle = "Generalized Pareto interpolation - WID.world"
    )
)

shinyApp(ui=ui, server=server)
