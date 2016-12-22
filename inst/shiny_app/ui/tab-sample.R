tabPanel("Sample",
    fixedPage(
        fixedRow(
            column(4,
                tags$form(
                    disabled(selectInput("synthpop_year", "Year", choices=NULL, width="100%")),
                    disabled(checkboxInput("synthpop_year_all", "Use all years", value=TRUE, width="100%")),
                    class = "select-synthpop"
                )
            ),
            column(4,
                tags$form(
                    disabled(selectInput("synthpop_country", "Country", choices=NULL, width="100%")),
                    disabled(checkboxInput("synthpop_country_all", "Use all countries", value=TRUE, width="100%")),
                    class = "select-synthpop"
                )
            ),
            column(4,
                tags$form(
                    disabled(selectInput("synthpop_component", "Component", choices=NULL, width="100%")),
                    disabled(checkboxInput("synthpop_component_all", "Use all components", value=TRUE, width="100%")),
                    class = "select-synthpop"
                )
            )
        ),
        numericInput("synthpop_size", "Sample size",
            value = 1e3,
            min = 1,
            width = "100%"
        ),
        hr(),
        fixedRow(
            column(6,
                downloadButton("synthpop_dl_csv", "Download as CSV",
                    class = "btn-block btn-primary disabled"
                )
            ),
            column(6,
                downloadButton("synthpop_dl_excel", "Download as Excel",
                    class = "btn-block btn-primary disabled"
                )
            )
        )
    ),
    icon = icon("random")
)
